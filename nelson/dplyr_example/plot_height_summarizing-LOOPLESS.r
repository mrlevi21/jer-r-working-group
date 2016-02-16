#Loads up the functions in dplyr. If you don't have dplyr installed, run install.packages(dplyr) first
require(dplyr)

#Just specifying the filenames. These are stored as strings so I can refer to them multiple times and if the name of the file I'm working with
#changes, I just change the value here and don't have to worry about replacing it anywhere else
input.file <- "MCFO_raw_records"
usda.species.file <- "mt_usdaspecies"

#Importing the .csv flies I named above. I specified that I didn't want strings as factors because I was worried it'd break something
#but it's probably unnecessary. The paste() is grabbing the filename I provided above, sticking .csv on the end and making it a string
#because read.csv() needs a string with the filename plus the file extension to know what it's looking for
raw.data <- read.csv(paste(input.file,".csv",sep=""), stringsAsFactors = F)
usda.species.data <- read.csv(paste(usda.species.file,".csv",sep=""), stringsAsFactors = F)

#This is just sad, but the heights were in inches AND importing as character strings instead of numbers so this converts to numerical values in cm
raw.data$Height <- as.numeric(raw.data$Height.IN)*2.54

#I differentiated the growth habits further here than necessary in case one day we want to analyze in a finer-grained way
#This takes the species list and makes a subset where in the Growth.Habit column the function grepl() returns TRUE because it's found the
#string it's looking for (e.g. "raminoid" or "ree"). The patterns to find cut off the first letter only because I didn't want to deal with
#capitalization issues.
#The %>% means "take the previous object or result and pass it on to the next function as the first argument" so this could also have been written as
#subset(usda.species.data, grepl("raminoid", Growth.Habit))
graminoids <- usda.species.data %>% subset(grepl("raminoid", Growth.Habit))
forbs <- usda.species.data %>% subset(grepl("orb", Growth.Habit))
trees <- shrubs <- usda.species.data %>% subset(grepl("ree", Growth.Habit))
shrubs <- usda.species.data %>% subset(grepl("hrub", Growth.Habit))
#Because looking for "hrub" will also find plants with growth habits like "Subshrub, forb", this makes another data frame for only the shrubs and subshrubs that
#that don't also occur in my forb list. Technically it's looking for all the ones that ARE in the forb list, but the ! inverts it to make it AREN'T in
shrubs.sans.forbs <- shrubs[!(shrubs$Accepted.Symbol %in% forbs$Accepted.Symbol),]

#There were some PF01 and SH01 values in here, so I manually set them to the correct growth habit because that was easiest. The automated part for
#entries with actual species codes is next. I also fixed some " GUSA2" entries to set them to "GUSA2"

#So, in the brackets, it's looking for all the values in raw.data's Species column that are in the Accepted.Symbol column. It'll give me a vector
#where every entry is TRUE or FALSE. Because it's in [] there, it means that it treats that like a list of index values to look at or not look at in
#raw.data$Growth.Habit and if it happens to be looking at it, it writes in the string there at the end of the line.
raw.data$Growth.Habit[raw.data$Species %in% forbs$Accepted.Symbol] <- "Herbaceous"
raw.data$Growth.Habit[raw.data$Species %in% graminoids$Accepted.Symbol] <- "Herbaceous"
raw.data$Growth.Habit[raw.data$Species %in% trees$Accepted.Symbol] <- "Woody"
raw.data$Growth.Habit[raw.data$Species %in% shrubs.sans.forbs$Accepted.Symbol] <- "Woody"

#Group it up! This is so we can summarize down to the point because there are multiple entries per point and we only want the tallest
data.by.point <- group_by(raw.data, Project, Plot, Line, Point)

#This is inefficient, but at least makes sense I hope. This takes all the point data where the Method column has the value "HAF" and the Growth.Habit
#column has "Herbaceous" and then finds the largest height value recorded for each point, putting all the results into summary.intermediate.haf.herbaceous
summary.intermediate.haf.herbaceous <- subset(data.by.point, Method == "HAF") %>% subset(Growth.Habit == "Herbaceous") %>% summarize(
 HAFHeightHerbaceous = max(na.omit(Height))
)

summary.intermediate.haf.woody <- subset(data.by.point, Method == "HAF") %>% subset(Growth.Habit == "Woody") %>% summarize(
  HAFHeightWoody = max(na.omit(Height))
)

#So, the herbaceous and woody summaries are separate currently, but we want them in one data frame. This uses merge() to combine the two so that any time
#they agree on Project, Plot, Line, and Point the values from each go into a single line. The all = T says that even if there isn't a value for a point in
#both data frames, it'll still keep the values for the one that does exist.
summary.intermediate.haf <- merge(summary.intermediate.haf.herbaceous, summary.intermediate.haf.woody,
                                  by.x = c("Project", "Plot", "Line", "Point"), by.y = c("Project", "Plot", "Line", "Point"),
                                  all = T)

#Technically, I should only need to filter these because there's only one entry per AIM measurement, but why not just copy/paste from above?
summary.intermediate.aim.herbaceous <- subset(data.by.point, Growth.Habit == "Herbaceous") %>% subset(Method == "AIM") %>% summarize(
  AIMHeightHerbaceous = max(na.omit(Height))
)

summary.intermediate.aim.woody <- subset(data.by.point, Growth.Habit == "Woody") %>% subset(Method == "AIM") %>% summarize(
  AIMHeightWoody = max(na.omit(Height))
)

summary.intermediate.aim <- merge(summary.intermediate.aim.herbaceous, summary.intermediate.aim.woody,
                                  by.x = c("Project", "Plot", "Line", "Point"), by.y = c("Project", "Plot", "Line", "Point"),
                                  all = T)

#There are all kinds of errors producing negative infinities where there weren't measurements to work with because that's how max() rolls.
#I just want them to be NA. This is using the same indexing principle as above
summary.intermediate.aim$AIMHeightWoody[summary.intermediate.aim$AIMHeightWoody < 0] <- NA
summary.intermediate.aim$AIMHeightHerbaceous[summary.intermediate.aim$AIMHeightHerbaceous < 0] <- NA
summary.intermediate.haf$HAFHeightWoody[summary.intermediate.haf$HAFHeightWoody < 0] <- NA
summary.intermediate.haf$HAFHeightHerbaceous[summary.intermediate.haf$HAFHeightHerbaceous < 0] <- NA

#Another merge(), this time to combine the AIM and HAF into a single data frame
raw.data.by.method <- merge(summary.intermediate.haf, summary.intermediate.aim,
                            by.x = c("Project", "Plot", "Line", "Point"), by.y = c("Project", "Plot", "Line", "Point"),
                            all.x = T)

#Summarizing the raw data that's point-by-point into plot means, so now I only group by Project and Plot instead of going down to Line or Point
plot.means <- group_by(raw.data.by.method, Project, Plot) %>% summarize(
  HAFHerbaceousMean = mean(HAFHeightHerbaceous, na.rm = T),
  HAFHerbaceousSD = sd(HAFHeightHerbaceous, na.rm = T),
  HAFHerbaceousCount = length(which(!is.na(HAFHeightHerbaceous))),
  
  HAFWoodyMean = mean(HAFHeightWoody, na.rm = T),
  HAFWoodySD = sd(HAFHeightWoody, na.rm = T),
  HAFWoodyCount = length(which(!is.na(HAFHeightWoody))),
  
  AIMHerbaceousMean = mean(AIMHeightHerbaceous, na.rm = T),
  AIMHerbaceousSD = sd(AIMHeightHerbaceous, na.rm = T),
  AIMHerbaceousCount = length(which(!is.na(AIMHeightHerbaceous))),
  
  AIMWoodyMean = mean(AIMHeightWoody, na.rm = T),
  AIMWoodySD = sd(AIMHeightWoody, na.rm = T),
  AIMWoodyCount = length(which(!is.na(AIMHeightWoody)))
)

#Write out the results! There's a hacky solution to getting it down to just the points that have both HAF and AIM there at the end
write.csv(plot.means, "MCFO_plot_means.csv")
write.csv(raw.data.by.method, "MCFO_all_data_by_point.csv")
subset(raw.data.by.method, HAFHeightHerbaceous > 0) %>% subset(AIMHeightHerbaceous > 0) %>% write.csv("MCFO_herbaceous_by_matched_points.csv")
subset(raw.data.by.method, HAFHeightWoody > 0) %>% subset(AIMHeightWoody > 0) %>% write.csv("MCFO_woody_by_matched_points.csv")
