###How to do some basic manipulation of data tables

##Start off by "requiring" dplyr and stringr, although you need to have install.packages()ed them at some point previously or require() just throws an error
require(dplyr)
require(stringr)
#dplyr gives us a lot of useful commands for summarizing data, specifically group_by() and summarize()
#It also includes the operator %>% from the package magrittr which basically means "take the output of
#the previous function/operations and use it as the first argument in the next" sorta like a pipe, but "ceci n'est pas une pipe."

#The filename of the .csv in the folder that we want to digest
input.file <- "SLFVO+BFO_records_101115"
usda.species.file <- "nm_id_usdaspecies"

#Take the .csv file exported from Excel and saved into the project folder this script is running in
raw.data <- read.csv(paste(input.file,".csv",sep=""), stringsAsFactors = F)
usda.species.data <- read.csv(paste(usda.species.file,".csv",sep=""), stringsAsFactors = F)
#The function read.csv() takes a bunch of arguments, but I only specify the file to read
#raw.data is just my data frame that until this line didn't exist
#The <- means "set equal to" so it's: "Set raw.data equal to the table that read.csv() is importing"
#The stringsAsFactors = F argument is because the default is T and several functions were incredibly unhappy about
#having factors when trying to summarize these data farther down

#I tweaked the input already in Excel so that instead of 0s there are no values which translate to NA when imported
#but that could be done after importing with a simple bit of R trickery too. This is relevant to this particular dataset.
#To use with another .csv, just change the target name, that is, the value of the string input.file

#Creating a data frame of graminoid species and another of forbs
#grep() returns the positions of the values that the search pattern can be found in, grepl() just returns TRUE/FALSE if it can find the pattern
#So this creates subsets where the entries are those where the search for the growth habits return TRUE
graminoids <- usda.species.data %>% subset(grepl("raminoid", Growth.Habit))
forbs <- usda.species.data %>% subset(grepl("orb", Growth.Habit))

#Adding these two columns to the raw data because otherwise there are weird errors in the next part
#I originally defined these as NA, but that broke first() in my summarize() late, so they're strings now and stripped later
raw.data$HAFHabitHerbaceous <- "NO"
raw.data$AIMHabitHerbaceous <- "NO"

#This would probably be best rewritten with lapply, but I'm more comfortable with loops and they're easier to read for a newcomer
#Looping through the raw data one row at a time. In each row it checks to see if the recorded species is in the
#forb data frame, graminoid data frame, or a generic code and then writes the corresponding growth habit into
#the relevant column initialized above.
for (n in 1:nrow(raw.data)) {
  #So that's: look at the data frame raw.data and in the column HAFSpeciesHerbaceous in row n then compare it to
  #all the entries in the data frame forbs in the column Accepted.Symbol. Comparing with %in% just returns TRUE
  #if the first thing is in the second thing. If we get a TRUE, we write in "Forb"; otherwise we move on to the next test
  if (raw.data$HAFSpeciesHerbaceous[n] %in% forbs$Accepted.Symbol){
    raw.data$HAFHabitHerbaceous[n] <- "Forb"
  } else if (raw.data$HAFSpeciesHerbaceous[n] %in% graminoids$Accepted.Symbol){
    raw.data$HAFHabitHerbaceous[n] <- "Graminoid"
    #So, that covers all the matching codes. We'll only use these "else if"s if the code isn't a recognized species
    #which means that we can look for the two letters associated with the generic codes without accidentally catching
    #a species code that happens to be something like "TAFO"
    #grepl() looks for the specified string in another string and returns TRUE if it's there. The || is our Boolean OR.
  } else if (grepl("PG", raw.data$HAFSpeciesHerbaceous[n]) || grepl("AG", raw.data$HAFSpeciesHerbaceous[n])){
    raw.data$HAFHabitHerbaceous[n] <- "Graminoid"
  } else if (grepl("PF", raw.data$HAFSpeciesHerbaceous[n]) || grepl("AF", raw.data$HAFSpeciesHerbaceous[n])){
    raw.data$HAFHabitHerbaceous[n] <- "Forb"
  }
  if (raw.data$AIMSpeciesHerbaceous[n] %in% forbs$Accepted.Symbol){
    raw.data$AIMHabitHerbaceous[n] <- "Forb"
  } else if (raw.data$AIMSpeciesHerbaceous[n] %in% graminoids$Accepted.Symbol){
    raw.data$AIMHabitHerbaceous[n] <- "Graminoid"
  } else if (grepl("PG", raw.data$AIMSpeciesHerbaceous[n]) || grepl("AG", raw.data$AIMSpeciesHerbaceous[n])){
    raw.data$AIMHabitHerbaceous[n] <- "Graminoid"
  } else if (grepl("PF", raw.data$AIMSpeciesHerbaceous[n]) || grepl("AF", raw.data$AIMSpeciesHerbaceous[n])){
    raw.data$AIMHabitHerbaceous[n] <- "Forb"
  }
}

#Using dplyr function group_by() to group the individual point measurements
#in a way that R can recognize as grouped. Because computers lack reading comprehension skills.
data.by.point <- group_by(raw.data, Project, Plot, Line, Point)
#This makes a new data frame called data.by.plot and sets it equal to
#the results of running group_by() on raw.data, asking it to group raw.data by
#the columns called Project and Plot


#SO! The way I received the initial dataset, there were two measurements for each point, but they were recorded as separate entries
#This intermediate.summary is look at at all the entries for a single point on a plot and taking the first non-NA entry it can find
#which should be the only non-NA entry for that method at that point on that plot. If that method had no measurement and it therefore
#can't find a non-NA value, it
intermediate.summary <- summarize(data.by.point,
                                  #So, for every point, this pulls out the first non-NA value in that column but still writes NA
                                  #if it can't find anything that isn't an NA. This was painful to figure out the syntax for.
                                  HAFHeightWoody = first(na.omit(as.numeric(HAFHeightWoody)), default=NA),
                                  HAFSpeciesWoody = first(na.omit(as.character(HAFSpeciesWoody)), default=NA),
                                  HAFHeightHerbaceous = first(na.omit(as.numeric(HAFHeightHerbaceous)), default=NA),
                                  HAFSpeciesHerbaceous = first(na.omit(as.character(HAFSpeciesHerbaceous)), default=NA),
                                  HAFHabitHerbaceous = first(na.omit(as.character(HAFHabitHerbaceous)), default=NA),
                                  AIMHeightWoody = first(na.omit(as.numeric(AIMHeightWoody)), default=NA),
                                  AIMSpeciesWoody = first(na.omit(as.character(AIMSpeciesWoody)), default=NA),
                                  AIMHeightHerbaceous = first(na.omit(as.numeric(AIMHeightHerbaceous)), default=NA),
                                  AIMSpeciesHerbaceous = first(na.omit(as.character(AIMSpeciesHerbaceous)), default=NA),
                                  AIMHabitHerbaceous = first(na.omit(as.character(AIMHabitHerbaceous)), default=NA)
                                  )

#Stupid "NO" strings are messing with our growth habit information, so I'm going to strip them out with this loop, setting us up
#for the final stage of summarization!
for (n in 1:nrow(intermediate.summary)){
  if (intermediate.summary$HAFHabitHerbaceous[n] == "NO"){
    intermediate.summary$HAFHabitHerbaceous[n] <- NA
  }
  if (intermediate.summary$AIMHabitHerbaceous[n] == "NO"){
    intermediate.summary$AIMHabitHerbaceous[n] <- NA
  }
}


#Unfortunately, we need to do the woody and herbaceous things separately because of this growth habit business and will merge later
#NOTE: Right now this ALL POINTS. Filtering down to only points where both HAF and AIM had a measurement returned three plots
#The dplyr function summarize() makes our lives much easier here
#because it saves us from writing a slow, complicated custom function to give us
#means for the heights on each plot, standard deviations on those means, and the n for the means
haf.herbaceous.plot.means <- filter(intermediate.summary, HAFHeightHerbaceous > 0) %>%
                          group_by(Project, Plot, HAFHabitHerbaceous) %>%
                          summarize(
                            HAFHerbaceousMean=mean(as.numeric(HAFHeightHerbaceous), na.rm=T),
                            HAFHerbaceousSD=sd(as.numeric(HAFHeightHerbaceous), na.rm=T),
                            HAFHerbaceousCount=length(which(!is.na(as.numeric(HAFHeightHerbaceous))))
                          )

aim.herbaceous.plot.means <- filter(intermediate.summary, AIMHeightHerbaceous > 0) %>%
                          group_by(Project, Plot, AIMHabitHerbaceous) %>%
                          summarize(
                            AIMHerbaceousMean=mean(as.numeric(AIMHeightHerbaceous), na.rm=T),
                            AIMHerbaceousSD=sd(as.numeric(AIMHeightHerbaceous), na.rm=T),
                            AIMHerbaceousCount=length(which(!is.na(as.numeric(AIMHeightHerbaceous))))
                          )

woody.plot.means <- filter(intermediate.summary, HAFHeightWoody > 0 && AIMHeightWoody > 0) %>%
                  group_by(Project, Plot) %>%
                  summarize(
                             HAFWoodyMean=mean(as.numeric(HAFHeightWoody), na.rm=T), #Make a column called HAFWoodyMean and set it equal to the result of the mean of the values in the column HAFHeightWoody ignoring NAs and forced to be numbers, not strings
                             HAFWoodySD=sd(as.numeric(HAFHeightWoody), na.rm=T), #Make a column that is the standard deviation of the values in the column HAFHeightWoody (forced to be numbers, not strings), ignoring the NA values
                             HAFWoodyCount=length(which(!is.na(as.numeric(HAFHeightWoody)))), #Finding the length (count) of the indices of the values in HAFHeightWoody (forced to be numeric), but only those that aren't NAs. is.na() returns TRUE if it's an NA but the ! inverts things, so !is.na returns true if it ISN'T an NA
                             AIMWoodyMean=mean(as.numeric(AIMHeightWoody), na.rm=T),
                             AIMWoodySD=sd(as.numeric(AIMHeightWoody), na.rm=T),
                             AIMWoodyCount=length(which(!is.na(as.numeric(AIMHeightWoody))))
)

write.csv(haf.herbaceous.plot.means, "haf.herbaceous.plot.means.csv")
write.csv(aim.herbaceous.plot.means, "aim.herbaceous.plot.means.csv")
#This last one isn't as critical, honestly, because of previous scripts' output weeks ago, but still included for the sake of having the data
write.csv(woody.plot.means, "woody.plot.means.csv")
