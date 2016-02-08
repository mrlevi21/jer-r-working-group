## Script to illustrate how to assign values to a field in a data.frame using a "lookup table" - similar to vlookup in Excel


# Define the base table
base.table <- data.frame("stratum"=rep(c("Stable","Decreasing","Increasing"),2),"weight"=rep(0,6))
base.table # 6 rows, weights all zero

# Define the lookup table, assigning the stratum names as column names and the values as one row
lookup.table <- c(39414.15,381012.5,669390.9)
names(lookup.table) <- c("Decreasing","Increasing","Stable")

# make the lookup assignment based on the stratum name
base.table$weight <- lookup.table[base.table$stratum]

base.table # weights assigned based on stratum.
