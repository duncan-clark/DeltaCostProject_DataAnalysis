#### SET-UP
### Clear Directory
rm(list=ls())

### Create a function that will table or summarize all variables in a dataset
Data_Table <- function(data, varlist=names(data)) {
  for(var in names(data)[names(data) %in% varlist]){
    print(paste("---------", deparse(substitute(data)), ": ", var, "-----------", sep=" "), quote=FALSE)
    table <- table(data[,var], useNA="ifany")
    if(is.na(names(table)[length(table)])) names(table)[length(table)] <- "NA"
    table <- data.frame(t(rbind(table, prop.table(table)*100)))
    names(table) <- c("Number", "Percentage")
    table$Percentage <- paste(as.character(round(table$Percentage, digits=2)), "%", sep="")
    print(table, quote=FALSE)
    print("", quote=FALSE)
    print("", quote=FALSE)
    rm("table")
    
    rm("var")
  } # end of for(var in names(data)[names(data) %in% varlist])
} # end of function(data, varlist=names(data))



##### CLEANING
### Read in Data
data <- NULL # empty dataset to rbind each year onto

for(yr in 2003:2015){
  # Assign file name
  file <- paste("data/Raw Data/Unemployment/", yr, ".csv", sep="")
  
  # Read in data
  temp <- read.csv(file, header=F)
  
  # If seperating character is different, try using other separator
  if(ncol(temp)==1) temp <- read.csv(file, sep="\t", header=F)
  
  # Create academicyear
  temp$academicyear <- yr
  
  # Add to data
  data <- rbind(data, temp)
  
  # Clean directory
  rm(list=c("yr", "file", "temp"))
}

### Change names of data
names(data)[1] <- "state_long"
names(data)[2] <- "unemployment"

### Change State names to state abbreviations
data$state_long <- toupper(data$state_long)

# Create new state variable (sample variable name as in delta cost)
data$state <- ""
data$state[data$state_long=="ALABAMA"] <- "AL"
data$state[data$state_long=="ALASKA"] <- "AK"
data$state[data$state_long=="ARIZONA"] <- "AZ"
data$state[data$state_long=="ARKANSAS"] <- "AR"
data$state[data$state_long=="CALIFORNIA"] <- "CA"
data$state[data$state_long=="COLORADO"] <- "CO"
data$state[data$state_long=="CONNECTICUT"] <- "CT"
data$state[data$state_long=="DELAWARE"] <- "DE"
data$state[data$state_long=="DISTRICT OF COLUMBIA"] <- "DC"
data$state[data$state_long=="FLORIDA"] <- "FL"
data$state[data$state_long=="GEORGIA"] <- "GA"
data$state[data$state_long=="HAWAII"] <- "HI"
data$state[data$state_long=="IDAHO"] <- "ID"
data$state[data$state_long=="ILLINOIS"] <- "IL"
data$state[data$state_long=="INDIANA"] <- "IN"
data$state[data$state_long=="IOWA"] <- "IA"
data$state[data$state_long=="KANSAS"] <- "KS"
data$state[data$state_long=="KENTUCKY"] <- "KY"
data$state[data$state_long=="LOUISIANA"] <- "LA"
data$state[data$state_long=="MAINE"] <- "ME"
data$state[data$state_long=="MARYLAND"] <- "MD"
data$state[data$state_long=="MASSACHUSETTS"] <- "MA"
data$state[data$state_long=="MICHIGAN"] <- "MI"
data$state[data$state_long=="MINNESOTA"] <- "MN"
data$state[data$state_long=="MISSISSIPPI"] <- "MS"
data$state[data$state_long=="MISSOURI"] <- "MO"
data$state[data$state_long=="MONTANA"] <- "MT"
data$state[data$state_long=="NEBRASKA"] <- "NE"
data$state[data$state_long=="NEVADA"] <- "NV"
data$state[data$state_long=="NEW HAMPSHIRE"] <- "NH"
data$state[data$state_long=="NEW JERSEY"] <- "NJ"
data$state[data$state_long=="NEW MEXICO"] <- "NM"
data$state[data$state_long=="NEW YORK"] <- "NY"
data$state[data$state_long=="NORTH CAROLINA"] <- "NC"
data$state[data$state_long=="NORTH DAKOTA"] <- "ND"
data$state[data$state_long=="OHIO"] <- "OH"
data$state[data$state_long=="OKLAHOMA"] <- "OK"
data$state[data$state_long=="OREGON"] <- "OR"
data$state[data$state_long=="PENNSYLVANIA"] <- "PA"
data$state[data$state_long=="RHODE ISLAND"] <- "RI"
data$state[data$state_long=="SOUTH CAROLINA"] <- "SC"
data$state[data$state_long=="SOUTH DAKOTA"] <- "SD"
data$state[data$state_long=="TENNESSEE"] <- "TN"
data$state[data$state_long=="TEXAS"] <- "TX"
data$state[data$state_long=="UTAH"] <- "UT"
data$state[data$state_long=="VERMONT"] <- "VT"
data$state[data$state_long=="VIRGINIA"] <- "VA"
data$state[data$state_long=="WASHINGTON"] <- "WA"
data$state[data$state_long=="WEST VIRGINIA"] <- "WV"
data$state[data$state_long=="WISCONSIN"] <- "WI"
data$state[data$state_long=="WYOMING"] <- "WY"

# Get rid of state long
data <- data[, -match("state_long", names(data))]

### Organize a bit
# Reorder variables
data <- data[, c("state", "academicyear", "unemployment")]

# Sort by state and year
data <- data[order(data$state, data$academicyear), ]

### Investigate variables
# Check types
summary(data) # Data-types check out, no crazy numbers for unemployment

Data_Table(data, c("state", "academicyear")) # All states in data for all years

View(data) # all looks good!
  


##### SAVE
### Rename datafile
unemployment_data <- data

### Save file
save(unemployment_data, file="data/Working Data/Unemployment 2003-2015.Rda")