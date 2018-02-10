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

states <- c( "AK", "AL", "AR", "AZ", "CA",
             "CO", "CT", "DC", "DE", "FL",
             "GA", "HI", "IA", "ID", "IL",
             "IN", "KS", "KY", "LA", "MA",
             "MD", "ME", "MI", "MN", "MO", 
             "MS", "MT", "NC", "ND", "NE", 
             "NH", "NJ", "NM", "NV", "NY", 
             "OH", "OK", "OR", "PA", "RI", 
             "SC", "SD", "TN", "TX", "UT", 
             "VA", "VT", "WA", "WI", "WV", "WY")

for(s in states){
  # Assign file name
  gdp_file <- paste("data/Raw Data/GDP/", s, "NGSP.csv", sep="")
  pop_file <- paste("data/Raw Data/GDP/", s, "POP.csv", sep="")

  # Read in data
  gdp_temp <- read.csv(gdp_file, header=T)
  pop_temp <- read.csv(pop_file, header=T)
  
  # Change names of variables
  names(gdp_temp) <- c("date", "gdp")
  names(pop_temp) <- c("date", "pop")
  
  # Merge on Date
  temp <- merge(gdp_temp, pop_temp, all=T)
  
  # Make a state variable
  temp$state <- s
  
  # Add to data
  data <- rbind(data, temp)
  
  # Clean directory
  rm(list=c("s", "gdp_file", "pop_file", "gdp_temp", "pop_temp", "temp"))
}

### Only keep 2003-2015 data
# Check out date variable
Data_Table(data, "date") # We only need 2003-2015

# Create a year variable
data$date <- as.character(data$date)
data$academicyear <- substring(data$date, 1, 4)
  
Data_Table(data, "academicyear")
data$academicyear <- as.numeric(data$academicyear)

# Keep 2003-2015
data <- data[data$academicyear %in% 2003:2015, ]

Data_Table(data, "academicyear")

# Remove data variable from dataset
data <- data[, -match("date", names(data))]

### Investigate variables
summary(data) # gdp is in millions and pop is in thousands

### Keep gdp in millions, but population back into raw numbers
data$pop <- 1000*data$pop

summary(data$pop)

### Create a GDP per capita variable, and put it back in terms of dollars (not millions of dollars)
data$gdp_per_capita <- (data$gdp / data$pop) * 10e6

# get rid of gdp and pop
data <- data[, -match(c("pop", "gdp"), names(data))]

### Organize a bit
# Reorder variables
data <- data[, c("state", "academicyear", "gdp_per_capita")]

# Sort by state and year
data <- data[order(data$state, data$academicyear), ]

### Investigate variables
# Check types
summary(data) # Data-types check out, no crazy numbers for gdp per capita, no missing

Data_Table(data, c("state", "academicyear")) # All states in data for all years

View(data) # all looks good!
  


##### SAVE
### Rename datafile
gdp_data <- data

### Save file
save(gdp_data, file="data/Working Data/GDP Per Capita 2003-2015.Rda")