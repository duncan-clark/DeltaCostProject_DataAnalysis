##### SETUP
### Run GDP and Unemployment Cleaning

#Shouldn't need to do this since should run in order.
#source("munge/01_GDP_Cleaning.R")
#source("munge/02_Unemployment_Cleaning.R")

### Clear Environment
rm(list=ls())

### Libraries
library(readstata13)

### Create a function that will table or summarize all variables in a dataset
Data_Table <- function(data, varlist=names(data)) {
  in_data <- names(data)[names(data) %in% varlist]
  for(var in in_data){
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

### Create a function that will examine missing rates by year for variables in a dataset
Yearly_Missingness <- function(data, varlist=names(data)){
  temp <- data
  in_data <- names(data)[names(data) %in% varlist]
  output <- data.frame(sort(unique(temp$academicyear)))
  for(var in in_data){
    temp$tag = 1*is.na(temp[, var])
    table <- tapply(temp$tag, temp$academicyear, function(x){100*mean(x)})
    table <- data.frame(t(t(table)))[,1]
    output <- cbind(output,table)
  }
  rm(temp) 
  rm(table)
  names(output) <- c("academicyear", in_data)
  return(output)
}

### Define a function that will drop a list of variables in a dataset
Drop_Var <- function(data, varlist){
  data <- data[, -match(varlist, names(data))]
  return(data)
}



##### INVESTIGATE VARIABLES WE'RE INTERESTED IN
### Read in data
data1 <- read.dta13("data/Raw_Data/Delta Cost/delta_public_release_87_99.dta")
data2 <- read.dta13("data/Raw_Data/Delta Cost/delta_public_release_00_15.dta")

### Check if columns match up in both datasets
sum(1-(names(data1)==names(data2))) # 0

### Check if variable types match up
for(var in names(data1)){
  if(typeof(data1[, var])!=typeof(data2[, var])) print(var)
} # no issues

rm(var)

### Append data together and get rid of smaller datasets
data <- rbind(data1, data2)
rm(list=c("data1", "data2"))

### Keep only 4-year public institutions
table(data$sector, data$sector_revised) # focused on bachelor's degrees, so just keep sector
data <- data[data$sector_revised==1, ]

### Check unique identifiers in data
# Check unique 
all_ids <- data[, c("academicyear", "groupid", "unitid_linchpin", "unitid")]
nrow(unique(all_ids))==nrow(all_ids) # No duplicates

unitid <- all_ids[, c("unitid", "academicyear")]
nrow(unique(unitid))==nrow(unitid) # No duplicates - could use unitid as the unique identifier

groupid <- all_ids[, c("groupid", "academicyear")]
nrow(unique(groupid))==nrow(groupid) # No duplicates - could use groupid as the unique identifier

rm(list=c("all_ids", "unitid", "groupid"))

# Count how institutions are grouped
sum(data$groupid>=0)
temp <- unique(data[, c("groupid", "unitid", "unitid_linchpin", "isgrouped", "academicyear", "instname")]) # Make dataset of all institution identifiers and year
temp <- temp[order(temp$groupid, temp$academicyear, temp$unitid),  ]
groups <- tapply(temp$unitid, temp$groupid, function(x) {length(unique(x))})
groups <- groups[groups>=2] # Only keep groupids that have more than one unitid assigned to them
View(temp[temp$groupid %in% as.numeric(names(groups)), ]) # groupid combines some institutions
temp$tag <- 1*(temp$groupid %in% as.numeric(names(groups)))

tag1 = data.frame(tapply(temp$tag, temp$groupid, function(x){max(x)}))
tag1$groupid <- as.numeric(rownames(tag1))
names(tag1)[1] <- "group_tag"

tag2 = data.frame(tapply(temp$tag, temp$unitid, function(x){max(x)}))
tag2$unitid <- as.numeric(rownames(tag2))
names(tag2)[1] <- "unit_tag"

temp <- merge(temp, tag1, all=T)
temp <- merge(temp, tag2, all=T)
temp <- temp[order(temp$groupid, temp$academicyear, temp$unitid),  ]
View(temp[(temp$unit_tag== 1 | temp$group_tag==1), ]) # No unitid's assigned to more than one groupid - just use groupid

rm(list=c("temp", "groups", "tag1", "tag2"))

# Sort Data by groupid and year
data <- data[order(data$groupid, data$academicyear), ]

### Get rid of variables we definitely don't need
names(data)

### Get rid of imputation flags
data <- data[, -c(439:ncol(data))]

### Get rid of extraenous location variables
#Don't drop yet since we select our variables later.
# to_drop <- c(
#   "city",
#   "zip",
#   "ansi_code",
#   "oberegion",
#   "census_division",
#   "census_region",
#   "region_compact"
#   )

#data <- Drop_Var(data, to_drop)

# rm(to_drop)

#### Get sector variables
to_tab <- c(
  "sector", 
  "sector_revised",
  "iclevel", 
  "control"
)
### No need to drop, select variables later
# Data_Table(data, to_tab) # can drop these
# 
# data <- Drop_Var(data, to_tab)
# 
# rm(to_tab)
###



### Test that scalar indices are correct for 2015.
# Define a function to test scalars
test_index <- function(data,index,scalar,year){
  test <- unique(data[, c("academicyear", index, scalar)])
  test <- test[order(test$academicyear),]
  test$my_scalar <- test[,index]/(test[,index][(test$academicyear == year)])
  test$diff <- test[,scalar] - test$my_scalar
  summary(test$diff)
}

# CPI
test_index(data,"cpi_index","cpi_scalar_2015",2015) # Essentially 0

# Hepi
test_index(data,"hepi_index","hepi_scalar_2015",2015) # Essentially 0

# Heca
test_index(data,"heca_index","heca_scalar_2015",2015) # Essentially 0

### Test to see if fte_count and fte12mn are different 
summary(data$fte_count)
Yearly_Missingness(data, "fte_count")

summary(data$fte12mn)
Yearly_Missingness(data, "fte12mn") # not available before 2004

test <- abs((data$fte_count - data$fte12mn) / data$fte_count)
test <- sort(test)
summary(test) # generally not that off, just use fte_count
tail(test, 100) # some weird numbers here
rm(test)

### Examine variables we plan of using
# Define vectors with lists of outcomes, covariates, normalizing constants/variables, and expenditure variables
var_outcomes <- c(
  "bachelordegrees",
  "grad_rate_150_p4yr"
)

var_covariates <- c(
  "total_enrollment_black_tot",
  "total_enrollment_hisp_tot",
  "total_enrollment_white_tot",
  "total_enrollment_asian_tot",
  "total_enrollment_amin_tot",
  "total_enrollment_multi_tot",
  "total_enrollment_unkn_tot",
  "total_enrollment_nonres_tot",
  "total_enrollment",
  "grant01",
  "state03",
  "total_undergraduates",
  "nettuition01",
  "tot_rev_w_auxother_sum"
)

var_normalizers <- c(
  "fte_count",
  "cpi_scalar_2015",
  "hepi_scalar_2015",
  "heca_scalar_2015"
)

# Old variable lists
var_expend <- c(
  "instruction01",
  "instruction02",
  "research01",
  "research02",
  "pubserv01",
  "pubserv02",
  "acadsupp01",
  "acadsupp02",
  "studserv01",
  "studserv02",
  "instsupp01",
  "instsupp02",
  "opermain01",
  "opermain02",
  "depreciation01",
  "grants01",
  "auxiliary01",
  "auxiliary02",
  "hospital01",
  "hospital02",
  "independ01",
  "independ02",
  "otheroper01",
  "otheroper02"
)

# Check missing rates
Yearly_Missingness(data, var_outcomes)

Yearly_Missingness(data, var_covariates) # Look pretty good!

Yearly_Missingness(data, var_normalizers) # Look pretty good!

Yearly_Missingness(data, var_expend) # Some have very high missing rates - will need to drop some of these or do imputation

# Check summaries
apply(data[var_expend],MARGIN =2,FUN = summary)

apply(data[var_outcomes],MARGIN =2,FUN = summary) # Some very low and very high values - may want to think about dropping those schools

apply(data[var_covariates],MARGIN =2,FUN = summary) # some schools with 0 undergraduates - will want to drop those

apply(data[var_normalizers],MARGIN =2,FUN = summary) # school with 1 FTE - will want to drop those

### Test student service expenditures
pct_education_rev <- (data$instruction01 + data$studserv01)/data$tot_rev_w_auxother_sum
pct_education_rev <- sort(pct_education_rev)
summary(pct_education_rev)
tail(pct_education_rev, 100)

rm(pct_education_rev)

### Total enrollment
summary(data$total_enrollment)
summary(data$total_enrollment_amin_tot)
summary(data$total_enrollment_asian_tot)
summary(data$total_enrollment_black_tot)
summary(data$total_enrollment_hisp_tot)
summary(data$total_enrollment_white_tot)
summary(data$total_enrollment_multi_tot) # A ton of missing
summary(data$total_enrollment_unkn_tot)
summary(data$total_enrollment_nonres_tot)

### Test total expenditures
pct_education_exp <- (data$instruction01 + data$studserv01)/data$total01
summary(pct_education_exp)
pct_education_exp <- sort(pct_education_exp)
tail(pct_education_exp) # a few very high numbers, but looks good otherwise
rm(pct_education_exp)

### ACT and SAT scores
names(data)
test_scores <- names(data)[c(318:331)]
Yearly_Missingness(data, test_scores) # too high - probably unusuable

rm(test_scores)

### CONCLUSIONS 
  # fte_count and fte12mn are similar - use fte_count = fall enrollment.
  # Full-time / Part-time employees too high a missing rate - can't use
  # sufficient race data to include as covariate - i.e. under represented minorities black & hispanic
  # post 2003 have low levels of missingness in all covariates and possible outcomes - restrict to post 2003
  # grad rate variables typically have high missingness ~10% so may not be the best variable.
  # ACT/SAT have high missingness rates so can't use
  


##### MAKE FINAL SAMPLE
### Define ID variables we're going to use
var_ids <- c(
  "groupid", 
  "unitid", 
  "instname", 
  "academicyear", 
  "state",
  "census_division"
)

### Define other variables we may  be interested in
#var_other <- c(
#  "associatedegrees",
#  "masterdegrees",
#  "doctordegrees",
#  "firstprofdegrees",
#  "totaldegrees",
#  "any_aid_pct",
#  "instruction_share",
#  "education_share",
#  "noneducation_share",
#  "studserv_share",
#  "admin_share",
#  "research_share",
#  "pubserv_share"
#)

### Create dataset with just the variables we need
sample <- data

### Keep only observations starting in 2003
sample <- sample[sample$academicyear>=2003, ]

# Check how many schools we start with in this time range
num_schools <- length(unique(sample$groupid))
num_schools # 553 universities

### Only keep schools from the 50 states plus DC
# Define states in sample
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

Data_Table(sample, "state")
length(unique(sample$state))

# Only keep the states listed above
sample <- sample[sample$state %in% states, ]

length(unique(sample$state))==length(states) # the same - good

# Keep track of how may schools lost / still in sample
num_schools - length(unique(sample$groupid)) # 9 schools dropped
num_schools <- length(unique(sample$groupid)) # replace number of schools we have
num_schools # 544

### Drop schools that didn't exists in entirety of the time range
# Get total number of years for each school
total_yrs <- length(unique(sample$academicyear))

yrs_existed <- data.frame(tapply(sample$academicyear, sample$groupid, function(x){length(x)}))
names(yrs_existed) <- "yrs_existed"
yrs_existed$groupid <- as.numeric(rownames(yrs_existed))

Data_Table(yrs_existed, "yrs_existed") # Will keep large majority of the dataset - 505 schools kept

# Drop schools that have less than the total years
sample <- merge(sample, yrs_existed, all=T)
sample <- sample[sample$yrs_existed==total_yrs, ]

rm(yrs_existed)

# Keep track of how may schools lost / still in sample
num_schools - length(unique(sample$groupid)) # 39 schools dropped
num_schools <- length(unique(sample$groupid)) # replace number of schools we have
num_schools # 505

# Remove yrs_existed from the dataset
sample <- Drop_Var(sample, "yrs_existed")
rm(total_yrs)

### Drop schools that are missing bachelor's degrees or graduation rates, or gave out 0 bachelor's degrees
# Check to see if graduation rate variables are ever part-missing
sum(1 - (is.na(sample$grad_rate_150_n4yr)==is.na(sample$grad_rate_150_p4yr) & is.na(sample$grad_rate_150_n4yr)==is.na(sample$grad_rate_adj_cohort_n4yr)))# 6
sample$tag[(is.na(sample$grad_rate_150_n4yr)==is.na(sample$grad_rate_150_p4yr) & is.na(sample$grad_rate_150_n4yr)==is.na(sample$grad_rate_adj_cohort_n4yr))==FALSE] <- 1
View(sample[sample$tag==1 & !is.na(sample$tag), var_outcomes]) # will just have to drop this university - no way to recover the other two missing variables
sample <- Drop_Var(sample, "tag")

# Tag schools that are missing bachelor's degrees or graduation rates in a year
sample$temp <- 0
sample$temp[is.na(sample$grad_rate_150_p4yr)] <- 1
sample$temp[is.na(sample$bachelordegrees)] <- 1

Data_Table(sample, "temp")
to_drop <- data.frame(tapply(sample$temp, sample$groupid, function(x){max(x)}))
names(to_drop) <- "to_drop"
to_drop$groupid <- as.numeric(rownames(to_drop))
Data_Table(to_drop, "to_drop") # will lose 48 schools

# Drop schools
sample <- merge(sample, to_drop, all=T)
sample <- sample[sample$to_drop!=1, ]

# Keep track of how may schools lost / still in sample
num_schools - length(unique(sample$groupid)) # 48 schools dropped
num_schools <- length(unique(sample$groupid)) # replace number of schools we have
num_schools # 457

# Remove tags from the dataset
sample <- Drop_Var(sample, c("temp", "to_drop"))
rm(to_drop)

### Drop schools that gave out 0 bachelor's degrees in a year or had fewer bachelor's degrees awarded than associate's degrees
# Tag entries where this is the case
sample$temp <- 0
sample$temp[!is.na(sample$bachelordegrees) & sample$bachelordegrees==0] <- 1
sample$temp[!is.na(sample$bachelordegrees) & !is.na(sample$associatedegrees) & sample$bachelordegrees<sample$associatedegrees] <- 1

Data_Table(sample, "temp")
View(sample[sample$temp==1, c("bachelordegrees", "associatedegrees")])
to_drop <- data.frame(tapply(sample$temp, sample$groupid, function(x){max(x)}))
names(to_drop) <- "to_drop"
to_drop$groupid <- as.numeric(rownames(to_drop))
Data_Table(to_drop, "to_drop") # will lose 12 schools

# Drop schools
sample <- merge(sample, to_drop, all=T)
sample <- sample[sample$to_drop!=1, ]

# Keep track of how may schools lost / still in sample
num_schools - length(unique(sample$groupid)) # 12 schools dropped
num_schools <- length(unique(sample$groupid)) # replace number of schools we have
num_schools # 445

# Remove tags from the dataset
sample <- Drop_Var(sample, c("temp", "to_drop"))
rm(to_drop)

### Remove schools that had too high or too low bachelor's degrees awarded
summary(sample$bachelordegrees)
hist(sample$bachelordegrees, breaks=100)
  # Some huge values

quantile(sample$bachelordegrees, seq(0, 1, length=201)) # Check top 2.5% - 97.5th percentile is 8285

# Tag schools that have total degrees awarded in top 2.5% of values
sample$temp<- 0
sample$temp[sample$bachelordegrees>=8285.00] <- 1

Data_Table(sample, "temp")
to_check <- data.frame(tapply(sample$temp, sample$groupid, function(x){max(x)}))
names(to_check) <- "to_check"
to_check$groupid <- as.numeric(rownames(to_check))
Data_Table(to_check, "to_check") # 17 schools

# Investigate
sample <- merge(sample, to_check, all=T)
View(sample[sample$to_check==1, c("groupid", "instname", "academicyear", "bachelordegrees", "total_undergraduates")])
  # Nothing implausible actually (in relation to total undergraduates on campus) - just keep them

# Remove tags from the dataset
sample <- Drop_Var(sample, c("temp", "to_check"))
rm(to_check)

### Check missing rates of all the variables now
Yearly_Missingness(sample, var_ids)

Yearly_Missingness(sample, var_outcomes) 

Yearly_Missingness(sample, var_covariates) # remove multi-racial
var_covariates <- var_covariates[-match("total_enrollment_multi_tot", var_covariates)]

Yearly_Missingness(sample, var_expend) # will need to drop some of these
to_drop <- c(
    "research01",
    "research02",
    "opermain02",
    "hospital01",
    "hospital02",
    "independ01",
    "independ02",
    "otheroper01",
    "otheroper02"
)
var_expend <- var_expend[-match(to_drop, var_expend)]
Yearly_Missingness(sample, var_expend) 
rm(to_drop)

Yearly_Missingness(sample, var_normalizers)

### Drop schools missing covariates / independent variables for the whole time
#function checks if every entry for that group is missing the variable for 13 years (or more) 
missing_var <- function(varlist,data){
  missing_var <- NULL
  for(var in varlist){
  tmp <- tapply(data[,var],data$groupid,function(x){
    sum(is.na(x))  } )
  missing_var <- c(missing_var,rownames(tmp[(tmp >= 13)])) # 13 since we have 13 years here
  
  }
  return(missing_var)
}

missing_var(c(var_expend,var_outcomes,var_covariates,var_normalizers),sample)

missing_var(var_expend,sample) # Missingness here
missing_var(var_outcomes,sample)
missing_var(var_covariates,sample) # Missingness here
missing_var(var_normalizers,sample)

length(unique(missing_var(c(var_expend,var_outcomes,var_covariates,var_normalizers),sample)))
#13 schools missing some covariates in every year - drop these schools.

drop <- missing_var(c(var_expend,var_outcomes,var_covariates,var_normalizers),sample)
keep = 1 - (sample$groupid %in% drop) 

sample <- sample[(keep==1),]

rm(keep)
rm(drop)

# Keep track of how may schools lost / still in sample
num_schools - length(unique(sample$groupid)) # 15 schools dropped
num_schools <- length(unique(sample$groupid)) # replace number of schools we have
num_schools # 432

### Check Missing values again
# Check missing value now
Yearly_Missingness(sample, var_ids)

Yearly_Missingness(sample, var_outcomes) 

Yearly_Missingness(sample, var_covariates) 

Yearly_Missingness(sample, var_expend)

Yearly_Missingness(sample, var_normalizers)

# Check summaries
apply(sample[var_expend],MARGIN =2,FUN = summary)

apply(sample[var_outcomes],MARGIN =2,FUN = summary) 

apply(sample[var_covariates],MARGIN =2,FUN = summary)

apply(sample[var_normalizers],MARGIN =2,FUN = summary) 



##### IMPUTE MISSING VALUES FOR KEY VARIABLES AND ONLY KEEP VARIABLES WE NEED
### Recode categorical variables in data as such
sample$groupid <- as.factor(sample$groupid)
sample$academicyear <- as.factor(sample$academicyear)
sample$state <- as.factor(sample$state)
sample$census_division <- as.factor(sample$census_division)

### Standardize monetary variables to 2015 values
# Define function to rescale to 2015 dollars for a specified index
inflation_transform <- function(data, varlist, index){
  for(var in varlist){
    data[ ,paste(var, "_", index, sep ="")] <- data[ ,var]/data[ ,paste(index, "_scalar_2015", sep="")]  # (current yr $) / (current yr $ / 2015 $) = (2015 $)
  }
  return(data)
}

# Define variables we want to standardize
var_cpi <- c(
  "grant01",
  "state03",
  "nettuition01",
  "tot_rev_w_auxother_sum",
  var_expend
)

# Standardize
sample <- inflation_transform(sample, var_cpi, "cpi")

### Impute
# Define variables we want to impute for
vars_impute <- c(
  var_covariates[-match(intersect(var_cpi, var_covariates), var_covariates)],
  paste(var_cpi, "_cpi", sep=""),
  "fte_count"
)

# Impute by doing fixed effects model with groupid and year
for(var in vars_impute){
  model <- lm(sample[, var] ~ sample$groupid + sample$academicyear)
  indeces <- is.na(sample[, var])
  n <- sum(indeces)
  sample[indeces, var] <- predict(model, newdata=sample)[indeces]
  print(paste(var, ": ", n, " change(s) made"))
  
  rm(list=c("var", "model", "n"))
}

### Use imputed cpi variables to fill in values for original variables
for(var in var_cpi){
  indeces <- is.na(sample[, var])
  sample[indeces, var] <- sample[indeces, paste(var, "_cpi", sep="")] * sample[indeces, "cpi_scalar_2015"]
}

### Remove cpi variables from data
sample <- sample[, -match(paste(var_cpi, "_cpi", sep=""), names(sample))]

### Check missing rates to be sure
Yearly_Missingness(sample, var_ids)

Yearly_Missingness(sample, var_outcomes) 

Yearly_Missingness(sample, var_covariates) 

Yearly_Missingness(sample, var_expend)

Yearly_Missingness(sample, var_normalizers)



##### MAKE/GET VARIABLES WE NEED FOR ANALYSIS
### Only keep variables we intend on using
sample <- sample[, c(var_ids, var_normalizers, var_expend, var_covariates, var_outcomes)]

### Merge with unemployment and GDP
# Load unemployment and GDP datasets
load("data/Working_Data/GDP Per Capita 2003-2015.Rda")
load("data/Working_Data/Unemployment 2003-2015.Rda")

unemployment_data$state[unemployment_data$state==""] <- "AL"

# Merge
sample <- merge(sample, gdp_data, by = c("academicyear","state"))
sum(is.na(sample$gdp_per_capita)) # 0

sample <- merge(sample, unemployment_data, by = c("academicyear","state"))
sum(is.na(sample$unemployment)) # 0

### Generate race % variables
sample$pct_black <- 100*sample$total_enrollment_black_tot / sample$total_enrollment
sample$pct_hisp <- 100*sample$total_enrollment_hisp_tot / sample$total_enrollment
sample$pct_white <- 100*sample$total_enrollment_white_tot / sample$total_enrollment
sample$pct_asian <- 100*sample$total_enrollment_asian_tot / sample$total_enrollment
sample$pct_other <- 100 -(sample$pct_black + sample$pct_hisp + sample$pct_white + sample$pct_asian)

sample$pct_urm <- sample$pct_black + sample$pct_hisp
sample$pct_nonurm <- sample$pct_white + sample$pct_asian

### Create a variable that's the total spent on education
#sample$education_ttl <- sample$instruction01 + sample$studserv01

### Transform variables into 2015 dollars and/or per FTE
# Define function to rescale to per FTE
fte_transform <- function(data, varlist){
  for(var in varlist){
    data[ ,paste(var,"_fte",sep ="")] <- data[ ,var]/data$fte_count
  }
  return(data)
}

# rescale to 2015 dollars (cpi)
var_cpi_only <- c(
  "gdp_per_capita"
)

sample <- inflation_transform(sample, var_cpi_only, "cpi")

# rescale to FTE
var_fte_only <- c(
  "bachelordegrees"
)

sample <- fte_transform(sample, var_fte_only)

# Rescale to 2015 dollars key variables (using cpi) AND fte
var_cpi_fte <- c(
  "grant01",
  "state03",
  "nettuition01",
  "tot_rev_w_auxother_sum",
  var_expend
)

sample <- inflation_transform(sample, var_cpi_fte, "cpi")
sample <- fte_transform(sample, paste(var_cpi_fte, "_cpi", sep=""))

# Check
names(sample)

ids <- c("groupid","academicyear")
sample_allvar <- merge(data,sample[,ids],by =ids)



##### FINAL TOUCHES AND SAVE
### Sort
sample <- sample[order(sample$groupid, sample$academicyear), ]

### Save file
save(sample, file="data/Working_Data/sample.Rda")
save(sample, file="data/Working_Data/sample_allvar.Rda")

cache("sample")
cache("sample_allvar")
