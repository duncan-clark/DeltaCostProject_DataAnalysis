
### Clear Directory
rm(list=ls())

### Set Directory
setwd("data")

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
### Read in data
data1 <- read.dta13("delta_public_release_87_99.dta")
data2 <- read.dta13("delta_public_release_00_15.dta")

### Check if columns match up
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
# Check 
test1 <- data[, c("academicyear", "groupid", "unitid_linchpin", "unitid")]
nrow(unique(test1))==nrow(test1)

test2 <- test1[, c("unitid", "academicyear")]
nrow(unique(test2))==nrow(test2) # could use unitid as the unique identifier

test3 <- test1[, c("groupid", "academicyear")]
nrow(unique(test3))==nrow(test3) # could use groupid as the unique identifier

rm(list=c("test1", "test2", "test3"))

# Count how institutions are grouped
sum(data$groupid>=0)
temp <- unique(data[, c("groupid", "unitid", "unitid_linchpin", "isgrouped", "academicyear", "instname")])
groups <- tapply(temp$unitid, temp$groupid, function(x) {length(unique(x))})
groups <- groups[groups>=2]
temp <- temp[order(temp$groupid, temp$academicyear, temp$unitid),  ]
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
View(temp[(temp$unit_tag== 1 | temp$group_tag==1), ]) # Use groupid as a institution unit

rm(list=c("temp", "groups", "tag1", "tag2"))

# Sort Data by groupid and year
data <- data[order(data$groupid, data$academicyear), ]

### Get rid of variables we definitely don't need
names(data)

# Get rid of imputation flags
data <- data[, -c(439:ncol(data))]

# Get rid of extraenous location variables
to_drop <- c(
  "city",
  "zip",
  "ansi_code",
  "oberegion",
  "census_division",
  "census_region",
  "region_compact"
  )

data <- data[, -match(to_drop, names(data))]

rm(to_drop)

# Get sector variables
to_tab <- c(
  "sector", 
  "sector_revised",
  "iclevel", 
  "control"
)

Data_Table(data, to_tab) # can drop these

data <- data[, -match(to_tab, names(data))]

rm(to_tab)

### Investigate variables we're definitely going to use
# Create a function for examining missingness by year
Yearly_Missingness <- function(data, var){
  temp <- data
  output <- data.frame(sort(unique(temp$academicyear)))
  for(i in 1:length(var)){
    temp$tag = 1*is.na(temp[, var[i]])
    table <- tapply(temp$tag, temp$academicyear, function(x){100*mean(x)})
    table <- data.frame(t(t(table)))[,1]
    output <- cbind(output,table)
  }
  rm(temp) 
  rm(table)
  names(output) <- c("academicyear",var)
  return(output)
}

#Test that scalar indices are correct for 2015.
test_index <- function(data,index,scalar,year){
  
test <- unique(data[, c("academicyear", index, scalar)])
test <- test[order(test$academicyear),]
test$my_scalar <- test[,index]/(test[,index][(test$academicyear == year)])
test$diff <- test[,scalar] - test$my_scalar
summary(test$diff)
}

#CPI
test_index(data,"cpi_index","cpi_scalar_2015",2015)

#Hepi
test_index(data,"hepi_index","hepi_scalar_2015",2015)

#Heca
test_index(data,"heca_index","heca_scalar_2015",2015)


# HBCU
Data_Table(data, "hbcu")

#Aggregate data missingness test for vector of variables of interest:
var_expend <- c(
         "instruction01",
         "instruction02",
         "tot_rev_w_auxother_sum",
         "grant01",
         "instruction_share",
         "education_share",
         "noneducation_share",
         "studserv_share",
         "admin_share",
         "research_share",
         "pubserv_share",
         "depreciation01")
         
var_revenue <-c("tuition03",
                "tuition_reliance_a1",
                "govt_reliance_a",
                "nettuition_share")

var_outcome <- c("fte_count",
                 "fte12mn",
                 "grad_rate_150_n",
                 "grad_rate_150_p",
                 "grad_rate_150_n4yr",
                 "grad_rate_150_p4yr",
                 "ftretention_rate",
                 "ptretention_rate",
                 "grad_rate_adj_cohort_n",
                 "grad_rate_adj_cohort_n4yr",
                 "bachelordegrees",
                 "masterdegrees",
                 "doctordegrees",
                 "firstprofdegrees")

var_misc <- c("year_total_undergrad",
              "total_undergraduates",
              "total_enrollment_multi_tot")

Yearly_Missingness(data, var_expend)
Yearly_Missingness(data, var_revenue)
Yearly_Missingness(data,var_outcome)
Yearly_Missingness(data,var_misc)

apply(data[var_expend],MARGIN =2,FUN = summary)
apply(data[var_revenue],MARGIN =2,FUN = summary)
apply(data[var_outcome],MARGIN =2,FUN = summary)
apply(data[var_misc],MARGIN =2,FUN = summary)


# Total enrollment
summary(data$total_enrollment)
summary(data$total_enrollment_amin_tot)
summary(data$total_enrollment_asian_tot)
summary(data$total_enrollment_black_tot)
summary(data$total_enrollment_hisp_tot)
summary(data$total_enrollment_white_tot)
summary(data$total_enrollment_multi_tot) # A ton of missing
summary(data$total_enrollment_unkn_tot)
summary(data$total_enrollment_nonres_tot)


# ACT and SAT scores
names(data)
to_check <- names(data)[c(318:331)]
to_check
for(var in to_check){
  print(paste("-------------- ", var, " --------------", sep=""))
  print(summary(data[, var]))
  print("")
  Yearly_Missingness(data, var)
  print("")
  print("")
  rm(var)
} # high missing rate, and completely missing until 2002