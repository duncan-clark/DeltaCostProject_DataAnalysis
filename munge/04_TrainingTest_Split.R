# This script splits the data into training and test data, stratifying ong region, since we will include
# region fixed effects.

# We also cluster on schools, i.e if a school is selected for test/training set all its years are

set.seed(201)

obvs <- length(unique(sample$groupid)) #441

#Choose 20% of each region to go to the test set, rounding down to whole states.

split <- data.frame(groupid =unique(sample$groupid),test=0)
blocks <- as.numeric(levels(as.factor(sample$census_division)))

for(i in blocks){
  block <- unique(sample$groupid[sample$census_division == i])
  n_test <- round(length(unique(block))*0.2)
  n_train <- length(unique(block)) - n_test
  
  test <- sample(block,n_test)
  
  split$test[which((split$groupid %in% test)==TRUE)] <- 1
}

sample_train <- sample[which((sample$groupid %in% split$groupid[split$test==0]) == TRUE),]
sample_test <- sample[which((sample$groupid %in% split$groupid[split$test==1]) == TRUE),]
sample_allvar_train <- sample_allvar[which((sample$groupid %in% split$groupid[split$test==0]) == TRUE),]
sample_allvar_test <- sample_allvar[which((sample$groupid %in% split$groupid[split$test==1]) == TRUE),]

cache("sample_train")
cache("sample_test")
cache("sample_allvar_train")
cache("sample_allvar_test")



