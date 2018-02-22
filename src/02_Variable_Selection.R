## This script perform lasso,ridge and elastic net on the training data in order to identify
## the important variables without pre concieved ideas on what variables might be useful

# Code split into Sections ctrl f #1) #2) etc. for each section

library(ProjectTemplate)
load.project()

#1)Examine data

#remove all outcome variables apart from "grad_rate_150_p4y" otherwise since these are highly
#correlated regularisation will just throw away everything else.

outcomes <- names(sample_allvar_train)[grep("grad_rate",names(sample_allvar_train))]
outcomes_drop <- outcomes[-match("grad_rate_150_p4yr",outcomes)]
sample_reg_train <- sample_allvar_train[,-match(outcomes_drop,names(sample_allvar))]

non_numeric <- apply((sample_reg_train),MARGIN =2 ,function(x){ all(is.na(as.numeric(x))) })
#remove non numeric variables

names(sample_reg_train)[non_numeric]
#instname"   "tcsname"    "city"       "state"      "tuition01" 
#"tuition02"  "grant03_04" "grants01u"  "grants01r" 
# okay to drop, some that should be numeric have all NAs

sample_reg_train <- sample_reg_train[,as.logical(1-non_numeric)]
sample_reg_train <- apply(sample_reg_train,MARGIN = 2, as.numeric)
#some zip codes not numeric - cause NAs. don't worry about this.

no_na <- apply((sample_reg_train),MARGIN =2 ,function(x){ all(!is.na(x)) })
sum(no_na)
#316 out of 421 variables have nas - removing these would remove a lot of flexibility - do this for now
#Could lookup regularisation with missing data, and various approaches for this.

#Count mean number of nas for each column:
numbers_nas <- apply(sample_reg_train[,as.logical(1-no_na)], MARGIN =2, function(x){sum(is.na(x))})
summary(numbers_nas)
#number of nas is skewed by a few columns with all nas, median of around 1/6 of observations, suggests
#that throwing all these variables away would cause loss of information.


## Approach1 replaces the nas with the mean for that column
sample_reg_train1 <- sample_reg_train
col_means <- apply(as.matrix(sample_reg_train), MARGIN =2, function(x){mean(x, na.rm = TRUE)})
summary(col_means)

for(i in 1:length(sample_reg_train[1,])){
  sample_reg_train1[is.na(sample_reg_train[,i]),i] <- col_means[i]
}

numbers_nas <- apply(sample_reg_train1[,as.logical(1-no_na)], MARGIN =2, function(x){sum(is.na(x))})
summary(numbers_nas)

#standardise:
sample_reg_train1 <- apply(sample_reg_train1, MARGIN =2, function(x){
                                                         if(sd(x) == 0){x}
                                                         else {
                                                         (x - mean(x))/(sd(x))} })

## Approach2 exclude variables that have any nas.
sample_reg_train2 <- sample_reg_train[,no_na]

#standardise:
sample_reg_train2 <- apply(sample_reg_train2, MARGIN =2, function(x){
                                                         if(sd(x) == 0){x}
                                                         else {
                                                         (x - mean(x))/(sd(x))} })

#2)## Ridge ###
lambdas <- 10^seq(3,-2,-0.01)

fit_ridge <- glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    family = "gaussian",alpha = 0,
                    lambda = lambdas)
cv_fit_ridge <- cv.glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          family = "gaussian",alpha = 0,
                          lambda = lambdas)
## Uses default 10 folds.
plot(cv_fit_ridge)

opt_lambda <- cv_fit_ridge$lambda.min
beta_ridge <- as.matrix(fit_ridge$beta[,match(opt_lambda,lambdas)])

# First look at the size of the coefficients:

summary(beta_ridge)

#10 biggest
beta_ridge_biggest <- sort(beta_ridge,decreasing = TRUE)[1:30]
beta_ridge_biggest_names <- rownames(beta_ridge)[match(beta_ridge_biggest,beta_ridge)]

####Looking at variables with largest coefficients comments:
#ftretention stands out as best predictor of grad rate
#some surprinsing variables with coefficients in top 10  = ft_faculty_salary and bach_deg_share_of_tot_deg


#10 smallest (i.e. largest negtive values)
beta_ridge_smallest <- sort(beta_ridge,decreasing = FALSE)[1:30]
beta_ridge_smallest_names <- rownames(beta_ridge)[match(beta_ridge_smallest,beta_ridge)]

###Looking at variables with smallest coefficients comments:
#fed_grant_pct and ptug_share_of_total_pt_enrl stand out as best predictors
#also picks ups part timers: ptug_share_of_total_pt_enrl is percentage of undergrads that are part time
#may also pick up negative effect of grad, assoc and prof degrees.

#3)## LASSO ###
#Try the same thing as for ridge but with LASSO

lambdas <- 10^seq(3,-2,-0.01)

fit_lasso <- glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    family = "gaussian",alpha = 1,
                    lambda = lambdas)
cv_fit_lasso <- cv.glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          family = "gaussian",alpha = 1,
                          lambda = lambdas)
## Uses default 10 folds.
plot(cv_fit_lasso)

opt_lambda <- cv_fit_lasso$lambda.min
beta_lasso <- as.matrix(fit_lasso$beta[,match(opt_lambda,lambdas)])

# First look at the size of the coefficients:

summary(beta_lasso)

#10 biggest
beta_lasso_biggest <- sort(beta_lasso,decreasing = TRUE)[1:30]
beta_lasso_biggest_names <- rownames(beta_lasso)[match(beta_lasso_biggest,beta_lasso)]

####Looking at variables with largest coefficients comments:


#10 smallest (i.e. largest negtive values)
beta_lasso_smallest <- sort(beta_lasso,decreasing = FALSE)[1:30]
beta_lasso_smallest_names <- rownames(beta_lasso)[match(beta_lasso_smallest,beta_lasso)]

###Looking at variables with smallest coefficients comments:


#4)## Elastic Net ###

lambdas <- 10^seq(3,-2,-0.01)

fit_elasticnet <- glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                    family = "gaussian",alpha = 0.5,
                    lambda = lambdas)
cv_fit_elasticnet <- cv.glmnet(x = sample_reg_train1[,-match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          y = sample_reg_train1[,match("grad_rate_150_p4yr",colnames(sample_reg_train1))],
                          family = "gaussian",alpha = 0.5,
                          lambda = lambdas)
## Uses default 10 folds.
plot(cv_fit_elasticnet)

opt_lambda <- cv_fit_elasticnet$lambda.min
beta_elasticnet <- as.matrix(fit_elasticnet$beta[,match(opt_lambda,lambdas)])

# First look at the size of the coefficients:

summary(beta_elasticnet)

#10 biggest
beta_elasticnet_biggest <- sort(beta_elasticnet,decreasing = TRUE)[1:30]
beta_elasticnet_biggest_names <- rownames(beta_elasticnet)[match(beta_elasticnet_biggest,beta_elasticnet)]

####Looking at variables with largest coefficients comments:


#10 smallest (i.e. largest negtive values)
beta_elasticnet_smallest <- sort(beta_elasticnet,decreasing = FALSE)[1:30]
beta_elasticnet_smallest_names <- rownames(beta_elasticnet)[match(beta_elasticnet_smallest,beta_elasticnet)]


###Looking at variables with smallest coefficients comments:

## Comparison gives broadly similar results over ridge,lasso and elastic net, with elastic net looking
# more similar to Lasso - suggest this is as both throw away the multicolinear terms?


#5)## Check for Lasso Robustness ###

lambdas <- 10^seq(3,-2,-0.01)

fit_lasso2 <- glmnet(x = sample_reg_train2[,-match("grad_rate_150_p4yr",colnames(sample_reg_train2))],
                    y = sample_reg_train2[,match("grad_rate_150_p4yr",colnames(sample_reg_train2))],
                    family = "gaussian",alpha = 1,
                    lambda = lambdas)
cv_fit_lasso2 <- cv.glmnet(x = sample_reg_train2[,-match("grad_rate_150_p4yr",colnames(sample_reg_train2))],
                          y = sample_reg_train2[,match("grad_rate_150_p4yr",colnames(sample_reg_train2))],
                          family = "gaussian",alpha = 1,
                          lambda = lambdas)
## Uses default 10 folds.
plot(cv_fit_lasso)

opt_lambda <- cv_fit_lasso2$lambda.min
beta_lasso2 <- as.matrix(fit_lasso2$beta[,match(opt_lambda,lambdas)])

# First look at the size of the coefficients:

summary(beta_lasso2)

#10 biggest
beta_lasso2_biggest <- sort(beta_lasso2,decreasing = TRUE)[1:30]
beta_lasso2_biggest_names <- rownames(beta_lasso2)[match(beta_lasso2_biggest,beta_lasso2)]

####Looking at variables with largest coefficients comments:


#10 smallest (i.e. largest negtive values)
beta_lasso2_smallest <- sort(beta_lasso2,decreasing = FALSE)[1:30]
beta_lasso2_smallest_names <- rownames(beta_lasso2)[match(beta_lasso2_smallest,beta_lasso)]

###Looking at variables with smallest coefficients comments:


#Comparison 

comparison <- data.frame(ridge_biggest = beta_ridge_biggest_names,
                         lasso_biggest = beta_lasso_biggest_names,
                         elasticnet_biggest = beta_elasticnet_biggest_names,
                         lasso2_biggest = beta_lasso2_biggest_names,
                         ridge_smallest =beta_ridge_smallest_names,
                         lasso_smallest = beta_lasso_smallest_names,
                         elasticnet_smallest = beta_elasticnet_smallest_names,
                         lasso2_smallest = beta_lasso2_smallest_names)

# Method 2 basically gives us completely different variables :(

#Look if any of the top5 for each method were excluded from method 2

exclusions2 <- colnames(sample_reg_train)[as.logical(1-no_na)]

num_exclusions_top10 <- apply(comparison[1:10,1:3],MARGIN =2, function(x){sum(x %in% exclusions2)})
num_exclusions_top5 <- apply(comparison[1:5,1:3],MARGIN =2, function(x){sum(x %in% exclusions2)})

# This is worrying, check to see if any of the top 5 in method 1 have high levels of missingness,
# and therefore can't be used.


cbind(as.character(comparison[1:10,1]),sapply(comparison[1:10,1],
                       function(x){sum(is.na(sample_reg_train[,match(x,colnames(sample_reg_train))]))}))

cbind(as.character(comparison[1:10,2]),sapply(comparison[1:10,2],
                                             function(x){sum(is.na(sample_reg_train[,match(x,colnames(sample_reg_train))]))}))

cbind(as.character(comparison[1:10,3]),sapply(comparison[1:10,3],
                                             function(x){sum(is.na(sample_reg_train[,match(x,colnames(sample_reg_train))]))}))
##Conclusions from this:
##fterententionrate almost best predictor - doesn't really tell us much about graduation rate. - but
##probably shouldn't use this anyhow due to missingness
## missingness not really a problem for other variables barring sat scores.

###Approach 3 
##remove variables that are obviously multicolinear, based on intuition and the above regularised regressions
##take hybrid between approaches 1 and 2, allow variables with missingness if it is less than say arbitrarily 200 observations out of 4000 = approx. 5%
##need to code discrete variables as factors so we don't give continous jumps and actually include discrete effects.

sample_reg_train3 <- sample_reg_train

###simply based on inspection from previous lasso model
covariates_drop <-  c("ftretention_rate", "grscohortpct","totaldegrees_100fte","zip")
sample_reg_train3 <- sample_reg_train[,-match(covariates_drop,colnames(sample_reg_train))]

#remove variables with more than 200 nas

numbers_nas <- apply(sample_reg_train3, MARGIN =2,
                     function(x){sum(is.na(x))})
sample_reg_train3 <- sample_reg_train3[,as.logical((1-(numbers_nas >=200)))]

# replace Nas with column means
col_means <- apply(as.matrix(sample_reg_train3), MARGIN =2, function(x){mean(x, na.rm = TRUE)})

for(i in 1:length(sample_reg_train3[1,])){
  sample_reg_train3[is.na(sample_reg_train3[,i]),i] <- col_means[i]
}

numbers_nas <- apply(sample_reg_train3, MARGIN =2, function(x){sum(is.na(x))})
summary(numbers_nas)

#define function to look for variables that should be coded as factors:
factor_search <- function(col){
  tmp <- sort(col,decreasing = TRUE)
  for(i in 1:(length(tmp)-1)){
    tmp[i] <- tmp[i] - tmp[i+1]
  }
  if(length(unique(tmp)) <50){return(1)}
  # Checks if there are few differences between the data points, 100 in the context of 4000 obsv not many
  tmp <- col
  if(length(unique(col)) <50){return(1)}
  # Checks if there are few different values.
return(0)
  }

factors <- apply(sample_reg_train3,2,factor_search)
possible_factors <- colnames(sample_reg_train3)[as.logical(factors)]
#captures what I think it should, though also captures percentages, index and scaling variables. 
#remove index, scaling and percentage variables from list

possible_factors <- possible_factors[-(grep("pct",possible_factors))]
possible_factors <- possible_factors[-(grep("index",possible_factors))]
possible_factors <- possible_factors[-(grep("scalar",possible_factors))]
#List seems more reasonable
#glmnet cannot handle factors so need to manually make all our factors into dummy variables
#to do this convert our matrix back into a dataframe use model.matrix then back to matrix

sample_reg_train3 <- as.data.frame(sample_reg_train3)

for(i in possible_factors){
  sample_reg_train3[,match(i,names(sample_reg_train3))] <- as.factor(sample_reg_train3[,match(i,names(sample_reg_train3))])
}

dim(sample_reg_train3)
#213 covariate
sample_reg_train3 <- model.matrix(data = as.matrix(sample_reg_train3), 
                                  object = colnames(sample_reg_train3))




# standardise

sample_reg_train3 <- apply(sample_reg_train3, MARGIN =2, function(x){
  if(sd(x) == 0){x}
  else {
    (x - mean(x))/(sd(x))} })

###Lassso###

lambdas <- 10^seq(3,-2,-0.01)

fit_lasso3 <- glmnet(x = sample_reg_train3[,-match("grad_rate_150_p4yr",colnames(sample_reg_train3))],
                    y = sample_reg_train3[,match("grad_rate_150_p4yr",colnames(sample_reg_train3))],
                    family = "gaussian",alpha = 1,
                    lambda = lambdas)
cv_fit_lasso3 <- cv.glmnet(x = sample_reg_train3[,-match("grad_rate_150_p4yr",colnames(sample_reg_train3))],
                          y = sample_reg_train3[,match("grad_rate_150_p4yr",colnames(sample_reg_train3))],
                          family = "gaussian",alpha = 1,
                          lambda = lambdas)
## Uses default 10 folds.
plot(cv_fit_lasso3)

opt_lambda <- cv_fit_lasso3$lambda.min
beta_lasso3 <- as.matrix(fit_lasso3$beta[,match(opt_lambda,lambdas)])

# First look at the size of the coefficients:

summary(beta_lasso3)

#10 biggest
beta_lasso3_biggest <- sort(beta_lasso3,decreasing = TRUE)[1:30]
beta_lasso3_biggest_names <- rownames(beta_lasso3)[match(beta_lasso3_biggest,beta_lasso3)]

####Looking at variables with largest coefficients comments:
#Interesting that faculty salary has big prediction value
#total completions_FTE should be removed
#bachelor degrees as share of total degrees also makes another appearance.

#10 smallest (i.e. largest negtive values)
beta_lasso3_smallest <- sort(beta_lasso3,decreasing = FALSE)[1:30]
beta_lasso3_smallest_names <- rownames(beta_lasso3)[match(beta_lasso3_smallest,beta_lasso3)]

###Looking at variables with smallest coefficients comments
#fed grant important,
#part time variables important, could be removed.










