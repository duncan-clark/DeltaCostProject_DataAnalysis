## This script perform lasso,ridge and elastic net on the training data in order to identify
## the important variables removing our pre concieved ideas on what variables might be useful

library(ProjectTemplate)
load.project()

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

### Ridge ###
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

### LASSO ###
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


### Elastic Net ###

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

comparison <- data.frame(ridge_biggest = beta_ridge_biggest_names,
                         lasso_biggest = beta_lasso_biggest_names,
                         elasticnet_biggest = beta_elasticnet_biggest_names,
                         ridge_smallest =beta_ridge_smallest_names,
                         lasso_smallest = beta_lasso_smallest_names,
                         elasticnet_smallest = beta_elasticnet_smallest_names)

## Comparison gives broadly similar results over ridge,lasso and elastic net, with elastic net looking
# more similar to Lasso - suggest this is as both throw away the multicolinear terms?




