#This script checks how the models using automated variable selection do for prediction
#of the test set.

#Need to standardise, drop variables, and encode factors as factors, as we did for the
#training set to be able to make this comparison.
#note that we included variables baased on what was missing or no in the training data, so we just need
#to put all the variables in that were in the training data.

####################################################
###Processing - Copied from 02_Variable_Selection###
####################################################
source("src/02_Variable_Selection.R")

regularisation_processing <-  function(data,data_reg,data_orig){
#sample_allvar_test_proc <- sample_allvar_test
factors <- c("academicyear","isgrouped","ansi_code","oberegion","census_division","census_region",      
             "region_compact","carnegie2000","carnegiegrp_2000","carnegie_sector_2000","carnegie2005",
             "carnegiegrp_2005","carnegie_sector_2005","carnegie2010","carnegiegrp_2010",
             "carnegie_sector_2010","flagship","landgrnt","hbcu","hsi","medical","hospital","has_fte",
             "has_all","matched_n_87_15_29","matched_n_05_15_11","matched_n_10_15_6","instacttype")
#taken from lasso processing file
#drop variables that are not factors or not included in reg_train4 data
tmp <- match(unique(c(factors,colnames(data_reg))),names(data))
tmp <- na.omit(tmp)
data <- data[,tmp]

#impute missing data:
numbers_nas <- apply(data, MARGIN =2, function(x){sum(is.na(x))})

vars_impute <- match(names(data[,as.logical((1-(numbers_nas == 0)))]),names(data))

for(i in vars_impute){
  formula = as.formula(paste(names(data)[i],"~ academicyear + census_division",sep=""))
  model <- lm(formula,data=data)
  tmp <- is.na(data[,i])
  data[tmp,i] <- predict(model,newdata = data)[tmp]
}

tmp <-apply(data[,factors],MARGIN=2, function(x){length(unique(x))})
#Some factors only have one level - remove these from possible_factors
# Some factors have hundered of levels - remove these
single_factors <- which(tmp<=1)
many_factors <- which(tmp>=100)
rm(tmp)
factors <- factors[-c(single_factors,many_factors)]

for(i in factors){
  data[,match(i,names(data))] <- as.factor(data[,match(i,names(data))])
}

data <- model.matrix(~ . ,data = data)
data <- data[,match(colnames(data_reg),colnames(data))]

#Monetary variables located manually
tmp <- seq(match("tuition03",colnames(data)),match("bach_deg_share_of_tot_deg",colnames(data))-1,1)
tmp <- c(tmp,match("ft_faculty_salary",colnames(data)), match("salarytotal",colnames(data)))
#remove all that are <10 - this removes all "share of" covariates
tmp <- tmp[-which(apply(data[,tmp],2,mean)<10)]

#scale by 2015_scalar
data[,tmp] <- data[,tmp]*data_orig$cpi_scalar_2015

#all but ft_faculty salary are totals so should be per FTE for grad rate pruposes.
tmp <- tmp[-match(c("ft_faculty_salary","eandr_degree"),colnames(data))]
# one
data[,tmp] <- data[,tmp]/data[,match("fte_count",colnames(data))]

#standardise - for the numeric variables
#find columns with factors in them
factor_cols <- sapply(factors,FUN = grep,x = colnames(data))
factor_cols <- c(1,unlist(factor_cols)) # add intercept

#replace nas with 0s any such columns were included by mistake
data[is.na(data)] <- 0 

data[,-factor_cols] <-
  apply(data[,-factor_cols], MARGIN =2, function(x)
  {if(sd(x) ==0){x} else{scale(x,center = TRUE, scale= TRUE)}})

tmp <- apply(data, MARGIN =2, function(x){sum(is.na(x))})
sum(tmp)
rm(tmp)

return(data)}

sample_test_proc <- regularisation_processing(sample_allvar_test,sample_reg_train4,sample_allvar_test)
sample_test_proc <- as.data.frame(sample_test_proc)

###############
### Testing ###
###############

###Grad_Rate###
grad_rate_test <- data.frame("method" = rep(0,6),"mse_raw" = rep(0,6),
                             "mse" = rep(0,6),
                              "R2" = rep(0,6))
grad_rate_test$method <- c("LM","Ridge","Lasso","Elastic Net","LM with lassoed variables with interactions",
                           "Gaussian Kernel with lassoed variables")

Y <- sample_test_proc$grad_rate_150_p4yr
sd <- sd(sample_allvar_test$grad_rate_150_p4yr)
X <- as.matrix(sample_test_proc[,-match("grad_rate_150_p4yr",names(sample_test_proc))])


test_error_func <- function(Y,Y_hat){
  mse <- mean((Y-Y_hat)^2)
  r2 <- (sum((Y - mean(Y))^2) - sum((Y - Y_hat)^2)) / sum((Y - mean(Y))^2)
  return(list(mse= mse,r2=r2))}

#Regular Regression - with all variables:
model <- lm(data=as.data.frame(sample_reg_train4[,-1]),grad_rate_150_p4yr ~ .)
coef <- model$coefficients
coef[which(is.na(coef))] <- 0

Y_hat <- X%*%coef

grad_rate_test$mse[1] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[1] <-test_error_func(Y,Y_hat)$r2

rm(model)
rm(coef)

#Masssive overfitting as expected.

#Ridge_GR
Y_hat <- X%*%GR_beta_ridge4


grad_rate_test$mse[2] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[2] <-test_error_func(Y,Y_hat)$r2

#Lasso_GR
Y_hat <- X%*%GR_beta_lasso4


grad_rate_test$mse[3] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[3] <-test_error_func(Y,Y_hat)$r2

#Elasticnet_GR

Y_hat <- X%*%GR_beta_elasticnet4


grad_rate_test$mse[4] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[4] <-test_error_func(Y,Y_hat)$r2

#Lassoed_GR Variables with 2 way interactions
X_lassoed <- as.matrix(sample_test_proc[,match(colnames(GR_sample_train_lassoed),colnames(sample_test_proc))])
tmp <- match("grad_rate_150_p4yr",colnames(X_lassoed))
X_lassoed <- X_lassoed[,-tmp]
Y_hat <- predict(GR_fit_gradrate_ls,newdata = as.data.frame(X_lassoed))


grad_rate_test$mse[5] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[5] <-test_error_func(Y,Y_hat)$r2


#KRLS_GR with lassoed variables

Y_hat <- predict(GR_Ker_allvar,newdata = X_lassoed)$predicted

grad_rate_test$mse[6] <- test_error_func(Y,Y_hat)$mse
grad_rate_test$R2[6] <-test_error_func(Y,Y_hat)$r2

grad_rate_test$mse_raw = grad_rate_test$mse*sd

grad_rate_test$mse <- round(grad_rate_test$mse,4)
grad_rate_test$mse_raw <- round(grad_rate_test$mse_raw,4)







###Bachelors per FTE###

Bach_test <- data.frame("method" = rep(0,6),
                             "mse_raw" = rep(0,6),"mse" = rep(0,6), "R2" = rep(0,6))
Bach_test$method <- c("LM","Ridge","Lasso","Elastic Net","LM with lassoed variables with interactions",
                           "Gaussian Kernel with lassoed variables")

Y <- sample_allvar_test$bachelordegrees/sample_allvar_test$fte_count
sd <- sd(Y)
Y <- scale(Y,center = TRUE,scale = TRUE)
X <- as.matrix(sample_test_proc[,-match("grad_rate_150_p4yr",names(sample_test_proc))])


test_error_func <- function(Y,Y_hat){
  mse <- mean((Y-Y_hat)^2)
  r2 <- (sum((Y - mean(Y))^2) - sum((Y - Y_hat)^2)) / sum((Y - mean(Y))^2)
  return(list(mse= mse,r2=r2))}

#Regular Regression - with all variables:
tmp <- cbind(Y_Bach,sample_reg_train4[,c(-1,-match("grad_rate_150_p4yr",colnames(sample_reg_train4)))])
colnames(tmp)[1] ="Y_Bach"
tmp <- as.data.frame(tmp)
model <- lm(data=tmp, Y_Bach ~ .)
coef <- model$coefficients
coef[which(is.na(coef))] <- 0

Y_hat <- X%*%coef

Bach_test$mse[1] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[1] <- test_error_func(Y,Y_hat)$r2

rm(model)
rm(coef)
rm(tmp)

#Masssive overfitting as expected.

#Ridge_Bach
Y_hat <- X%*%Bach_beta_ridge4

Bach_test$mse[2] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[2] <- test_error_func(Y,Y_hat)$r2

#Lasso_Bach
Y_hat <- X%*%Bach_beta_lasso4

Bach_test$mse[3] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[3] <- test_error_func(Y,Y_hat)$r2

#Elasticnet_Bach
Y_hat <- X%*%Bach_beta_elasticnet4

Bach_test$mse[4] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[4] <- test_error_func(Y,Y_hat)$r2

#Lassoed_Bach Variables with 2 way interactions
X_lassoed <- as.matrix(sample_test_proc[,match(colnames(Bach_sample_train_lassoed)[-1],colnames(sample_test_proc))])

Y_hat <- predict(Bach_fit_ls,newdata = as.data.frame(X_lassoed))


Bach_test$mse[5] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[5] <- test_error_func(Y,Y_hat)$r2

## Still lots of overfitting

#KRLS_Bach with lassoed variables

Y_hat <- predict(Bach_Ker_allvar,newdata = X_lassoed)$predicted

Bach_test$mse[6] <- test_error_func(Y,Y_hat)$mse
Bach_test$R2[6] <- test_error_func(Y,Y_hat)$r2

Bach_test$mse_raw <- Bach_test$mse*sd

Bach_test$mse_raw <- round(Bach_test$mse_raw,4)
Bach_test$mse <- round(Bach_test$mse,4)













