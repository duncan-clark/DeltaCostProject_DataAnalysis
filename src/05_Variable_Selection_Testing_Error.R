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
tmp <- seq(match("tuition03",colnames(data)),match("bachelordegrees",colnames(data))-1,1)
tmp <- c(tmp,match("ft_faculty_salary",colnames(data)), match("salarytotal",colnames(data)))
#remove all that are <10 - this removes all "share of" covariates
tmp <- tmp[-which(apply(data[,tmp],2,mean)<10)]

#scale by 2015_scalar
data[,tmp] <- data[,tmp]*data_orig$cpi_scalar_2015

#all but ft_faculty salary are totals so should be per FTE for grad rate pruposes.
tmp <- tmp[-match("ft_faculty_salary",colnames(data))]
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
Y <- sample_test_proc$grad_rate_150_p4yr
X <- as.matrix(sample_test_proc[,-match("grad_rate_150_p4yr",names(sample_test_proc))])


test_error_func <- function(Y,Y_hat){
  mse <- mean((Y-Y_hat)^2)
  r2 <- (sum((Y - mean(Y))^2) - sum((Y - Y_hat)^2)) / sum((Y - mean(Y))^2)
  return(list(mse= mse,r2=r2))}

#Ridge
Y_hat <- X%*%beta_ridge4

test_error_func(Y,Y_hat)$mse
test_error_func(Y,Y_hat)$r2

#Lasso
Y_hat <- X%*%beta_lasso4

test_error_func(Y,Y_hat)$mse
test_error_func(Y,Y_hat)$r2

#Elasticnet

Y_hat <- X%*%beta_elasticnet4

test_error_func(Y,Y_hat)$mse
test_error_func(Y,Y_hat)$r2

#Lassoed Variables with 2 way interactions

Y_hat <- predict(fit_gradrate_ls,newdata = as.data.frame(X))

test_error_func(Y,Y_hat)$mse
test_error_func(Y,Y_hat)$r2

#KRLS with lassoed variables





