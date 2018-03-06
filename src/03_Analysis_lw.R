##### PREPARE DATA
### Clear Environment
rm(list=ls())

### Libraries
library(glmnet)
library(KRLS)
library(bigKRLS)

### Read in Data
load("cache/sample.RData")

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

### Create dummy variables 
# Years
yr_dummies <- model.matrix(~sample$academicyear)
yr_dummies <- data.frame(yr_dummies)[, -1]
names(yr_dummies) <- paste("yr_", as.character(2004:2015), sep="")

sample <- cbind(sample, yr_dummies)

rm(yr_dummies)

# Region
unique(sample$census_division)
region_dummies <- model.matrix(~sample$census_division)
region_dummies <- data.frame(region_dummies)[, -1]
names(region_dummies) <- paste("region_", as.character(2:9), sep="")

sample <- cbind(sample, region_dummies)

rm(region_dummies)

### Keep only variables we need from the sample dataset
names(sample)

vars_to_keep <- c(
  "groupid",
  "academicyear",
  "census_division",
  "unemployment",
  "gdp_per_capita_cpi",
  "total_undergraduates",
  "pct_urm",
  names(sample)[71:109],
  "grad_rate_150_p4yr",
  "bachelordegrees_fte"
)

analysis_data <- sample[, vars_to_keep]

rm(vars_to_keep)

### Split into training and testing
set.seed(201)

# Choose 20% of each region to go to the test set, rounding down to whole states.

split <- data.frame(groupid =unique(analysis_data$groupid),test=0)
blocks <- as.numeric(levels(as.factor(analysis_data$census_division)))

for(i in blocks){
  block <- unique(analysis_data$groupid[analysis_data$census_division == i])
  n_test <- round(length(unique(block))*0.2)
  n_train <- length(unique(block)) - n_test
  
  test <- sample(block,n_test)
  
  split$test[which((split$groupid %in% test)==TRUE)] <- 1
  
  rm(list=c("i", "block",  "n_test", "n_train", "test"))
}

training <- analysis_data[which((analysis_data$groupid %in% split$groupid[split$test==0]) == TRUE),]
testing <- analysis_data[which((analysis_data$groupid %in% split$groupid[split$test==1]) == TRUE),]

rm(blocks)
rm(split)



##### LASSO
### Standardize training set
names(training)

X <- scale(training[, 4:46])
Y1 <- scale(training$grad_rate_150_p4yr)
Y2 <- scale(training$bachelordegrees_fte)

### Add in interaction terms between all the expense variables
X_int <- data.frame(X)
names(X_int)

expense_var <- names(X_int)[9:23]

for(e in expense_var[1:(length(expense_var)-1)]){
  for(var in expense_var[(which(expense_var==e)+1):length(expense_var)]){
    X_int <- cbind(X_int, X_int[, e]*X_int[, var])
    names(X_int)[ncol(X_int)] <- paste(e, "*", var)
  }
  
  rm(e)
  rm(var)
}

X_int <- as.matrix(X_int)

### Lasso
# Grad-rate
set.seed(34238)
lambdas <- seq(10^-4, 1, length.out = 10000)
cv_lasso_gr <- cv.glmnet(x = X_int,
                          y = Y1,
                          family = "gaussian",
                          alpha = 1,
                          standardize=F,
                          lambda = lambdas)
which(round(lambdas, 4)==round(cv_lasso_gr$lambda.min, 4)) # try a smaller range

set.seed(34238)
lambdas <- seq(10^-5, 10^-1, length.out = 10000)
cv_lasso_gr <- cv.glmnet(x = X_int,
                         y = Y1,
                         family = "gaussian",
                         alpha = 1,
                         standardize=F,
                         lambda = lambdas)
which(lambdas==cv_lasso_gr$lambda.min) # Try smaller range

set.seed(34238)
lambdas <- seq(10^-6, 10^-2, length.out = 10000)
cv_lasso_gr <- cv.glmnet(x = X_int,
                         y = Y1,
                         family = "gaussian",
                         alpha = 1,
                         standardize=F,
                         lambda = lambdas)
which(lambdas==cv_lasso_gr$lambda.min) # Stop here

plot(cv_lasso_gr$lambda, cv_lasso_gr$cvm, type="l")
lambda_gr <- cv_lasso_gr$lambda.min

GR_Lasso <- glmnet(x=X_int, y=Y1, family="gaussian", alpha=1, standardize = F, lambda=lambda_gr)
GR_Lasso$beta # We'll try drop interaction terms that have coefficients of 0

save(GR_Lasso, file="cache/GR_Lasso.rda")

# Bachelor's per FTE
set.seed(34238)
lambdas <- seq(10^-4, 1, length.out = 10000)
cv_lasso_bach <- cv.glmnet(x = X_int,
                         y = Y2,
                         family = "gaussian",
                         alpha = 1,
                         standardize=F,
                         lambda = lambdas)
which(round(lambdas, 4)==round(cv_lasso_bach$lambda.min, 4)) # try a smaller range

set.seed(34238)
lambdas <- seq(10^-5, 10^-1, length.out = 10000)
cv_lasso_bach <- cv.glmnet(x = X_int,
                           y = Y2,
                           family = "gaussian",
                           alpha = 1,
                           standardize=F,
                           lambda = lambdas)
which(lambdas==cv_lasso_bach$lambda.min) # try a smaller range

set.seed(34238)
lambdas <- seq(10^-6, 10^-2, length.out = 10000)
cv_lasso_bach <- cv.glmnet(x = X_int,
                           y = Y2,
                           family = "gaussian",
                           alpha = 1,
                           standardize=F,
                           lambda = lambdas)
which(lambdas==cv_lasso_bach$lambda.min) # Stick with this

plot(cv_lasso_bach$lambda, cv_lasso_bach$cvm, type="l")
lambda_bach <- cv_lasso_bach$lambda.min

Bach_Lasso <- glmnet(x=X_int, y=Y2, family="gaussian", alpha=1, standardize = F, lambda=lambda_bach)
Bach_Lasso$beta

save(Bach_Lasso, file="cache/Bach_Lasso.rda")



##### RUN REGULAR AND RIDGE REGRESSION WITH INTERACTION TERMS 
#####   AFTER DROPPING INTERACTION TERMS WITH 0 COEFFICIENTS FROM LASSO
### Remove interaction terms from data that had coefficients of 0
# Get lists of int terms that had 0 coefficients from each model
colnames(X_int)
sum(1-(colnames(X_int)==rownames(GR_Lasso$beta))) # 0

int_terms <- as.vector(colnames(X_int)[44:ncol(X_int)])

gr_int0 <- intersect(colnames(X_int)[as.vector(GR_Lasso$beta==0)], int_terms)
bach_int0 <- intersect(colnames(X_int)[as.vector(Bach_Lasso$beta==0)], int_terms)

# Remove those terms from the data
X_GR <- X_int
X_GR <- X_GR[, -match(gr_int0, colnames(X_GR))]
ncol(X_GR)

X_Bach <- X_int
X_Bach <- X_Bach[, -match(bach_int0, colnames(X_Bach))]
ncol(X_Bach)

### Graduation Rate
# Regular LM
GR_LM <- lm(Y1~X_GR+0)
GR_LM$coefficients

# Ridge
set.seed(34238)
lambdas <- seq(10^-9, 10^-5, length.out = 10000)
cv_ridge_gr <- cv.glmnet(x = X_GR,
                           y = Y1,
                           family = "gaussian",
                           alpha = 0,
                           standardize=F,
                           lambda = lambdas) 
which(lambdas==cv_ridge_gr$lambda.min) # Stick with this
plot(cv_ridge_gr$lambda, cv_ridge_gr$cvm, type="l")
lambda_gr <- cv_ridge_gr$lambda.min

GR_Ridge <- glmnet(X_GR, Y1, family="gaussian", lambda=lambda_gr, alpha=0, standardize=F)
GR_Ridge$beta

# Check differences
summary(GR_LM$coefficients)
summary(as.vector(GR_Ridge$beta))

round(abs(GR_LM$coefficients - GR_Ridge$beta) / abs(GR_LM$coefficients), 4)

# Save models
save(GR_LM, file="cache/GR_LM.rda")
save(GR_Ridge, file="cache/GR_Ridge.rda")

### Bachelor's Degrees
# Regular LM
Bach_LM <- lm(Y2~X_Bach+0)
Bach_LM$coefficients

# Ridge
set.seed(34238)
lambdas <- seq(10^-5, 10^-1, length.out = 10000)
cv_ridge_bach <- cv.glmnet(x = X_Bach,
                         y = Y2,
                         family = "gaussian",
                         alpha = 0,
                         standardize=F,
                         lambda = lambdas) 
which(lambdas==cv_ridge_bach$lambda.min) # Stick with this
plot(cv_ridge_bach$lambda, cv_ridge_bach$cvm, type="l")
lambda_bach <- cv_ridge_bach$lambda.min

Bach_Ridge <- glmnet(X_Bach, Y2, family="gaussian", lambda=lambda_bach, alpha=0, standardize=F)
Bach_Ridge$beta

# Check differences
summary(Bach_LM$coefficients)
summary(as.vector(Bach_Ridge$beta))

round(abs(Bach_LM$coefficients - Bach_Ridge$beta) / abs(Bach_LM$coefficients), 4)

# Save models
save(Bach_LM, file="cache/Bach_LM.rda")
save(Bach_Ridge, file="cache/Bach_Ridge.rda")



##### RUN KERNEL REGRESSION ON DATA
### Graduation Rate
# Run model
GR_Ker <- bigKRLS(X=X, y=Y1)

# Save
save(GR_Ker, file="cache/GR_Ker.rda")

rm(temp)

### Bachelor's Degrees
# Run model
Bach_Ker <- bigKRLS(X=X, y=Y2)

# Save
save(Bach_Ker, file="cache/Bach_Ker.rda")

rm(temp)



##### SEE HOW EACH MODEL DOES ON THE TESTING SET
### Get SD and Mean from Training set
names(training)
temp <- as.matrix(training[, 4:48])

Training_Means <- t(as.matrix(apply(temp, 2, mean)))
Training_SDs <- t(as.matrix(apply(temp, 2, sd)))

rm(temp)

### Standardize X and Y from testing Set
Test_Standard <- as.matrix(testing[, 4:48])
Ones <- as.matrix(rep(1, nrow(Test_Standard)))

Test_Standard <- Test_Standard - (Ones %*% Training_Means)
Test_Standard <- Test_Standard / (Ones %*% Training_SDs)

rm(Ones)

### Add correct interaction terms to Testing X
# Extract X and Y
colnames(Test_Standard)

X_Test <- Test_Standard[, 1:43]
Y1_Test <- Test_Standard[, 44]
Y2_Test <- Test_Standard[, 45]

# Add interaction Terms
X_Test <- data.frame(X_Test)
expense_var

for(e in expense_var[1:(length(expense_var)-1)]){
  for(var in expense_var[(which(expense_var==e)+1):length(expense_var)]){
    X_Test <- cbind(X_Test, X_Test[, e]*X_Test[, var])
    names(X_Test)[ncol(X_Test)] <- paste(e, "*", var)
  }
  
  rm(e)
  rm(var)
}

X_Test <- as.matrix(X_Test)

X_Test_GR <- X_Test[, -match(gr_int0, colnames(X_Test))]
X_Test_Bach <- X_Test[, -match(bach_int0, colnames(X_Test))]

### See the testing errors for each model
# Grad Rate - Lasso
dim(X_Test)
length(GR_Lasso$beta)

sum(1-(colnames(X_Test)==rownames(GR_Lasso$beta)))

mean((Y1_Test - (X_Test %*% GR_Lasso$beta))^2) # 0.9256324
(sum((Y1_Test - mean(Y1_Test))^2) - sum((Y1_Test - (X_Test %*% GR_Lasso$beta))^2)) / sum((Y1_Test - mean(Y1_Test))^2) # 0.1992744

# Grad Rate - LM and Ridge
dim(X_Test_GR)
length(GR_LM$coefficients)
length(GR_Ridge$beta)

sum(1-(paste("X_GR", colnames(X_Test_GR), sep="")==names(GR_LM$coefficients)))
sum(1-(colnames(X_Test_GR)==rownames(GR_Ridge$beta)))

mean((Y1_Test - (X_Test_GR %*% GR_LM$coefficients))^2) # 0.6053765
(sum((Y1_Test - mean(Y1_Test))^2) - sum((Y1_Test - (X_Test_GR %*% GR_LM$coefficients))^2)) / sum((Y1_Test - mean(Y1_Test))^2) # 0.4763143

mean((Y1_Test - (X_Test_GR %*% GR_Ridge$beta))^2) # 0.9644693
(sum((Y1_Test - mean(Y1_Test))^2) - sum((Y1_Test - (X_Test_GR %*% GR_Ridge$beta))^2)) / sum((Y1_Test - mean(Y1_Test))^2) # 0.1656782

# Grad Rate - Kernel
X_Test_noint <- Test_Standard[, 1:43]

mean((Y1_Test - predict(GR_Ker, X_Test_noint)$predicted)^2) 
(sum((Y1_Test - mean(Y1_Test))^2) - sum((Y1_Test - predict(GR_Ker, X_Test_noint)$predicted)^2)) / sum((Y1_Test - mean(Y1_Test))^2) 

# Bach - Lasso
dim(X_Test)
length(Bach_Lasso$beta)

sum(1-(colnames(X_Test)==rownames(Bach_Lasso$beta)))

mean((Y2_Test - (X_Test %*% Bach_Lasso$beta))^2) # 1.161551
(sum((Y2_Test - mean(Y2_Test))^2) - sum((Y2_Test - (X_Test %*% Bach_Lasso$beta))^2)) / sum((Y2_Test - mean(Y2_Test))^2) # -0.3539167

# Bach - LM and Ridge
dim(X_Test_Bach)
length(Bach_LM$coefficients)
length(Bach_Ridge$beta)

sum(1-(paste("X_Bach", colnames(X_Test_Bach), sep="")==names(Bach_LM$coefficients)))
sum(1-(colnames(X_Test_Bach)==rownames(Bach_Ridge$beta)))

mean((Y2_Test - (X_Test_Bach %*% Bach_LM$coefficients))^2) # 1.222239
(sum((Y2_Test - mean(Y2_Test))^2) - sum((Y2_Test - (X_Test_Bach %*% Bach_LM$coefficients))^2)) / sum((Y2_Test - mean(Y2_Test))^2) # -0.4246554

mean((Y2_Test - (X_Test_Bach %*% Bach_Ridge$beta))^2) # 1.209067
(sum((Y2_Test - mean(Y2_Test))^2) - sum((Y2_Test - (X_Test_Bach %*% Bach_Ridge$beta))^2)) / sum((Y2_Test - mean(Y2_Test))^2) # -0.4093017

# Bach - Kernel
X_Test_noint <- Test_Standard[, 1:43]

mean((Y2_Test - predict(Bach_Ker, X_Test_noint)$predicted)^2) 
(sum((Y2_Test - mean(Y2_Test))^2) - sum((Y2_Test - predict(Bach_Ker, X_Test_noint)$predicted)^2)) / sum((Y2_Test - mean(Y2_Test))^2) 
