##### PREPARE DATA
### Clear Environment
rm(list=ls())

### Libraries
library(glmnet)
library(KRLS)
library(bigKRLS)

### Read in Data
load("cache/sample.RData")

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

### Prepare scaled training X and Y
# Regular X and Y
names(training)

X <- as.matrix(training[, 4:46])
Y1 <- as.matrix(training$grad_rate_150_p4yr)
Y2 <- as.matrix(training$bachelordegrees_fte)

# X with interaction terms
names(training)
X_Int <- training[, 4:46]
names(X_Int)

expense_var <- names(X_Int)[9:23]

for(e in expense_var[1:(length(expense_var)-1)]){
  for(var in expense_var[(which(expense_var==e)+1):length(expense_var)]){
    X_Int <- cbind(X_Int, X_Int[, e]*X_Int[, var])
    names(X_Int)[ncol(X_Int)] <- paste(e, "*", var)
  }
  
  rm(e)
  rm(var)
}

X_Int <- as.matrix(X_Int)



##### LM
### Grad Rate
# No interactions
GR_LM <- lm(Y1 ~ X)
summary(GR_LM)

save(GR_LM, file="cache/GR_LM.rda")

# Interactions
GR_LM_Int <- lm(Y1 ~ X_Int)
summary(GR_LM_Int)

save(GR_LM_Int, file="cache/GR_LM_Int.rda")

### Bachelor's degrees
# No interactions
Bach_LM <- lm(Y2 ~ X)
summary(Bach_LM)

save(Bach_LM, file="cache/Bach_LM.rda")

# Interactions
Bach_LM_Int <- lm(Y2 ~ X_Int)
summary(Bach_LM_Int)

save(Bach_LM_Int, file="cache/Bach_LM_Int.rda")



##### LASSO
### Grad-rate
# No interactions
set.seed(34238)
lambdas <- seq(10^-8, 10^-4, length.out = 10000)
cv_lasso_gr <- cv.glmnet(x = X,
                         y = Y1,
                         family = "gaussian",
                         alpha = 1,
                         standardize=T,
                         intercept=T,
                         lambda = lambdas)
which(round(lambdas, 8)==round(cv_lasso_gr$lambda.min, 8)) # try a smaller range

plot(cv_lasso_gr$lambda, cv_lasso_gr$cvm, type="l")
lambda_gr <- cv_lasso_gr$lambda.min

GR_Lasso <- glmnet(x=X, y=Y1, family="gaussian", alpha=1, standardize = T, intercept=T, lambda=lambda_gr)
GR_Lasso$beta

save(GR_Lasso, file="cache/GR_Lasso.rda")

rm(lambdas)
rm(cv_lasso_gr)
rm(lambda_gr)

# With interactions
set.seed(34238)
lambdas <- seq(10^-7, 10^-3, length.out = 10000)
cv_lasso_gr <- cv.glmnet(x = X_Int,
                         y = Y1,
                         family = "gaussian",
                         alpha = 1,
                         standardize=T,
                         intercept=T,
                         lambda = lambdas)
which(round(lambdas, 7)==round(cv_lasso_gr$lambda.min, 7)) # try a smaller range

plot(cv_lasso_gr$lambda, cv_lasso_gr$cvm, type="l")
lambda_gr <- cv_lasso_gr$lambda.min

GR_Lasso_Int <- glmnet(x=X_Int, y=Y1, family="gaussian", alpha=1, standardize = T, intercept=T, lambda=lambda_gr)
GR_Lasso_Int$beta

save(GR_Lasso_Int, file="cache/GR_Lasso_Int.rda")

rm(lambdas)
rm(cv_lasso_gr)
rm(lambda_gr)

### Bachelor's per FTE
# No interactions
set.seed(34238)
lambdas <- seq(10^-8, 10^-4, length.out = 10000)
cv_lasso_bach <- cv.glmnet(x = X,
                           y = Y2,
                           family = "gaussian",
                           alpha = 1,
                           standardize=T,
                           intercept=T,
                           lambda = lambdas)
which(round(lambdas, 8)==round(cv_lasso_bach$lambda.min, 8)) # try a smaller range

plot(cv_lasso_bach$lambda, cv_lasso_bach$cvm, type="l")
lambda_bach <- cv_lasso_bach$lambda.min

Bach_Lasso <- glmnet(x=X, y=Y2, family="gaussian", alpha=1, standardize = T, intercept=T, lambda=lambda_bach)
Bach_Lasso$beta

save(Bach_Lasso, file="cache/Bach_Lasso.rda")

rm(lambdas)
rm(cv_lasso_bach)
rm(lambda_bach)

# With interactions
set.seed(34238)
lambdas <- seq(10^-8, 10^-4, length.out = 10000)
cv_lasso_bach <- cv.glmnet(x = X_Int,
                           y = Y2,
                           family = "gaussian",
                           alpha = 1,
                           standardize=T,
                           intercept=T,
                           lambda = lambdas)
which(round(lambdas, 8)==round(cv_lasso_bach$lambda.min, 8)) # try a smaller range

plot(cv_lasso_bach$lambda, cv_lasso_bach$cvm, type="l")
lambda_bach <- cv_lasso_bach$lambda.min

Bach_Lasso_Int <- glmnet(x=X_Int, y=Y2, family="gaussian", alpha=1, standardize = T, intercept=T, lambda=lambda_bach)
Bach_Lasso_Int$beta

save(Bach_Lasso_Int, file="cache/Bach_Lasso_Int.rda")

rm(lambdas)
rm(cv_lasso_bach)
rm(lambda_bach)



##### RIDGE
### Grad-rate
# No interactions
set.seed(34238)
lambdas <- seq(10^-7, 10^-3, length.out = 10000)
cv_ridge_gr <- cv.glmnet(x = X,
                         y = Y1,
                         family = "gaussian",
                         alpha = 0,
                         standardize=T,
                         intercept=T,
                         lambda = lambdas)
which(round(lambdas, 7)==round(cv_ridge_gr$lambda.min, 7)) # try a smaller range

plot(cv_ridge_gr$lambda, cv_ridge_gr$cvm, type="l")
lambda_gr <- cv_ridge_gr$lambda.min

GR_Ridge <- glmnet(x=X, y=Y1, family="gaussian", alpha=0, standardize = T, intercept=T, lambda=lambda_gr)
GR_Ridge$beta

save(GR_Ridge, file="cache/GR_Ridge.rda")

rm(lambdas)
rm(cv_ridge_gr)
rm(lambda_gr)

# With interactions
set.seed(34238)
lambdas <- seq(10^-6, 10^-2, length.out = 10000)
cv_ridge_gr <- cv.glmnet(x = X_Int,
                         y = Y1,
                         family = "gaussian",
                         alpha = 0,
                         standardize=T,
                         intercept=T,
                         lambda = lambdas)
which(round(lambdas, 6)==round(cv_ridge_gr$lambda.min, 6)) # try a smaller range

plot(cv_ridge_gr$lambda, cv_ridge_gr$cvm, type="l")
lambda_gr <- cv_ridge_gr$lambda.min

GR_Ridge_Int <- glmnet(x=X_Int, y=Y1, family="gaussian", alpha=0, standardize = T, intercept=T, lambda=lambda_gr)
GR_Ridge_Int$beta

save(GR_Ridge_Int, file="cache/GR_Ridge_Int.rda")

rm(lambdas)
rm(cv_ridge_gr)
rm(lambda_gr)

### Bachelor's per FTE
# No interactions
set.seed(34238)
lambdas <- seq(10^-7, 10^-3, length.out = 10000)
cv_ridge_bach <- cv.glmnet(x = X,
                           y = Y2,
                           family = "gaussian",
                           alpha = 0,
                           standardize=T,
                           intercept=T,
                           lambda = lambdas)
which(round(lambdas, 7)==round(cv_ridge_bach$lambda.min, 7)) # try a smaller range

plot(cv_ridge_bach$lambda, cv_ridge_bach$cvm, type="l")
lambda_bach <- cv_ridge_bach$lambda.min

Bach_Ridge <- glmnet(x=X, y=Y2, family="gaussian", alpha=0, standardize = T, intercept=T, lambda=lambda_bach)
Bach_Ridge$beta

save(Bach_Ridge, file="cache/Bach_Ridge.rda")

rm(lambdas)
rm(cv_ridge_bach)
rm(lambda_bach)

# With interactions
set.seed(34238)
lambdas <- seq(10^-7, 10^-3, length.out = 10000)
cv_ridge_bach <- cv.glmnet(x = X_Int,
                           y = Y2,
                           family = "gaussian",
                           alpha = 0,
                           standardize=T,
                           intercept=T,
                           lambda = lambdas)
which(round(lambdas, 7)==round(cv_ridge_bach$lambda.min, 7)) # try a smaller range

plot(cv_ridge_bach$lambda, cv_ridge_bach$cvm, type="l")
lambda_bach <- cv_ridge_bach$lambda.min

Bach_Ridge_Int <- glmnet(x=X_Int, y=Y2, family="gaussian", alpha=0, standardize = T, intercept=T, lambda=lambda_bach)
Bach_Ridge_Int$beta

save(Bach_Ridge_Int, file="cache/Bach_Ridge_Int.rda")

rm(lambdas)
rm(cv_ridge_bach)
rm(lambda_bach)



##### KERNEL REGRESSION (GAUSSIAN KERNEL)
### Graduation Rate
# Run model
#GR_Ker <- bigKRLS(X=X, y=Y1)

# Save
#save(GR_Ker, file="cache/GR_Ker.rda")

# Load
load("cache/GR_Ker.rda")

### Bachelor's Degrees
# Run model
#Bach_Ker <- bigKRLS(X=X, y=Y2)

# Save
#save(Bach_Ker, file="cache/Bach_Ker.rda")

# Load
load("cache/Bach_Ker.rda")



##### SEE HOW EACH MODEL DOES ON THE TESTING SET
### Prepare Testing Set
Y1_Test <- testing[, 47] 
Y2_Test <- testing[, 48]

# Standardize X from testing Set
X_Test_Int <- testing[, 4:46]
for(e in expense_var[1:(length(expense_var)-1)]){
  for(var in expense_var[(which(expense_var==e)+1):length(expense_var)]){
    X_Test_Int <- cbind(X_Test_Int, X_Test_Int[, e]*X_Test_Int[, var])
    names(X_Test_Int)[ncol(X_Test_Int)] <- paste(e, "*", var)
  }
  
  rm(e)
  rm(var)
}
X_Test_Int <- as.matrix(X_Test_Int)

X_Test <- X_Test_Int[, 1:43]

# Prepare Kernel for the testing X
X_Training_Means <- t(as.matrix(apply(X, 2, mean)))
X_Training_SDs <- t(as.matrix(apply(X, 2, sd)))

sum(1-(colnames(X_Test)==colnames(X_Training_Means))) # 0
sum(1-(colnames(X_Test)==colnames(X_Training_SDs))) # 0

Ones <- as.matrix(rep(1, nrow(X_Test)))

X_Test_Standard <- X_Test
X_Test_Standard <- X_Test_Standard - (Ones %*% X_Training_Means)
X_Test_Standard <- X_Test_Standard / (Ones %*% X_Training_SDs)

rm(Ones)

X_Standard <- scale(X)

n <- nrow(X_Test)
N <- nrow(X)

K_Test <- matrix(rep(0, n*N), nrow=n)
for(i in 1:n){
  for(j in 1:N){
    test <- X_Test_Standard[i, ]
    train <- X_Standard[j, ]
    K_Test[i, j] <- exp(-(norm(test-train, type="2")^2)/ncol(X))
  }
  
  rm(i)
  rm(j)
  rm(test)
  rm(train)
}

rm(n)
rm(N)
rm(X_Test_Standard)
rm(X_Standard)

save(K_Test, file="cache/K_Test.rda")

### See the testing errors for each model
# Define outcomes and models
outcomes <- c("GR", "Bach")
models <- c("LM", "Lasso", "Ridge", "Ker")

# Get RMSE (scaled and unscaled) and testing R^2 for each model
sink("logs/test_data_results_lw.txt", type="output")
sink()

for(o in outcomes){
  # Log file
  sink("logs/test_data_results_lw.txt", type="output", append=T)
  
  # Print the outcome
  print(paste("---", o, "---", sep=""))
  # Get the right outcome
  y <- Y1_Test
  y_sd <- sd(training$grad_rate_150_p4yr)
  y_mean <- mean(training$grad_rate_150_p4yr)
  
  if(o=="Bach") y <- Y2_Test
  if(o=="Bach") y_sd <- sd(training$bachelordegrees_fte)
  if(o=="Bach") y_mean <- mean(training$bachelordegrees_fte)
  
  # Loop through the models
  for(m in models){
    # grab models
    model <- globalenv()[[paste(o, "_", m, sep="")]]
    if(m!="Ker") model_int <- globalenv()[[paste(o, "_", m, "_Int", sep="")]]
    
    # Get predicted values
    if(m=="LM")yhat <- cbind(1,X_Test) %*% model$coefficients
    if(m=="LM")yhat_int <- cbind(1, X_Test_Int) %*% model_int$coefficients
    if(m %in% c("Lasso", "Ridge"))yhat <- predict(model, newx=X_Test)
    if(m %in% c("Lasso", "Ridge"))yhat_int <- predict(model_int, newx=X_Test_Int)
    if(m=="Ker")yhat <- (K_Test %*% model$coeffs) * y_sd + y_mean

    # Get RMSE
    RMSE <- sqrt(mean((y - yhat)^2))
    if(m!="Ker") RMSE_int <- sqrt(mean((y - yhat_int)^2))
    
    # Get R2
    #R2 <- (sum((y - mean(y))^2) - sum((y - yhat)^2))/sum((y - mean(y))^2)
    #if(m!="Ker") R2_int <- (sum((y - mean(y))^2) - sum((y - yhat_int)^2))/sum((y - mean(y))^2)
    
    # Print Results
    print(m)
    print(paste("RMSE: ", round(RMSE, 4), "(", round(RMSE/y_sd, 4), " standardized)"))
    #print(paste("R^2: ", round(R2, 4)))
    if(m!="Ker") print(paste("RMSE (w/ Int.): ", round(RMSE_int, 4), "(", round(RMSE_int/y_sd, 4), " standardized)"))
    #if(m!="Ker") print(paste("R^2 (Interactions): ", round(R2_int, 4)))
    print("", quote=F)
  }
  
  # Stop log
  sink()
  
  # Clean environment
  rm(o)
  rm(m)
  rm(y)
  rm(y_sd)
  rm(y_mean)
  rm(model)
  rm(model_int)
  rm(yhat)
  rm(yhat_int)
  rm(RMSE)
  rm(RMSE_int)
} # Kernel Models do the best on both outcomes

### Check derivatives of the GR models
GR_Derivatives <- cbind(as.matrix(GR_LM$coefficients[-1]), as.matrix(GR_Lasso$beta))
GR_Derivatives <- cbind(GR_Derivatives, as.matrix(GR_Ridge$beta))
#GR_Derivatives <- cbind(GR_Derivatives, t(as.matrix(GR_Ker$avgderivatives)) * sd(Y1))
GR_Derivatives <- cbind(GR_Derivatives, t(as.matrix(GR_Ker$avgderivatives)))
GR_Derivatives <- round(GR_Derivatives, 7)
rownames(GR_Derivatives) <- colnames(X)
View(GR_Derivatives) # Signs of the Kernel model seem to make more intuitive sense (urm, pell grants, and instruction01)

rm(GR_Derivatives)



##### EXAMINE RESULTS
### Get Derivative Matrices
# Grad Rate
#GR_Derivatives <- cbind(t(as.matrix(GR_Ker$avgderivatives)) * sd(Y1), sqrt(t(as.matrix(GR_Ker$var.avgderivatives))) * sd(Y1))
GR_Derivatives <- cbind(t(as.matrix(GR_Ker$avgderivatives)), sqrt(t(as.matrix(GR_Ker$var.avgderivatives))))
GR_Derivatives <- data.frame(GR_Derivatives)
names(GR_Derivatives) <- c("avg", "se")
GR_Derivatives$p <- 2*(1-pnorm(abs(GR_Derivatives$avg / GR_Derivatives$se)))
GR_Derivatives <- round(GR_Derivatives, 7)
GR_Derivatives <- GR_Derivatives[expense_var, ]
GR_Derivatives <- GR_Derivatives[GR_Derivatives$p < .05, ]
View(GR_Derivatives) # most positive are student services and auxiliary (auxiliary02_cpi_fte most)
                     # most negative is instsupp02_cpi_fte

save(GR_Derivatives, file="cache/GR_Derivatives.rda")

# Bachelor's per FTE
#Bach_Derivatives <- cbind(t(as.matrix(Bach_Ker$avgderivatives)) * sd(Y2), sqrt(t(as.matrix(Bach_Ker$var.avgderivatives))) * sd(Y2))
Bach_Derivatives <- cbind(t(as.matrix(Bach_Ker$avgderivatives)), sqrt(t(as.matrix(Bach_Ker$var.avgderivatives))))
Bach_Derivatives <- data.frame(Bach_Derivatives)
names(Bach_Derivatives) <- c("avg", "se")
Bach_Derivatives$p <- 2*(1-pnorm(abs(Bach_Derivatives$avg / Bach_Derivatives$se)))
Bach_Derivatives <- round(Bach_Derivatives, 7)
Bach_Derivatives <- Bach_Derivatives[expense_var,]
Bach_Derivatives <- Bach_Derivatives[Bach_Derivatives$p < .05, ]
View(Bach_Derivatives) # Most positive are student services and auxiliary (studserv02_cpi_fte most)
                       # Most negative is pubserv02_cpi_fte

save(Bach_Derivatives, file="cache/Bach_Derivatives.rda")


##### KERNEL FIRST DIFFERENCES
### Create X Training Means, Sds, and Ones vector
X_Training_Means <- t(as.matrix(apply(X, 2, mean)))
X_Training_SDs <- t(as.matrix(apply(X, 2, sd)))
Ones <- as.matrix(rep(1, nrow(analysis_data)))

### Original Kernel Matrix
names(analysis_data)
Original <- as.matrix(analysis_data[4:46])

Original <- Original - (Ones %*% X_Training_Means)
Original <- Original / (Ones %*% X_Training_SDs)

# Create kernel
X_Standard <- scale(X)

n <- nrow(Original)
N <- nrow(X)

K_Original <- matrix(rep(0, n*N), nrow=n)
for(i in 1:n){
  for(j in 1:N){
    test <- Original[i, ]
    train <- X_Standard[j, ]
    K_Original[i, j] <- exp(-(norm(test-train, type="2")^2)/ncol(X))
  }
  
  rm(i)
  rm(j)
  rm(test)
  rm(train)
}

rm(n)
rm(N)
rm(X_Standard)

save(K_Original, file="cache/K_Original.rda")

### Graduation Rate Kernel Matrix
# Created altered matrix
names(analysis_data)
GR_Altered <- analysis_data[4:46]
GR_Altered$auxiliary02_cpi_fte <- GR_Altered$auxiliary02_cpi_fte + 1
GR_Altered$instsupp02_cpi_fte <- GR_Altered$instsupp02_cpi_fte - 1
GR_Altered <- as.matrix(GR_Altered)

GR_Altered <- GR_Altered - (Ones %*% X_Training_Means)
GR_Altered <- GR_Altered / (Ones %*% X_Training_SDs)

# Create kernel
X_Standard <- scale(X)

n <- nrow(GR_Altered)
N <- nrow(X)

K_GR <- matrix(rep(0, n*N), nrow=n)
for(i in 1:n){
  for(j in 1:N){
    test <- GR_Altered[i, ]
    train <- X_Standard[j, ]
    K_GR[i, j] <- exp(-(norm(test-train, type="2")^2)/ncol(X))
  }
  
  rm(i)
  rm(j)
  rm(test)
  rm(train)
}

rm(n)
rm(N)
rm(X_Standard)

save(K_GR, file="cache/K_GR.rda")

### Bachelor's Kernel Matrix
# Create altered matrix
names(analysis_data)
Bach_Altered <- analysis_data[4:46]
Bach_Altered$studserv02_cpi_fte <- Bach_Altered$studserv02_cpi_fte + 1
Bach_Altered$pubserv02_cpi_fte <- Bach_Altered$pubserv02_cpi_fte - 1
Bach_Altered <- as.matrix(Bach_Altered)

Bach_Altered <- Bach_Altered - (Ones %*% X_Training_Means)
Bach_Altered <- Bach_Altered / (Ones %*% X_Training_SDs)

# Create kernel
X_Standard <- scale(X)

n <- nrow(Bach_Altered)
N <- nrow(X)

K_Bach <- matrix(rep(0, n*N), nrow=n)
for(i in 1:n){
  for(j in 1:N){
    test <- Bach_Altered[i, ]
    train <- X_Standard[j, ]
    K_Bach[i, j] <- exp(-(norm(test-train, type="2")^2)/ncol(X))
  }
  
  rm(i)
  rm(j)
  rm(test)
  rm(train)
}

rm(n)
rm(N)
rm(X_Standard)

save(K_Bach, file="cache/K_Bach.rda")

### Graduation Rate First Difference
# Grad Rate Estimate
#GR_Ker_FD <- sd(Y1)*mean((GR_Altered %*% GR_Ker$coeffs) - (Original %*% GR_Ker$coeffs))
GR_Ker_FD <- mean((K_GR %*% GR_Ker$coeffs) - (K_Original %*% GR_Ker$coeffs))*sd(Y1)
GR_Ker_FD

save(GR_Ker_FD, file="cache/GR_Ker_FD.rda")

# Bachelor Estimate
#Bach_Ker_FD <- sd(Y2)*mean((Bach_Altered %*% Bach_Ker$coeffs) - (Original %*% Bach_Ker$coeffs))
Bach_Ker_FD <- mean((K_Bach %*% Bach_Ker$coeffs) - (K_Original %*% Bach_Ker$coeffs))*sd(Y2)
Bach_Ker_FD

save(Bach_Ker_FD, file="cache/Bach_Ker_FD.rda")

# Cluster Bootstrap
GR_Bootstrap_FD <- NULL
Bach_Bootstrap_FD <- NULL

set.seed(43289)
for(i in 1:50){
  # Print the step
  print(i)
  print("")
  
  # Get schools and length of unique schools
  schools <- unique(training$groupid)
  m <- length(schools)
  
  # Create new data and kernel matrix
  data <- NULL
  K_Original_B <- NULL
  K_GR_B <- NULL
  K_Bach_B <- NULL
  
  for(i in 1:m){
    # Get school
    s <- sample(schools, 1)
    
    # Data
    sub <- training[training$groupid==s, ]
    data <- rbind(data, sub)
    
    # Original
    sub <- K_Original[, training$groupid==s]
    K_Original_B <- cbind(K_Original_B, sub)

    # GR
    sub <- K_GR[, training$groupid==s]
    K_GR_B <- cbind(K_GR_B, sub)

    # Bachelor's
    sub <- K_Bach[, training$groupid==s]
    K_Bach_B <- cbind(K_Bach_B, sub)
  }
  
  # Get new x and y
  X_B <- as.matrix(data[, 4:46])
  Y1_B <- data$grad_rate_150_p4yr
  Y2_B <- data$bachelordegrees_fte
  
  # Get new coefficients
  GR_B_c <- bigKRLS(X = X_B, y = Y1_B, derivative = F, vcov.est = F, lambda=GR_Ker$lambda)$coeffs
  Bach_B_c <- bigKRLS(X = X_B, y = Y2_B, derivative = F, vcov.est = F, lambda=Bach_Ker$lambda)$coeffs
  
  # Get FD
  GR_B_FD <- mean((K_GR_B %*% GR_B_c) - (K_Original_B %*% GR_B_c))*sd(Y1_B)
  Bach_B_FD <- mean((K_GR_B %*% Bach_B_c) - (K_Original_B %*% Bach_B_c))*sd(Y2_B)
  
  # Add to vectors
  GR_Bootstrap_FD <- c(GR_Bootstrap_FD, GR_B_FD)
  Bach_Bootstrap_FD <- c(Bach_Bootstrap_FD, Bach_B_FD)
  
  # Clear environment
  rm(i)
  rm(schools)
  rm(m)
  rm(s)
  rm(data)
  rm(K_Original_B)
  rm(K_GR_B)
  rm(K_Bach_B)
  rm(sub)
  rm(X_B)
  rm(Y1_B)
  rm(Y2_B)
  rm(GR_B_c)
  rm(Bach_B_c)
  rm(GR_B_FD)
  rm(Bach_B_FD)
}

save(GR_Bootstrap_FD, file="cache/GR_Bootstrap_FD.rda")
save(Bach_Bootstrap_FD, file="cache/Bach_Bootstrap_FD.rda")
