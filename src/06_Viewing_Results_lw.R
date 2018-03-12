##### LOAD RESULTS
### Clear Environment
rm(list=ls())

### Libraries
library(glmnet)
library(KRLS)
library(bigKRLS)

### Load Data
load("cache/analysis_data.Rda")
load("cache/training.Rda")
load("cache/testing.Rda")

### Load Kernel Models
load("cache/GR_Ker.Rda")
load("cache/Bach_Ker.Rda")

### Load Results
load("cache/GR_Derivatives.Rda")
load("cache/Bach_Derivatives.Rda")

load("cache/GR_Ker_FD.Rda")
load("cache/Bach_Ker_FD.Rda")

load("cache/GR_Bootstrap_FD.Rda")
load("cache/Bach_Bootstrap_FD.Rda")



##### Browse 
### Check out Derivatives
View(GR_Derivatives)