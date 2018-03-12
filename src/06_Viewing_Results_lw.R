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



##### LOOK THROUGH RESULTS 
### Training vs. Testing tables
# Training Data
length(unique(training$groupid))
nrow(training)

# Testing Data
length(unique(testing$groupid))
nrow(testing)

# Full set
length(unique(analysis_data$groupid))
nrow(analysis_data)

### Names of variables in model
names(training)[4:40]

### Testing errors
# Check out log file

### Check out Derivatives
View(GR_Derivatives)
View(Bach_Derivates)

### Make first difference graphs and CIs
# Graduation Rate
GR_Ker_FD

CI_upper <- round(GR_Ker_FD + 1.96*sd(GR_Bootstrap_FD), 5)
CI_lower <- round(GR_Ker_FD - 1.96*sd(GR_Bootstrap_FD), 5)
CI_lower
CI_upper

pdf("reports/figures/GR_FD.pdf")
hist(GR_Bootstrap_FD, main="Graduation Rate (N=50)", xlab=paste("95% CI: (", CI_lower, ", ", CI_upper, ")", sep=""))
legend("topright", legend=c(paste("Estimate ", "(", round(GR_Ker_FD, 5), ")", sep="")), col=c("blue"), lty = c(2), cex=.8)
abline(v=GR_Ker_FD, col="blue", lty=2)
dev.off()

# Bachelor's Degrees
Bach_Ker_FD

CI_upper <- round(Bach_Ker_FD + 1.96*sd(Bach_Bootstrap_FD), 5)
CI_lower <- round(Bach_Ker_FD - 1.96*sd(Bach_Bootstrap_FD), 5)
CI_lower
CI_upper

pdf("reports/figures/Bach_FD.pdf")
hist(Bach_Bootstrap_FD, main="Bachelor's Degrees per FTE (N=50)", xlab=paste("95% CI: (", CI_lower, ", ", CI_upper, ")", sep=""))
legend("topright", legend=c(paste("Estimate ", "(", format(round(Bach_Ker_FD, 5), scientific=F), ")", sep="")), col=c("blue"), lty = c(2), cex=.8)
abline(v=Bach_Ker_FD, col="blue", lty=2)
dev.off()

