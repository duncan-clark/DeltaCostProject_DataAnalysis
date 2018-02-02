#This script reads the data in the data file

setwd("data")
dat <- read.table("delta_public_release_00_15.csv",sep ="\t", fill = TRUE,header = TRUE)

