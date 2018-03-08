###This Script examines interactions and first differences with the variables selected by Lasso###
#setwd("C:/Users/Duncan/Documents/Academics/UCLA_Academics/Classes/Stats_201B/DeltaCostProject_DataAnalysis")

source("src/02_Variable_Selection.R")

#Test how the Lasso, Ridge and Elasticnet do on the test set with 
#sample_reg_train4 - full data set with obvious outcomes removed.

sample_train_lassoed <- sample_reg_train4[,c(grep("census_division",colnames(sample_reg_train4)),
                                             grep("academicyear",colnames(sample_reg_train4)),
                                             match(c("grad_rate_150_p4yr",
                                                lasso_covariate_selection),
                                                   colnames(sample_reg_train4)))]

sample_train_lassoed <- as.data.frame(sample_train_lassoed)

fit_gradrate_ls <- glm(data = sample_train_lassoed,
                       family="gaussian",grad_rate_150_p4yr ~ .*.)

#m <- margins(fit_gradrate_ls)
#margins function takes a long time to run and errors out.

#Worry that with so many variables we are probably overfitting 31 choose 2 = 465
#many first order coefficients are no longer significant.

#Factor variables are dummy - we could look at average marginal effects, for each census division
#this may have a substantive interpretation.

census_div_margins <- function(div,data){
  div_name <- paste("census_division",div,sep="")
  if(div != 1){
  sample <- data[(data[,match(div_name,names(data))]==1),]
  sample <- sample[,-grep("census_division",names(sample))]}
  else{
  sample <- data[rowSums(data[,grep("census_division",names(data))])==0,]
  sample <- sample[,-grep("census_division",names(sample))]}
  
  formula <- paste(names(sample[,-c(grep("academicyear",names(sample)),match("grad_rate_150_p4yr",names(sample)))]),collapse = "+")
  formula2 <- paste(names(sample[,grep("academicyear",names(sample))]),collapse = "+")
  formula <- paste("grad_rate_150_p4yr ~ (",formula,")^2 +",formula2)
  formula <- as.formula(formula)
  
  fit_gradrate_ls <- glm(data =sample,family="gaussian",formula)
  
  m<- margins(fit_gradrate_ls)
  
  print(summary(fit_gradrate_ls))
  print(summary(m))
}

#sapply(seq(1,9),census_div_margins,data=sample_train_lassoed) - takes a while to run

census_div_margins(1,sample_train_lassoed)

### This seems confusing and perhaps does not tell us all that much .... ?????

###Repeat but keep region fixed effects in and compare all at once:

formula <- paste(names(sample_train_lassoed[,-c(grep("census_division",names(sample_train_lassoed)),grep("academicyear",names(sample_train_lassoed)),match("grad_rate_150_p4yr",names(sample_train_lassoed)))]),collapse = "+")
formula2 <- paste(names(sample_train_lassoed[,grep("academicyear",names(sample_train_lassoed))]),collapse = "+")
formula3 <- paste(names(sample_train_lassoed[,grep("census_division",names(sample_train_lassoed))]),collapse = "+")

formula <- paste("grad_rate_150_p4yr ~ (",formula,")^2 +",formula2, "+",formula3)
formula <- as.formula(formula)

fit_gradrate_ls <- glm(data =sample_train_lassoed,family="gaussian",formula)

m<- margins(fit_gradrate_ls)

print(summary(fit_gradrate_ls))
print(summary(m))

#Margins here seem to be more what we expected - if the model fits the test data well, we 
#hope that our interpretations based on this model are somewhat valid.





