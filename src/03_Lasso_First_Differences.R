###This Script examines interactions and first differences with the variables selected by Lasso###
#setwd("C:/Users/Duncan/Documents/Academics/UCLA_Academics/Classes/Stats_201B/DeltaCostProject_DataAnalysis")

source("src/02_Variable_Selection.R")

sample_train_lassoed <- sample_allvar_train[,match(c("grad_rate_150_p4yr","census_division",
                                                     "academicyear","groupid","cpi_scalar_2015",
                                                     lasso_covariate_selection),
                                                   names(sample_allvar_train))]

#small worry - monetary values were not rescaled by inflation in the lasso variable selection, not
# sure if this will have a big effect or not, but I think we should be rescaling anyway so resale now.

monetary_cov <- c("ft_faculty_salary","tuitionfee03_tf","grant02")

inflation_transform(sample_train_lassoed,monetary_cov,"cpi")
covariates_drop <- c("groupid","cpi_scalar_2015")

#replace values with less than 200 nas with colmeans
col_means <- apply(sample_train_lassoed[,-tmp], MARGIN =2, function(x){mean(x, na.rm = TRUE)})

for(i in 1:length(sample_train_lassoed[1,-tmp])){
  sample_train_lassoed[,-tmp][is.na(sample_train_lassoed[,-tmp][,i]),i] <- col_means[i]
}

#transform factors to factors:
factors <- c("census_division","academicyear", "hbcu")
tmp <- which(colnames(sample_train_lassoed) %in% factors)

for(i in tmp){sample_train_lassoed[,i] <- as.factor(sample_train_lassoed[,i])}

#standardise
for(i in 1:length(sample_train_lassoed[1,])){
  x<- sample_train_lassoed[,i]
  if(is.factor(x)){x}
  else{if(sd(x)==0){x}
    else{ sample_train_lassoed[,i] <- scale(x,center = TRUE, scale= TRUE)}}
}

rm(tmp)


fit_gradrate_ls <- glm(data = sample_train_lassoed[,-match(covariates_drop,colnames(sample_train_lassoed))],
                       family="gaussian",grad_rate_150_p4yr ~ .*.)

m <- margins(fit_gradrate_ls)
#margins function takes a long time to run and errors out.

#Worry that with so many variables we are probably overfitting 31 choose 2 = 465
#many first order coefficients are no longer significant.

#Factor variables are dummy - we could look at average marginal effects, for each census division
#this may have a substantive interpretation.

sample_train_lassoed_census1 <- sample_train_lassoed[sample_train_lassoed$census_division ==1,]
sample_train_lassoed_census1 <- sample_train_lassoed_census1[,-match(covariates_drop,colnames(sample_train_lassoed))]
sample_train_lassoed_census1 <- sample_train_lassoed_census1[,-match("census_division",colnames(sample_train_lassoed_census1))]

if(length(unique(sample_train_lassoed_census1$hbcu))==1){
  sample_train_lassoed_census1 <- within(sample_train_lassoed_census1,rm(hbcu)) }


#need to not include interaction effects for academic year#

formula <- paste(colnames(sample_train_lassoed_census1[-match(c("academicyear","grad_rate_150_p4yr"),colnames(sample_train_lassoed_census1))]),collapse = "+")
formula <- paste("grad_rate_150_p4yr ~ (",formula,")^2","+academicyear")
formula <- as.formula(formula)

fit_gradrate_ls <- glm(data =sample_train_lassoed_census1,
                       family="gaussian",formula)

m<- margins(fit_gradrate_ls)

### This seems confusing and perhaps does not tell us all that much .... ?????













