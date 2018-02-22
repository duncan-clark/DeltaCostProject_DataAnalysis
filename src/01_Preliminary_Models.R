library(ProjectTemplate)
load.project()

##### SOME PRELIMINARY MODELS #####
### Regular Regression models on bachelors per 100 FTE and grad rate

covariates <-  c("grant01_cpi_fte","pct_black","pct_hisp","instruction01_cpi_fte","studserv01_cpi_fte")

### bachelors 100 FTE

fit_bach100fte_ls <- glm(data = sample_train, family="gaussian",
                         bachelordegrees_fte ~
                           grant01_cpi_fte +
                           pct_black +
                           pct_hisp +
                           instruction01_cpi_fte+
                           studserv01_cpi_fte
)
summary(fit_bach100fte_ls)

# Conclude that all these variables have significant effects
# Note that pct_hispanic has a positive coefficient despite status as underrepresented

### Grad Rate
fit_gradrate_ls <- glm(data = sample_train, family="gaussian", 
                       grad_rate_150_p4yr ~ 
                         grant01_cpi_fte +
                         pct_black +
                         pct_hisp +
                         instruction01_cpi_fte+
                         studserv01_cpi_fte
)

summary(fit_gradrate_ls) 


### Count models on number of bachelors
# Total bachelor degrees (Poisson Model)
fit_bach_pois <- glm(data = sample_train, family=poisson(link ="log"),
                     bachelordegrees ~ 
                       grant01_cpi_fte +
                       pct_black +
                       pct_hisp +
                       instruction01_cpi_fte+
                       studserv01_cpi_fte
)

summary(fit_bach_pois) 

#  Total bachelor degrees (Binomial) Model)
fit_gradrate_bin <- glm(data = sample_train, family="binomial",
                        bachelordegrees ~ 
                          grant01_cpi_fte +
                          pct_black +
                          pct_hisp +
                          instruction01_cpi_fte+
                          studserv01_cpi_fte
)

summary(fit_gradrate_bin)

### Now look at some first differences for effect of education spending variables:
# Grad Rate
interactions <- function(var,vars_interact){
  tmp <- var
  for(i in 1:length(vars_interact)){
    tmp <- paste(tmp," + ",var,"*",vars_interact[i],sep ="")
  }
  return(tmp)
}
# 
# 
# formula <- as.formula(paste(paste(collapse(covariates,sep=" + ")),
#                  interactions("instruction01_cpi_fte",covariates[-match("instruction01_cpi_fte",covariates)]),
#                   interactions("studserv01_cpi_fte",covariates[-match("studserv01_cpi_fte",covariates)]))))
# 
# fit_gradrate_ls2 <- glm(data = sample_train, family="gaussian", 
#                        grad_rate_150_p4yr ~ 
#                          grant01_cpi_fte +
#                          pct_black +
#                          pct_hisp +
#                          interactions("instruction01_cpi_fte",covariates[-match("instruction01_cpi_fte",covariates)])+
#                          interactions("studserv01_cpi_fte",covariates[-match("studserv01_cpi_fte",covariates)])
# )
# 
# fit_gradrate_ls3 <- glm(data = sample_train, family="gaussian", 
#                         grad_rate_150_p4yr ~ 
#                           grant01_cpi_fte +
#                           pct_black +
#                           pct_hisp +
#                           "instruction01_cpi_fte"+
#                           "studserv01_cpi_fte"+
#                           .*.
# )
# 
# 




