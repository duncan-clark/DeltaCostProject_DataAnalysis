library(ProjectTemplate)
load.project()

##### SOME PRELIMINARY MODELS #####
### Regular Regression models on bachelors per 100 FTE and grad rate

### bachelors 100 FTE

fit_bach100fte_ls <- glm(data = sample_2003, family="gaussian",
                         bachelor_100fte ~
                           grant01_100fte +
                           pct_black +
                           pct_hisp +
                           instruction01_100fte+
                           studserv01_100fte
)
summary(fit_bach100fte_ls)

# Conclude that all these variables have significant effects
# Note that pct_hispanic has a positive coefficient despite status as underrepresented

### Grad Rate
fit_gradrate_ls <- glm(data = sample_2003, family="gaussian", 
                       grad_rate_150_p4yr ~ 
                         grant01_100fte +
                         pct_black +
                         pct_hisp +
                         instruction01_100fte+
                         studserv01_100fte
)

summary(fit_gradrate_ls) 


### Count models on bachelors and grad rate
# Total bachelor degrees (Poisson Model)
fit_bach_pois <- glm(data = sample_2003, family=poisson(link ="log"),
                     bachelordegrees ~ 
                       grant01_100fte +
                       pct_black +
                       pct_hisp +
                       instruction01_100fte+
                       studserv01_100fte
)

summary(fit_bach_pois) 

# Graduation Rate (binomial model)
fit_gradrate_bin <- glm(data = sample_2003, family="binomial",
                        grad_rate_150_p4yr ~ 
                          grant01_100fte +
                          pct_black +
                          pct_hisp +
                          instruction01_100fte+
                          studserv01_100fte
)

summary(fit_gradrate_bin)
