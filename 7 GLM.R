library(data.table)

spending <- readRDS("5 combined.rds")

# extract those with positive spending in modelling (percentage of 0's = 9.8%)
spending_no0 <- spending[!spending$spend.adj == 0]

# add age and dm duration according to year
spending$age_yr <- spending$yr - 2006 + spending$age
spending$duration_yr <- spending$yr - 2006 + spending$duration
spending_yr_no0 <- spending[!spending$spend.adj == 0]

# glm modelling - for predicting modifiable spending (age and duration constant)
# modelling only based on those with positive spending
fm <- spend.adj ~ yr + female + age + duration + age*duration + alcohol + anemdef + arrhythmia + bldloss + chf + chrnlung + coag + depress + drug + htncx + htn + hypothy + liver + lymph + lytes + mets + neuro + obese + para + perivasc + psych + pulmcirc + renlfail + rheum + tumor + valve

glm_modifiable <- glm (fm, family = Gamma(link=log), data = spending_no0)

# glm modelling - for predicting spending (age and duration according to year)
# modelling only based on those with positive spending
fm_yr <- spend.adj ~ female + yr + age_yr + duration_yr + age_yr * duration_yr + alcohol + anemdef + arrhythmia + bldloss + chf + chrnlung + coag + depress + drug + htncx + htn + hypothy + liver + lymph + lytes + mets + neuro + obese + para + perivasc + psych + pulmcirc + renlfail + rheum + tumor + valve

glm_yr <- glm (fm_yr, family = Gamma(link=log), data = spending_yr_no0)

# predicting spending (modifiable or with age and duration according to year)
spending$predicted_modifiable <- predict(glm_modifiable, newdata = spending, type = "response")
spending$predicted_year <- predict(glm_yr, newdata = spending, type = "response")

# get spending in baseline and spending periods
s <- spending[, c("serial_no", "yr", "predicted_modifiable", "predicted_year")]
s <- reshape(data = s, timevar = "yr", idvar = "serial_no", direction = "wide")
s$bl.spend.mod <- s$predicted_modifiable.2007 + s$predicted_modifiable.2008
s$fn.spend.mod <- s$predicted_modifiable.2013 + s$predicted_modifiable.2014
s$diff.spend.mod <- s$fn.spend.mod - s$bl.spend.mod

s$bl.spend.yr <- s$predicted_year.2007 + s$predicted_year.2008
s$fn.spend.yr <- s$predicted_year.2013 + s$predicted_year.2014
s$diff.spend.yr <- s$fn.spend.yr - s$bl.spend.yr

saveRDS(s, "7 predicted spending.rds")
