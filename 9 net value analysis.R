library(data.table); library(tidyr); library(dplyr)
load("8b predicted_mortality.Rdata")
df <- as.data.table(df)

# VALUE FOR RISKS

# we approximate the predicted probability of death in the next 5 years by giving one fifth of the predicted probability(0.2 Pjt) to each of the first 5 years, and assume that all patients surviving beyond year 5 (with probability 1- Pjt)have the same age- and sex-specific remaining life expectancy as a general individual from the lifetable of that population.

# LE baseline and final for various values of life ($50,000 $100,000, and $200,000)
# the following follows Brian's code

rate <- 0.03 #discount rate = 3%
df$le_int <- df$le - df$le %% 1
df$le_rem <- df$le %% 1

for (i in c(50, 100, 200)){
      assign(paste0("valyear", i, "k1"), i*1000*((1-(1/(1+rate)^1))/rate))
      assign(paste0("valyear", i, "k2"), i*1000*((1-(1/(1+rate)^2))/rate))
      assign(paste0("valyear", i, "k3"), i*1000*((1-(1/(1+rate)^3))/rate))
      assign(paste0("valyear", i, "k4"), i*1000*((1-(1/(1+rate)^4))/rate))
      assign(paste0("valyear", i, "k5"), i*1000*((1-(1/(1+rate)^5))/rate))
}

df$le_50k_bl <-
      ((df$baseline/5)*(valyear50k1 + valyear50k2 + valyear50k3 + valyear50k4 + valyear50k5)
       + (1-df$baseline)*
             (50000*((1-(1/(1+rate)^df$le_int))/rate) 
              + 50000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_50k_fn <-
      ((df$final/5)*(valyear50k1 + valyear50k2 + valyear50k3 + valyear50k4 + valyear50k5)
       + (1-df$final)*
             (50000*((1-(1/(1+rate)^df$le_int))/rate)
              + 50000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_100k_bl <-
      ((df$baseline/5)*(valyear100k1 + valyear100k2 + valyear100k3 + valyear100k4 + valyear100k5)
       + (1-df$baseline)*
             (100000*((1-(1/(1+rate)^df$le_int))/rate) 
              + 100000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_100k_fn <-
      ((df$final/5)*(valyear100k1 + valyear100k2 + valyear100k3 + valyear100k4 + valyear100k5)
       + (1-df$final)*
             (100000*((1-(1/(1+rate)^df$le_int))/rate) 
              + 100000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_200k_bl <-
      ((df$baseline/5)*(valyear200k1 + valyear200k2 + valyear200k3 + valyear200k4 + valyear200k5)
       + (1-df$baseline)*
             (200000*((1-(1/(1+rate)^df$le_int))/rate)
              + 200000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_200k_fn <-
      ((df$final/5)*(valyear200k1 + valyear200k2 + valyear200k3 + valyear200k4 + valyear200k5)
       + (1-df$final)*
             (200000*((1-(1/(1+rate)^df$le_int))/rate)
              + 200000*df$le_rem/(1+rate)^(df$le_int+1)))

# calculate difference between LE final and baseline

df$diff_50k <- df$le_50k_fn - df$le_50k_bl
df$diff_100k <- df$le_100k_fn - df$le_100k_bl
df$diff_200k <- df$le_200k_fn - df$le_200k_bl

# VALUE FOR SPENDING

# load required data
s <- readRDS("7 predicted spending.rds")
s <- s[s$serial_no %in% df$serial_no] # only survivors < 100 years old subset (n=303170)

# Convert from HKD to USD
s[, 2:25] <- s[, 2:25] / 7.84975

# combine difference in spending with difference in risk
nv <- merge(df[, c("serial_no", "female", "age", "diff_50k", "diff_100k", "diff_200k")], s[, c("serial_no", "diff.spend.mod")], by = "serial_no")
nv$nv_50k <- nv$diff_50k - nv$diff.spend.mod
nv$nv_100k <- nv$diff_100k - nv$diff.spend.mod
nv$nv_200k <- nv$diff_200k - nv$diff.spend.mod

saveRDS(nv, "9 method 1 results.rds")

########################################
## Calculations for Karen's result table ##

# Table 3, 4 and 5 - risk of mortality
# summarize overall by values of life
summary(df$diff_50k)
summary(df$diff_100k)
summary(df$diff_200k)

# by age
diff_age <- (df[, c("serial_no", "age", "diff_50k", "diff_100k", "diff_200k")])
diff_age %>% mutate(quintile = ntile(age, 5)) -> diff_age
age<-diff_age %>% group_by(quintile) %>% 
      summarize(mean_diff_50k = mean(diff_50k), mean_diff_100k = mean(diff_100k), mean_diff_200k = mean(diff_200k)) 
age

# by duration
diff_dur <- (df[, c("serial_no", "duration", "diff_50k", "diff_100k", "diff_200k")])
diff_dur %>% mutate(quintile = ntile(duration, 5)) -> diff_dur
duration <-diff_dur %>% group_by(quintile) %>% 
      summarize(mean_diff_50k = mean(diff_50k), mean_diff_100k = mean(diff_100k), mean_diff_200k = mean(diff_200k)) 
duration

# Table 2_spending
# Summary Statistics - Predicted Modifiable Spending
summary(s$bl.spend.mod); sd(s$bl.spend.mod)
summary(s$fn.spend.mod); sd(s$fn.spend.mod)
nrow(s) # no. of observations

summary(s$predicted_modifiable.2007); sd(s$predicted_modifiable.2007)
summary(s$predicted_modifiable.2008); sd(s$predicted_modifiable.2008)
summary(s$predicted_modifiable.2009); sd(s$predicted_modifiable.2009)
summary(s$predicted_modifiable.2010); sd(s$predicted_modifiable.2010)
summary(s$predicted_modifiable.2011); sd(s$predicted_modifiable.2011)
summary(s$predicted_modifiable.2012); sd(s$predicted_modifiable.2012)
summary(s$predicted_modifiable.2013); sd(s$predicted_modifiable.2013)
summary(s$predicted_modifiable.2014); sd(s$predicted_modifiable.2014)

# Summary Statistics - Predicted Spending
summary(s$bl.spend.yr); sd(s$bl.spend.yr)
summary(s$fn.spend.yr); sd(s$fn.spend.yr)
nrow(s) # no. of observations

summary(s$predicted_year.2007); sd(s$predicted_year.2007)
summary(s$predicted_year.2008); sd(s$predicted_year.2008)
summary(s$predicted_year.2009); sd(s$predicted_year.2009)
summary(s$predicted_year.2010); sd(s$predicted_year.2010)
summary(s$predicted_year.2011); sd(s$predicted_year.2011)
summary(s$predicted_year.2012); sd(s$predicted_year.2012)
summary(s$predicted_year.2013); sd(s$predicted_year.2013)
summary(s$predicted_year.2014); sd(s$predicted_year.2014)

# for Table3_Age
summary(s$bl.spend.mod); sd(s$bl.spend.mod)
summary(s$fn.spend.mod); sd(s$fn.spend.mod)

#by age
diff_age <- merge(diff_age[, c("serial_no", "quintile")], s[, c("serial_no", "bl.spend.mod", "fn.spend.mod", "diff.spend.mod")], by = "serial_no")

for (i in 1:5){
      assign(paste0("age", i), diff_age[diff_age$quintile == i,])
}

summary(age1); sd(age1$bl.spend.mod); sd(age1$fn.spend.mod)
summary(age2); sd(age2$bl.spend.mod); sd(age2$fn.spend.mod)
summary(age3); sd(age3$bl.spend.mod); sd(age3$fn.spend.mod)
summary(age4); sd(age4$bl.spend.mod); sd(age4$fn.spend.mod)
summary(age5); sd(age5$bl.spend.mod); sd(age5$fn.spend.mod)

# Table 4_Duration
#by duration
diff_dur <- merge(diff_dur[, c("serial_no", "quintile")], s[, c("serial_no", "bl.spend.mod", "fn.spend.mod", "diff.spend.mod")], by = "serial_no")

for (i in 1:5){
      assign(paste0("dur", i), diff_dur[diff_dur$quintile == i,])
}

summary(dur1); sd(dur1$bl.spend.mod); sd(dur1$fn.spend.mod)
summary(dur2); sd(dur2$bl.spend.mod); sd(dur2$fn.spend.mod)
summary(dur3); sd(dur3$bl.spend.mod); sd(dur3$fn.spend.mod)
summary(dur4); sd(dur4$bl.spend.mod); sd(dur4$fn.spend.mod)
summary(dur5); sd(dur5$bl.spend.mod); sd(dur5$fn.spend.mod)

# Table 5_NetValue
# Overall
summary(nv); lapply(nv[, 7:10], sd)

# by age quintile
diff_age <- merge(diff_age[, c("serial_no", "quintile")], nv, by = "serial_no")

for (i in 1:5){
      assign(paste0("age", i), diff_age[diff_age$quintile == i,])
}

summary(age1[, 8:11]); lapply(age1[, 8:11], sd)
summary(age2[, 8:11]); lapply(age2[, 8:11], sd)
summary(age3[, 8:11]); lapply(age3[, 8:11], sd)
summary(age4[, 8:11]); lapply(age4[, 8:11], sd)
summary(age5[, 8:11]); lapply(age5[, 8:11], sd)
