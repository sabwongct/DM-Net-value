# Method 3: age-adjusted cross-section (survivors + incident cohorts + decedents) (P.4)

setwd("C:/Users/janet/Documents/Codes/DM Project")
library(Hmisc); library(dplyr); library(data.table)

# For year t=baseline (2007-2008), calculate the following: ---------

# (1) For those who survive the year (to the end of year t), calculate expected remaining life-years for each individual, based on risk prediction model using biomarkers measured in year t. The predicted remaining life expectancy (LE) is calculated in the same way as for the survivor panel, method 1, but for each individual use their own actual age and duration of diagnosis to predict risk of death and, based on that, their remaining LE.

mylist <- readRDS("pt_clinicalvalues.rds")
load("8a MI_clinicalvalues_0711.Rdata")
rm(imp_bl, imp_fn)
baseline_imputed <- as.data.table(baseline_imputed)
final_imputed <- as.data.table(final_imputed)

load("6 participant status.Rdata")

# using model equation for HKU-SG model eq
HKU_SG_mortality <- function(input) {
  100*(1-0.912^exp(2.727159
                   +0.02659452*as.numeric(input$age)
                   +4.075628e-5*(as.numeric(input$age) - 41)^3
                   -0.0001070358*(as.numeric(input$age) - 58)^3
                   +7.311264e-5*(as.numeric(input$age) - 70)^3
                   -6.833147e-6*(as.numeric(input$age) - 85)^3
                   +0.2650322*as.numeric(input$duration)
                   -0.01608406*(as.numeric(input$duration) - 0.04654346)^3
                   +0.01883374*(as.numeric(input$duration) - 0.9609856)^3 
                   -0.00277583*(as.numeric(input$duration) - 6.466804)^3
                   +2.614735e-5*(as.numeric(input$duration) - 22.96235)^3
                   -0.1983312*(as.numeric(input$female==1))
                   -0.3118533*(as.numeric(input$smoking==1)) # ex-smoker
                   -0.6109742*(as.numeric(input$smoking==2)) # non-smoker
                   +0.5252391*(as.numeric(input$af==1))
                   +1.077321*(as.numeric(input$ckd==1))
                   +0.4913603*(as.numeric(input$stroke==1))
                   +0.2324324*(as.numeric(input$chd==1))
                   -0.3320009*as.numeric(input$hba1c)
                   +0.06135776*(as.numeric(input$hba1c) - 5.6)^3
                   -0.1198288*(as.numeric(input$hba1c) - 6.6)^3
                   +0.05774934*(as.numeric(input$hba1c) - 7.6)^3
                   +0.0007216831*(as.numeric(input$hba1c) - 11.6)^3
                   -0.006923551*as.numeric(input$sbp)
                   +3.548158e-6*(as.numeric(input$sbp) - 108)^3
                   -8.185037e-6*(as.numeric(input$sbp) - 130)^3
                   +4.343557e-6*(as.numeric(input$sbp) - 145)^3
                   +2.93321e-7*(as.numeric(input$sbp) - 174)^3
                   -0.00510383*as.numeric(input$dbp)
                   +8.585339e-6*(as.numeric(input$dbp) - 58)^3
                   -1.604159e-5*(as.numeric(input$dbp) - 71)^3
                   +4.674797e-6*(as.numeric(input$dbp) - 80)^3
                   +2.781449e-6*(as.numeric(input$dbp) - 96)^3
                   -0.1802774*as.numeric(input$ldl)
                   +0.03426755*(as.numeric(input$ldl) - 1.62)^3
                   -0.06139979*(as.numeric(input$ldl) - 2.6606)^3
                   +0.01499461*(as.numeric(input$ldl) - 3.3636)^3
                   +0.01213762*(as.numeric(input$ldl) - 4.73)^3
                   -0.0506029*as.numeric(input$bmi)
                   +0.0003252084*(as.numeric(input$bmi) - 19.7)^3
                   -0.0004954199*(as.numeric(input$bmi) - 23.95)^3
                   +2.750309e-5*(as.numeric(input$bmi) - 26.83)^3
                   +0.0001427083*(as.numeric(input$bmi) - 33.08)^3))
}

# select survivors + decedents who enter within the baseline period (2007-08) and are still alive by the end of baseline period (before 2009-01-01)
bl <- baseline_imputed[serial_no %in% survivor_no | serial_no %in% decedent_no]
bl <- bl[!(death.date < "2009-01-01") | is.na(death.date)] #369600 left

# for each individual, use their own actual age and duration of DM
# for baseline, calculate age at entry year 
bl$entry.year <- as.numeric(substr(bl$entry.date,1,4))
bl$age <- bl$entry.year - bl$dob

# predict risk
r <- data.frame(serial_no = bl$serial_no, bl_risk = HKU_SG_mortality(bl))

# calculate remaining life years
# lifetable at 2006
male_lifetable <- data.frame(age = seq(1:100), le=c(78.53, 77.56, 76.58, 75.60, 74.61, 73.63, 72.64, 71.65, 70.65, 69.66, 68.67, 67.68, 66.68, 65.69, 64.70, 63.71, 62.72, 61.73, 60.74, 59.76, 58.78, 57.80, 56.82, 55.85, 54.87, 53.90, 52.94, 51.97, 51.00, 50.04, 49.07, 48.10, 47.14, 46.17, 45.20, 44.24, 43.28, 42.32, 41.36, 40.41, 39.46, 38.51, 37.56, 36.61, 35.67, 34.73, 33.79, 32.86, 31.93, 31.01, 30.10, 29.20, 28.30, 27.41, 26.52, 25.65, 24.78, 23.92, 23.07, 22.23, 21.40, 20.59, 19.79, 18.99, 18.21, 17.44, 16.67, 15.92, 15.19, 14.47, 13.77, 13.09, 12.42, 11.78, 11.15, 10.55, 9.96, 9.40, 8.86, 8.34, 7.84, 7.37, 6.92, 6.49, 6.09, 5.70, 5.33, 4.98, 4.65, 4.34, 4.05, 3.77, 3.51, 3.27, 3.04, 2.83, 2.63, 2.44, 2.27, 2.11))
female_lifetable <- data.frame(seq =seq(1:100), le = c(84.69, 83.71, 82.72, 81.73, 80.74, 79.75, 78.76, 77.77, 76.77, 75.78, 74.78, 73.79, 72.80, 71.81, 70.81, 69.82, 68.83, 67.84, 66.85, 65.86, 64.86, 63.87, 62.88, 61.89, 60.90, 59.91, 58.93, 57.94, 56.96, 55.97, 54.99, 54.01, 53.03, 52.05, 51.07, 50.09, 49.12, 48.14, 47.17, 46.20, 45.23, 44.26, 43.29, 42.33, 41.37, 40.40, 39.45, 38.49, 37.54, 36.60, 35.66, 34.72, 33.79, 32.86, 31.93, 31.01, 30.09, 29.17, 28.25, 27.35, 26.44, 25.55, 24.65, 23.77, 22.89, 22.01, 21.14, 20.28, 19.44, 18.60, 17.78, 16.98, 16.20, 15.43, 14.68, 13.94, 13.23, 12.54, 11.87, 11.23, 10.60, 10.01, 9.43, 8.88, 8.35, 7.84, 7.35, 6.88, 6.44, 6.01, 5.61, 5.22, 4.86, 4.52, 4.19, 3.89, 3.60, 3.33, 3.08, 2.85))
le <- mylist[[1]][c("serial_no", "female", "age")]
le$le <- ifelse(le$female==TRUE, female_lifetable$le[le$age], male_lifetable$le[le$age])

# (3) Multiply remaining LYs by the value of a LY to get the value of remaining life, V.

# we approximate the predicted probability of death in the next 5 years by giving one fifth of the predicted probability(0.2 Pjt) to each of the first 5 years, and assume that all patients surviving beyond year 5 (with probability 1- Pjt)have the same age- and sex-specific remaining life expectancy as a general individual from the lifetable of that population.

# Below adapted from Brian's Stata Codes (09_lifetable_netvalue)
df <- merge(le, r, by="serial_no", all.y=T)
df$bl_risk <- df$bl_risk/100
df <- df[complete.cases(df)==TRUE,] #15 participants with age >100 eliminated
df <- as.data.table(df)

rate <- 0.03 # 3%
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
  ((df$bl_risk/5)*(valyear50k1 + valyear50k2 + valyear50k3 + valyear50k4 + valyear50k5)
   + (1-df$bl_risk)*
     (50000*((1-(1/(1+rate)^df$le_int))/rate) 
      + 50000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_100k_bl <-
  ((df$bl_risk/5)*(valyear100k1 + valyear100k2 + valyear100k3 + valyear100k4 + valyear100k5)
   + (1-df$bl_risk)*
     (100000*((1-(1/(1+rate)^df$le_int))/rate) 
      + 100000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_200k_bl <-
  ((df$bl_risk/5)*(valyear200k1 + valyear200k2 + valyear200k3 + valyear200k4 + valyear200k5)
   + (1-df$bl_risk)*
     (200000*((1-(1/(1+rate)^df$le_int))/rate)
      + 200000*df$le_rem/(1+rate)^(df$le_int+1)))

df <- df[, -c("le", "bl_risk", "le_int", "le_rem")]

# (2) For those who die during year t, give them 0.5 as their remaining life-years. In other words, decedents received 0.5LY for the year.

# only include decedents who died before 2009-01-01

decedent <- as.data.table(mylist[[1]])
decedent <- decedent[serial_no %in% decedent_no]
decedent <- decedent[death.date < "2009-01-01", c("serial_no", "female", "dob", "entry.date", "death.date")] #27218 obs

decedent$entry.yr <- as.numeric(substr(decedent$entry.date, 1, 4))
decedent$death.yr <- as.numeric(substr(decedent$death.date, 1, 4))
decedent <- decedent[!death.yr == 2006] # remove those who died in 2006 (before entry year)
decedent$le <- decedent$death.yr - decedent$entry.yr + 0.5 # for those who die during year t, give them 0.5 remaining life-years

decedent$age <- decedent$entry.yr - decedent$dob
decedent <- decedent[, c("serial_no", "female", "age", "le")]

decedent$le_int <- decedent$le - decedent$le %% 1
decedent$le_rem <- decedent$le %% 1

decedent$le_50k_bl <- 50000*((1-(1/(1+rate)^decedent$le_int))/rate) + 50000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent$le_100k_bl <- 100000*((1-(1/(1+rate)^decedent$le_int))/rate) + 100000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent$le_200k_bl <- 200000*((1-(1/(1+rate)^decedent$le_int))/rate) + 200000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent <- decedent[, -c("le", "le_int", "le_rem")]

# Combine decedents and survivors dataframes
df <- rbind(df, decedent)

# (4) For each individual in year t (including decedents), subtract c, their total medical spending in year t, from V, the value of remaining life. We call this Vt - ct.

spending <- readRDS("7 predicted spending.rds")
spending[, 2:25] <- spending[, 2:25] / 7.84975

s <- data.table(serial_no = spending$serial_no, spending = spending$bl.spend.yr)
df <- merge(df, s, by = "serial_no", all.x = TRUE) 

# crude results
crude_baseline <- c((mean(df$le_50k_bl) - mean(df$spending)), (mean(df$le_100k_bl) - mean(df$spending)), (mean(df$le_200k_bl) - mean(df$spending)))

# (5) For each age- and sex- group (e.g. males age 40-45), obtain average Vt - ct

# 15-19, 20-24 etc. until >85
df <- mutate(df, group = cut(age, breaks=c(seq(from = 14, to = 84, by = 5), Inf), labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")))
df <- as.data.table(df)
df <- df[, .(spending = mean(spending), le50k = mean(le_50k_bl), le100k = mean(le_100k_bl), le200k = mean(le_200k_bl)), keyby = .(group, female)]

df$nv50k <- df$le50k - df$spending
df$nv100k <- df$le100k - df$spending
df$nv200k <- df$le200k - df$spending

df <- df[, -c("le50k", "le100k", "le200k", "spending")]

# (6) Create a weighted average Vt - ct based on the weights of the reference population,by multiplying the average for each age-sex group by the weight assigned to that age-sex group in the reference population.

# WHO reference population from age 15+ to 85+
df$who <- rep(c(0.057318597, 0.055626862, 0.053664342, 0.051498732, 0.048386049, 0.0445964, 0.040874449, 0.03634014, 0.03079106, 0.025174285, 0.020031384, 0.014955503, 0.010286478, 0.006158347, 0.004297372), each = 2)

df$hkdm <- c(0.001340, 0.000997, 0.001315, 0.001293, 0.001794, 0.002307, 0.003605, 0.004583, 0.007690,	0.008642, 0.016029, 0.014628, 0.029435,	0.023546, 0.059159,	0.046097, 0.082705,	0.068599, 0.090264,	0.081155, 0.090036,	0.084927, 0.070938, 0.066088, 0.067006, 0.075821, NA, NA, NA, NA)

who_baseline <- c(sum(df$nv50k * df$who), sum(df$nv100k * df$who), sum(df$nv200k * df$who))

df2 <- df[complete.cases(df)]
hkdm_baseline <- c(sum(df2$nv50k * df2$hkdm), sum(df2$nv100k * df2$hkdm), sum(df2$nv200k * df2$hkdm))
results <- cbind(crude_baseline, who_baseline, hkdm_baseline)
row.names(results) <- c("Net value per life-year 50k", "Net value per life-year 100k", "Net value per life-year 200k") 

#######################

# For year t=final (2013-2014), calculate the following: ---------

# (1) For those who survive the year (to the end of year t), calculate expected remaining life-years for each individual, based on risk prediction model using biomarkers measured in year t. The predicted remaining life expectancy (LE) is calculated in the same way as for the survivor panel, method (1), but for each individual use their own actual age and duration of diagnosis to predict risk of death and, based on that, their remaining LE.

# select survivors
fn <- final_imputed[is.na(death.date)]

# for each individual, use their own actual age and duration of DM
# for final period, calculate age and duration at 2014-01-01
fn$age <- 2014 - fn$dob
fn$duration <- as.Date("2014-12-31") - fn$dm.date
fn$duration <- as.numeric(fn$duration/365.25) # change format from "difftime" to "numeric", unit from "day" to "year"

# predict risk 
r <- data.frame(serial_no = fn$serial_no, fn_risk = HKU_SG_mortality(fn))

# calculate remaining life years
le <- mylist[[1]][c("serial_no","female","duration","age")]
le$le <- ifelse(le$female==TRUE, female_lifetable$le[le$age], male_lifetable$le[le$age])

df <- merge(le, r, by="serial_no", all.y=T)
df$fn_risk <- df$fn_risk/100

df <- df[complete.cases(df)==TRUE,] #35 participants with age >100 eliminated
df <- as.data.table(df)

df$le_int <- df$le - df$le %% 1
df$le_rem <- df$le %% 1

df$le_50k_fn <-
  ((df$fn_risk/5)*(valyear50k1 + valyear50k2 + valyear50k3 + valyear50k4 + valyear50k5)
   + (1-df$fn_risk)*
     (50000*((1-(1/(1+rate)^df$le_int))/rate) 
      + 50000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_100k_fn <-
  ((df$fn_risk/5)*(valyear100k1 + valyear100k2 + valyear100k3 + valyear100k4 + valyear100k5)
   + (1-df$fn_risk)*
     (100000*((1-(1/(1+rate)^df$le_int))/rate) 
      + 100000*df$le_rem/(1+rate)^(df$le_int+1)))

df$le_200k_fn <-
  ((df$fn_risk/5)*(valyear200k1 + valyear200k2 + valyear200k3 + valyear200k4 + valyear200k5)
   + (1-df$fn_risk)*
     (200000*((1-(1/(1+rate)^df$le_int))/rate)
      + 200000*df$le_rem/(1+rate)^(df$le_int+1)))

df <- df[, -c("le", "fn_risk", "le_int", "le_rem", "duration")]

# (2) For those who die during year t, give them 0.5 as their remaining life-years. In other words, decedents received 0.5LY for the year.
# only include participants who died in 2013 and 2014

decedent <- as.data.table(mylist[[1]])
decedent <- decedent[death.date > "2012-12-31" & death.date < "2015-01-01", c("serial_no", "female", "dob", "entry.date", "death.date")]

decedent$entry.yr <- as.numeric(substr(decedent$entry.date, 1, 4))
decedent$death.yr <- as.numeric(substr(decedent$death.date, 1, 4))
decedent$entry.yr <- ifelse(decedent$entry.yr == 2014, 2014, 2013) # only consider spending / life value during final period, so only the life-year of 2013-14 is considered, even if participants entered before 2013

decedent$le <- decedent$death.yr - decedent$entry.yr + 0.5 
decedent$age <- as.numeric(2014 - decedent$dob)
decedent <- decedent[, c("serial_no", "female", "age", "le")]

# (3) Multiply remaining LYs by the value of a LY to get the value of remaining life, V, assuming value of life-year = $50 000, $100,000 and $200 000

decedent$le_int <- decedent$le - decedent$le %% 1
decedent$le_rem <- decedent$le %% 1

decedent$le_50k_fn <- 50000*((1-(1/(1+rate)^decedent$le_int))/rate) + 50000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent$le_100k_fn <- 100000*((1-(1/(1+rate)^decedent$le_int))/rate) + 100000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent$le_200k_fn <- 200000*((1-(1/(1+rate)^decedent$le_int))/rate) + 200000*decedent$le_rem/(1+rate)^(decedent$le_int+1)

decedent <- decedent[, -c("le", "le_int", "le_rem")]

# Combine decedents and survivors dataframes
df <- rbind(df, decedent)

# (4) For each individual in year t (including decedents), subtract c, their total medical spending in year t, from V, the value of remaining life. We call this Vt - ct.
s <- data.table(serial_no = spending$serial_no, spending = spending$fn.spend.yr)
df <- merge(df, s, by = "serial_no", all.x = TRUE) 

# crude results
crude_final <- c((mean(df$le_50k_fn) - mean(df$spending)), (mean(df$le_100k_fn) - mean(df$spending)), (mean(df$le_200k_fn) - mean(df$spending)))

# (5) For each age- and sex- group (e.g. males age 40-45), obtain average Vt - ct
# 15-19, 20-24 etc. until >85
df <- mutate(df, group = cut(age, breaks=c(seq(from = 14, to = 84, by = 5), Inf), labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")))
df <- as.data.table(df)
df <- df[, .(spending = mean(spending), le50k = mean(le_50k_fn), le100k = mean(le_100k_fn), le200k = mean(le_200k_fn)), keyby = .(group, female)]

df$nv50k <- df$le50k - df$spending
df$nv100k <- df$le100k - df$spending
df$nv200k <- df$le200k - df$spending

df <- df[, -c("le50k", "le100k", "le200k", "spending")]

# (6) Create a weighted average Vt - ct based on the weights of the reference population,by multiplying the average for each age-sex group by the weight assigned to that age-sex group in the reference population.

# WHO reference population from age 15+ to 85+
df$who <- rep(c(0.057318597, 0.055626862, 0.053664342, 0.051498732, 0.048386049, 0.0445964, 0.040874449, 0.03634014, 0.03079106, 0.025174285, 0.020031384, 0.014955503, 0.010286478, 0.006158347, 0.004297372), each = 2)

df$hkdm <- c(0.001340, 0.000997, 0.001315, 0.001293, 0.001794, 0.002307, 0.003605, 0.004583, 0.007690,	0.008642, 0.016029, 0.014628, 0.029435,	0.023546, 0.059159,	0.046097, 0.082705,	0.068599, 0.090264,	0.081155, 0.090036,	0.084927, 0.070938, 0.066088, 0.067006, 0.075821, NA, NA, NA, NA)

who_final <- c(sum(df$nv50k * df$who), sum(df$nv100k * df$who), sum(df$nv200k * df$who))

df2 <- df[complete.cases(df)]
hkdm_final <- c(sum(df2$nv50k * df2$hkdm), sum(df2$nv100k * df2$hkdm), sum(df2$nv200k * df2$hkdm))
results2 <- cbind(crude_final, who_final, hkdm_final)
row.names(results2) <- c("Net value per life-year 50k", "Net value per life-year 100k", "Net value per life-year 200k") 

net_value <- cbind(results, results2)
net_value[, 1:4] <- as.numeric(net_value[, 1:4])
net_value <- as.data.frame(net_value)
net_value$crude_diff <- net_value$crude_final - net_value$crude_baseline
net_value$who_diff <- net_value$who_final - net_value$who_baseline
net_value$hkdm_diff <- net_value$hkdm_final - net_value$hkdm_baseline
net_value

