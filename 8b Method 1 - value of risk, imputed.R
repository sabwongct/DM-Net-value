# Risk prediction using model formulas:
# risk prediction (5-year mortality) HKU-SG model
library(Hmisc); library(dplyr); library(data.table)

# load required data
mylist <- readRDS("pt_clinicalvalues.rds")
load("8b MI_clinicalvalues_0711.Rdata")
rm(imp_bl, imp_fn)
baseline_imputed <- as.data.table(baseline_imputed)
final_imputed <- as.data.table(final_imputed)

# method 1: choose only survivors (serial no identified already)
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

# only select survivors
bl <- baseline_imputed[serial_no %in% survivor_no] # only survivors
fn <- final_imputed[serial_no %in% survivor_no] # only survivors

# get participants' age at entry year, which is held constant at baseline and final period
bl$entry.year <- as.numeric(substr(bl$entry.date,1,4))
bl$age <- bl$entry.year - bl$dob
fn$age <- bl$age

# duration of diabetes was already calculated

# make baseline and final clincial values a list
imputed_list <- list(bl, fn)
names(imputed_list) <- c("baseline", "final")
                        
# modifiable risk
risk_HKU_SG <- data.frame(serial_no = survivor_no, lapply(imputed_list, HKU_SG_mortality))
r <- risk_HKU_SG

# remove objects not needed
rm(baseline_imputed, final_imputed, risk_HKU_SG)
gc()

# we approximate the predicted probability of death in the next 5 years by giving one fifth of the predicted probability (0.2 Pjt) to each of the first 5 years, and assume that all patients surviving beyond year 5 (with probability 1- Pjt) have the same age- and sex-specific remaining life expectancy
# lifetable at 2006
# lifetable <- read.csv("/Users/chao/Dropbox (Personal)/Diabetes/Source data/HK life tables.csv")
male_lifetable <- data.frame(age = seq(1:100), le=c(78.53, 77.56, 76.58, 75.60, 74.61, 73.63, 72.64, 71.65, 70.65, 69.66, 68.67, 67.68, 66.68, 65.69, 64.70, 63.71, 62.72, 61.73, 60.74, 59.76, 58.78, 57.80, 56.82, 55.85, 54.87, 53.90, 52.94, 51.97, 51.00, 50.04, 49.07, 48.10, 47.14, 46.17, 45.20, 44.24, 43.28, 42.32, 41.36, 40.41, 39.46, 38.51, 37.56, 36.61, 35.67, 34.73, 33.79, 32.86, 31.93, 31.01, 30.10, 29.20, 28.30, 27.41, 26.52, 25.65, 24.78, 23.92, 23.07, 22.23, 21.40, 20.59, 19.79, 18.99, 18.21, 17.44, 16.67, 15.92, 15.19, 14.47, 13.77, 13.09, 12.42, 11.78, 11.15, 10.55, 9.96, 9.40, 8.86, 8.34, 7.84, 7.37, 6.92, 6.49, 6.09, 5.70, 5.33, 4.98, 4.65, 4.34, 4.05, 3.77, 3.51, 3.27, 3.04, 2.83, 2.63, 2.44, 2.27, 2.11))
female_lifetable <- data.frame(seq =seq(1:100), le = c(84.69, 83.71, 82.72, 81.73, 80.74, 79.75, 78.76, 77.77, 76.77, 75.78, 74.78, 73.79, 72.80, 71.81, 70.81, 69.82, 68.83, 67.84, 66.85, 65.86, 64.86, 63.87, 62.88, 61.89, 60.90, 59.91, 58.93, 57.94, 56.96, 55.97, 54.99, 54.01, 53.03, 52.05, 51.07, 50.09, 49.12, 48.14, 47.17, 46.20, 45.23, 44.26, 43.29, 42.33, 41.37, 40.40, 39.45, 38.49, 37.54, 36.60, 35.66, 34.72, 33.79, 32.86, 31.93, 31.01, 30.09, 29.17, 28.25, 27.35, 26.44, 25.55, 24.65, 23.77, 22.89, 22.01, 21.14, 20.28, 19.44, 18.60, 17.78, 16.98, 16.20, 15.43, 14.68, 13.94, 13.23, 12.54, 11.87, 11.23, 10.60, 10.01, 9.43, 8.88, 8.35, 7.84, 7.35, 6.88, 6.44, 6.01, 5.61, 5.22, 4.86, 4.52, 4.19, 3.89, 3.60, 3.33, 3.08, 2.85))
le <- mylist[[1]][c("serial_no","female","duration","age")]
le$le <- ifelse(le$female==TRUE, female_lifetable$le[le$age], male_lifetable$le[le$age])

df <- merge(le, r, by="serial_no", all.y=T)

# change mortality risk from percentage to decimals
df$baseline <- df$baseline/100
df$final <- df$final/100

df <- df[complete.cases(df)==TRUE,] # eliminated those whose age > 100
df$diff <- df$final - df$baseline

save(df, file = "8b predicted_mortality.Rdata")

# summary of changes in risks (for sheet 3 in Karen's result table)
summary(df$baseline)
sd(df$baseline)
summary(df$final)
sd(df$final)
nrow(df)
summary(df$diff)
sd(df$diff)

#by age
diff_age <- (df[, c("serial_no", "age", "baseline", "final", "diff")])
diff_age %>% mutate(quintile = ntile(age, 5)) -> diff_age

for (i in 1:5){
  assign(paste0("age", i), diff_age[diff_age$quintile == i,])
}

summary(age1)
sd(age1$baseline); sd(age1$final); sd(age1$diff)
summary(age2)
sd(age2$baseline); sd(age2$final); sd(age2$diff)
summary(age3)
sd(age3$baseline); sd(age3$final); sd(age3$diff)
summary(age4)
sd(age4$baseline); sd(age4$final); sd(age4$diff)
summary(age5)
sd(age5$baseline); sd(age5$final); sd(age5$diff)

#by duration
diff_dur <- (df[, c("serial_no", "duration", "baseline", "final")])
diff_dur %>% mutate(quintile = ntile(duration, 5)) -> diff_dur

for (i in 1:5){
  assign(paste0("dur", i), diff_dur[diff_dur$quintile == i,])
}

summary(dur1)
sd(dur1$baseline); sd(dur1$final)
summary(dur2)
sd(dur2$baseline); sd(dur2$final)
summary(dur3)
sd(dur3$baseline); sd(dur3$final)
summary(dur4)
sd(dur4$baseline); sd(dur4$final)
summary(dur5)
sd(dur5$baseline); sd(dur5$final)