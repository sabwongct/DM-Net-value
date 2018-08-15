#imputation on pt_clinicalvalues.rds

library(Hmisc)
mylist <- readRDS("pt_clinicalvalues.rds")
mylist <- lapply(mylist, function(df) {
  df$age <- df$age_at_diagnosis
  df$age_at_diagnosis <- NULL
  df$smoking[is.na(df$smoking)] <- "Non-smoker"
  df
})
baseline <- mylist[[1]]
final <- mylist[[4]]
summary(baseline)

fm <- as.formula(~ female + age + duration + age*duration + smoking + af + ckd + stroke + chd + cancer + pad + meds + statin + bprx + anticoag + hba1c + sbp + dbp + ldl + bmi + urine_acr + creatinine + egfr_chinese + tc + hdl + haemoglobin + wbc) 

'# do imputation for baseline
before <- proc.time()
imp_bl <- aregImpute(fm, data = baseline, n.impute = 5, nk = 4)
(proc.time() - before)/60/60 # imputation took 15 hours

# do imputation for final
before <- proc.time()
imp_fn <- aregImpute(fm, data = final, n.impute = 5, nk = 4)
(proc.time() - before)/60/60 # imputation took 15 hours
'

imp_bl
imp_fn
View(imp_bl$imputed$bmi) # View e.g. imputed results of BMI

# plug in imputed results (baseline)
imputed_bl <-impute.transcan(imp_bl, data=baseline, imputation=1, list.out=TRUE, check=FALSE, trantab=FALSE) 
tail(imputed_bl) # see imputed values are marked by asterisk
baseline_imputed <- baseline # copy original data to new dataset
baseline_imputed[names(imputed_bl)] <- imputed_bl # NAs are replaced
head(baseline_imputed) # see NAs are substituted

# plug in imputed results (final)
imputed_fn <-impute.transcan(imp_fn, data=final, imputation=1, list.out=TRUE, check=FALSE, trantab=FALSE) 
tail(imputed_fn) # see imputed values are marked by asterisk
final_imputed <- final # copy original data to new dataset
final_imputed[names(imputed_fn)] <- imputed_fn # NAs are replaced
head(final_imputed) # see NAs are substituted

save(baseline_imputed, final_imputed, imp_bl, imp_fn, file = "8b MI_clinicalvalues_0711.Rdata") # save imputation results
