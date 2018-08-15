### Load patients' Elixhauser comorbidity index
#AIDS and ulcer not loaded, as no patients have these comorbidities
library(data.table)
setwd("~/5 Elixhauser (enhanced)")

load("alcohol.Rdata"); load("anemdef.Rdata"); load("arrhythmia.Rdata"); load("bldloss.Rdata"); load("chf.Rdata"); load("chrnlung.Rdata"); load("coag.Rdata"); load("depress.Rdata"); load("drug.Rdata"); load("htn.Rdata"); load("htncx.Rdata"); load("hypothy.Rdata"); load("liver.Rdata"); load("lymph.Rdata"); load("lytes.Rdata"); load("mets.Rdata"); load("neuro.Rdata"); load("obese.Rdata"); load("para.Rdata"); load("perivasc.Rdata"); load("psych.Rdata"); load("pulmcirc.Rdata"); load("renlfail.Rdata"); load("rheum.Rdata"); load("tumor.Rdata"); load("valve.Rdata")
setwd("C:/Users/janet/Documents/Codes")
names(alcohol)<-c("serial_no", "alcohol")
names(anemdef)<-c("serial_no", "anemdef")
names(arrhythmia)<-c("serial_no","arrhythmia")
names(bldloss)<-c("serial_no", "bldloss")
names(chf)<-c("serial_no", "chf")
names(arrhythmia)<-c("serial_no", "arrhythmia")
names(bldloss)<-c("serial_no", "bldloss")
names(chf)<-c("serial_no", "chf")
names(chrnlung)<-c("serial_no", "chrnlung")
names(coag)<-c("serial_no", "coag")
names(depress)<-c("serial_no", "depress")
names(drug)<-c("serial_no", "drug")
names(htn)<-c("serial_no", "htn")
names(htncx)<-c("serial_no", "htncx")
names(hypothy)<-c("serial_no", "hypothy")
names(liver)<-c("serial_no", "liver")
names(lymph)<-c("serial_no", "lymph")
names(lytes)<-c("serial_no", "lytes")
names(mets)<-c("serial_no", "mets")
names(neuro)<-c("serial_no", "neuro")
names(obese)<-c("serial_no", "obese")
names(para)<-c("serial_no", "para")
names(perivasc)<-c("serial_no", "perivasc")
names(psych)<-c("serial_no", "psych")
names(pulmcirc)<-c("serial_no", "pulmcirc")
names(renlfail)<-c("serial_no", "renlfail")
names(rheum)<-c("serial_no", "rheum")
names(tumor)<-c("serial_no", "tumor")
names(valve)<-c("serial_no", "valve")

comorbidity <- Reduce(function(x, y) merge(x, y, all=TRUE), list(alcohol, anemdef, arrhythmia, bldloss, chf, chrnlung, coag, depress, drug, htn, htncx, hypothy, liver, lymph, lytes, mets, neuro, obese, para, perivasc, psych, pulmcirc, renlfail, rheum, tumor, valve))

# change all dates to years
for(i in 2:27) {
  comorbidity[, i] <- format(comorbidity[, i], "%Y")
  comorbidity[, i] <- as.numeric(comorbidity[, i])
}

# format comorbidity dataframe for matching with spending
comorbidity <- comorbidity[rep(seq_len(nrow(comorbidity)), each=9),]
comorbidity$yr <- rep(2006:2014, length.out = nrow(comorbidity))
for(i in 2:27) {
  comorbidity[, i] <- ifelse(comorbidity[, i] <= comorbidity$yr, TRUE, FALSE)
}
comorbidity[is.na(comorbidity)] <- FALSE

comorbidity <- as.data.table(comorbidity)
saveRDS(comorbidity, file = "5 comorbidity.rds")

##########------------------
# combine with spending
spending <- readRDS("4c total adjusted spending.rds")
spending$yr <- as.numeric(spending$yr)+2005 # changing from as.factor to as.numeric
d <- merge(x = spending, y = comorbidity, all.x = TRUE, by = c("serial_no", "yr")) # all dm patients' serial no. included in the spending dataframe

# combine with age and duration of diabetes
mylist <- readRDS("pt_clinicalvalues.rds")
baseline <- mylist[[1]][, c("serial_no", "female", "age_at_diagnosis", "dob", "duration", "age")]

d <- merge(x = d, y = baseline, all = TRUE, by = c("serial_no"))

# for missing comorbidities (i.e. the participant does not have any comorbidity, n = 287419), code as FALSE
d[is.na(d)] <- FALSE 

saveRDS(d, file = "5 combined.rds")
