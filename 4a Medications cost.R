library(data.table); library(reshape2)

load("3 medication_DT.Rdata") # Rdata/medication_DT.Rdata
load("Rdata/medication_DT.Rdata")

### Load and tidy up patients' prescription data
myCols <- c("serial_no", "item_cd", "disp_qty", "disp_date")
med <- medication[, myCols, with=F]

rm(medication); gc()

med$yr <- gsub("\\D", "", med$disp_date)
# med$yr <- gsub("[^0-9]", "", med$disp_date) # slower
med$yr <- as.factor(med$yr)
    
### drug costs (edited to reduce missingness)
# For drugs with missing unit costs for only some years, the nearest available costs are used
drugcost <- read.csv("4 drug cost edited.csv") 
drugcost <- read.csv("/Users/chao/OneDrive - The University Of Hong Kong/Diabetes/2018 Net Value paper/source data/4 drug cost edited.csv")

# drugcost <- data.table(drugcost)

### Convert nominal to real spending

#(P.2) Spending #1: Convert nominal spending into real spending in local currency units (preferred method is using GDP deflator, and preferred base year is 2010)
drugcost$yr2006 <- drugcost$yr2006 * 0.9591
drugcost$yr2007 <- drugcost$yr2007 * 0.9895
drugcost$yr2008 <- drugcost$yr2008 * 1.0012
drugcost$yr2009 <- drugcost$yr2009 * 0.9977
drugcost$yr2010 <- drugcost$yr2010 * 1
drugcost$yr2011 <- drugcost$yr2011 * 1.0397
drugcost$yr2012 <- drugcost$yr2012 * 1.0759
drugcost$yr2013 <- drugcost$yr2013 * 1.0958
drugcost$yr2014 <- drugcost$yr2014 * 1.1273

drugcost <- melt(drugcost, id="item_cd")
names(drugcost)[2] <- "yr"
drugcost$yr <- gsub("\\D", "", drugcost$yr)

# Match patients' medications with their drug costs
medcost <- merge(med, drugcost, by = c("item_cd", "yr"), all.x = TRUE) 
medcost <- medcost[, c("serial_no", "item_cd", "disp_qty", "disp_date", "yr", "value")]
sum(!is.na(medcost$value))/length(medcost$serial_no)
medcost <- medcost[complete.cases(medcost),] # 68% prescription without drug cost data

# Multiple individual medication cost by quantity
medcost$totalcost <- medcost$value * medcost$disp_qty
medcost <- medcost[, sum(totalcost), by = .(serial_no, yr)]

# Tidy up dataframe
medcost$yr <- as.factor(medcost$yr)
names(medcost) <- c("serial_no", "yr", "med_cost")
medcost$serial_no <- as.character(medcost$serial_no)

saveRDS(medcost, file = "4a medcost.rds")
