library(data.table); library(reshape); library(magrittr); library(tidyr)
load("3 medication_DT.Rdata") #"medication"

### drug costs (edited to reduce missingness)
# For drugs with missing unit costs for only some years, the nearest available costs are used
drugcost <- read.csv("4 drug cost edited.csv")
drugcost <- as.data.table(drugcost)
drugcost$item_cd <- as.character(drugcost$item_cd)

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

### Reshape cost data
# Combine "item code" and "year" to new variable "itemyr"
drugcost <- melt(drugcost, id="item_cd")
drugcost <- unite(drugcost, itemyr, item_cd, variable)
drugcost$itemyr <- as.factor(drugcost$itemyr)
setkey (drugcost, itemyr)

### Load and tidy up patients' prescription data
medication <- as.data.table(medication)
med <- medication[, c("ï»¿serial_no", "item_cd", "disp_qty", "disp_date")]
med <- as.data.table(med)
rm(medication)

med$yr <- ""
med[grepl(2006, med$disp_date), c("yr")] <- paste0("yr", 2006)
med[grepl(2007, med$disp_date), c("yr")] <- paste0("yr", 2007)
med[grepl(2008, med$disp_date), c("yr")] <- paste0("yr", 2008)
med[grepl(2009, med$disp_date), c("yr")] <- paste0("yr", 2009)
med[grepl(2010, med$disp_date), c("yr")] <- paste0("yr", 2010)
med[grepl(2011, med$disp_date), c("yr")] <- paste0("yr", 2011)
med[grepl(2012, med$disp_date), c("yr")] <- paste0("yr", 2012)
med[grepl(2013, med$disp_date), c("yr")] <- paste0("yr", 2013)
med[grepl(2014, med$disp_date), c("yr")] <- paste0("yr", 2014)

med$yr <- as.factor(med$yr)
names(med) <-c("serial_no", "item_cd", "disp_qty", "disp_date", "yr")

### Create new variable "itemyr" for matching with drug costs
med <- unite(med, itemyr, item_cd, yr, remove = FALSE)
med$itemyr <- as.factor(med$itemyr)
setkey (med, itemyr)

# Match patients' medications with their drug costs
medcost <- merge(x = med, y = drugcost, by = "itemyr", all.x = TRUE) 
medcost <- medcost[complete.cases(medcost),] #68% prescription without drug cost data

# Multiple individual medication cost by quantity
medcost$totalcost <- medcost$value * medcost$disp_qty
medcost <- medcost[, sum(totalcost), by = .(serial_no, yr)]

# Tidy up dataframe
medcost[, c("yr")] <- lapply(medcost[, c("yr")], gsub, pattern='yr', replacement='')
medcost$yr <- as.factor(medcost$yr)
names(medcost) <- c("serial_no", "yr", "med_cost")
setkey(medcost, "serial_no")
medcost$serial_no <- as.character(medcost$serial_no)

saveRDS(medcost, file = "4a medcost.rds")
