# (P.2) Spending #2: All spending for a given individual is included, with no attempt to isolate DM-specific spending

### Load visit data
library(data.table)
load("~/Codes/DM Project/4 vfm/visits.Rdata")
load("vfm/visits.Rdata")

# AE cost on per visit basis (not per night)? 2nd night -> counted as inpatient costs?
str(ae)
ae <- ae[, .(ae = sum(!is.na(adate))), by = .(serial_no, yr)]
# use los_hr?
ae

# GOPC
str(gopc)
gopc <- gopc[, .(gopc = sum(!is.na(adate))), by = .(serial_no, yr)]
gopc

# SOPC first vs follow-up price?
# need to separate out FAMILY SOPC / FMSC?
str(sopc)
table(sopc$case_type, exclude=NULL)
# sopc_new more costly than sopc_fu because of time, workup
sopc <- sopc[, list(sopc_new = sum(case_type == "First"), sopc_fu = sum(case_type == "Follow-up")), by = list(serial_no, yr)]
sopc

### Input unit costs for visits from HA annual reports from 2006 to 2014
cost <- data.table(yr = c(2006:2014))

#### Unit costs for A&E, GOPC, SOPC visits, from 2006 to 2014
cost$ae <- c(700, 750, 820, 800, 800, 875, 935, 1040, 1140)
cost$gopc <- c(260, 270, 280, 290, 290, 335, 360, 385, 410)
cost$sopc <- c(740, 780, 840, 880, 910, 985, 1050, 1080, 1130)
# check costs
cost

ae <- merge(ae, cost, by.x = "yr", by.y = "ae", all.x = T)
ae$cost <- ae$cost * ae$ae

gopc <- merge(gopc, cost, by.x = "yr", by.y = "gopc", all.x = T)
gopc$cost <- gopc$cost * gopc$gopc

sopc <- merge(sopc, cost, by.x = "yr", by.y = "sopc", all.x = T)
sopc$cost <- sopc$cost * sopc$sopc

# Inpatients - case type?
str(inpatient)
table(inpatient$a_spec, exclude = NULL)
inpatient$los <- inpatient$los_day
# Assume all LOS = 0 are charged as 1 day -> likely to be day case
table(inpatient$los == 0, exclude = NULL)
# 1mil+ admissions have LOS = 0
inpatient$los[inpatient$los == 0] <- 1

# what type of inpatient costs are there? HA annual report vs Gazette costs
# "ICU", "HDU", "PRI", "PSY" = "MEN_ILL", "INF", "MEN_HAND"
icu <- inpatient[, list(icu = sum(los)), by = list(serial_no, yr, a_spec)]

# No MUL
# ?exclude OBS?
# ICU/HDU
# Private ward
# Psychiatry ward
# General ward

exclude <- c("ICU", "HDU", "PRI", "PSY")
inpatient$type <- "NA"
inpatient[a_spec=="INF", c("type")] <- "INF"
inpatient[a_spec=="PSY", c("type")] <- "MEN_ILL"
inpatient[a_spec=="MH", c("type")] <- "MEN_HAND"
inpatient[type=="NA", c("type")] <- "GEN"


#### Unit costs for inpatient days, from 2006 to 2014
# Costs for general / infirmary / mentally ill / mentally handicapped wards are different

ip <- ip[order(yr)]
ip$cost <- ""
ip$cost <- as.numeric(ip$cost)
ip$ip <- as.numeric (ip$ip)
ip[yr=="2006" & type =="GEN", c("cost")] <- 3290
ip[yr=="2006" & type =="INF", c("cost")] <- 990
ip[yr=="2006" & type =="MEN_ILL", c("cost")] <- 1560
ip[yr=="2006" & type =="MEN_HAND", c("cost")] <- 960
ip[yr=="2007" & type =="GEN", c("cost")] <- 3440
ip[yr=="2007" & type =="INF", c("cost")] <- 1030
ip[yr=="2007" & type =="MEN_ILL", c("cost")] <- 1720
ip[yr=="2007" & type =="MEN_HAND", c("cost")] <- 1030
ip[yr=="2008" & type =="GEN", c("cost")] <- 3650
ip[yr=="2008" & type =="INF", c("cost")] <- 1090
ip[yr=="2008" & type =="MEN_ILL", c("cost")] <- 1890
ip[yr=="2008" & type =="MEN_HAND", c("cost")] <- 1050
ip[yr=="2009" & type =="GEN", c("cost")] <- 3590
ip[yr=="2009" & type =="INF", c("cost")] <- 1130
ip[yr=="2009" & type =="MEN_ILL", c("cost")] <- 1780
ip[yr=="2009" & type =="MEN_HAND", c("cost")] <- 1070
ip[yr=="2010" & type =="GEN", c("cost")] <- 3600
ip[yr=="2010" & type =="INF", c("cost")] <- 1130
ip[yr=="2010" & type =="MEN_ILL", c("cost")] <- 1750
ip[yr=="2010" & type =="MEN_HAND", c("cost")] <- 1070
ip[yr=="2011" & type =="GEN", c("cost")] <- 3950
ip[yr=="2011" & type =="INF", c("cost")] <- 1270
ip[yr=="2011" & type =="MEN_ILL", c("cost")] <- 1930
ip[yr=="2011" & type =="MEN_HAND", c("cost")] <- 1190
ip[yr=="2012" & type =="GEN", c("cost")] <- 4180
ip[yr=="2012" & type =="INF", c("cost")] <- 1360
ip[yr=="2012" & type =="MEN_ILL", c("cost")] <- 2150
ip[yr=="2012" & type =="MEN_HAND", c("cost")] <- 1220
ip[yr=="2013" & type =="GEN", c("cost")] <- 4330
ip[yr=="2013" & type =="INF", c("cost")] <- 1400
ip[yr=="2013" & type =="MEN_ILL", c("cost")] <- 2270
ip[yr=="2013" & type =="MEN_HAND", c("cost")] <- 1290
ip[yr=="2014" & type =="GEN", c("cost")] <- 4600
ip[yr=="2014" & type =="INF", c("cost")] <- 1470
ip[yr=="2014" & type =="MEN_ILL", c("cost")] <- 2470
ip[yr=="2014" & type =="MEN_HAND", c("cost")] <- 1400
ip$cost <- ip$cost * ip$ip

#### Combine all visit costs for each individual
ae <- ae[, .(serial_no, yr, cost)]
gopc <- gopc[, .(serial_no, yr, cost)]
sopc <- sopc[, .(serial_no, yr, cost)]
ip <- ip[, .(serial_no, yr, cost)]
all <- rbind(ae, gopc, sopc, ip)
all <- all[, .(cost=sum(cost)), by = c("serial_no", "yr")][order(serial_no)]

### Convert nominal to real spending
#(P.2) Spending #1: Convert nominal spending into real spending in local currency units (preferred method is using GDP deflator, and preferred base year is 2010)

all$deflator <- as.numeric("NA")
all[yr=="2006", c("deflator")] <- 0.9591
all[yr=="2007", c("deflator")] <- 0.9895
all[yr=="2008", c("deflator")] <- 1.0012
all[yr=="2009", c("deflator")] <- 0.9977
all[yr=="2010", c("deflator")] <- 1
all[yr=="2011", c("deflator")] <- 1.0397
all[yr=="2012", c("deflator")] <- 1.0759
all[yr=="2013", c("deflator")] <- 1.0958
all[yr=="2014", c("deflator")] <- 1.1273
all$cost <- all$cost * all$deflator

# tidy up dataframe
all <- all[, sum(cost), by = .(serial_no, yr)]
names(all) <- c("serial_no", "yr", "visit_cost")
all$yr <- as.factor(all$yr)

saveRDS(all, file = "4b visitcost.rds")



# Still need to work on (old code):

# NEP Gazaette Fees revised in on 1 April 2013!
###################################################


# Hemodialysis visits
# Assume all A&E (including A&E follow-up?) is acute,  all SOPC is chronic 
# Inpatients assume ALL LOS = 0 as chronic hemodialysis (day case), 
# LOS of 1 or more days = acute hemodialysis
load("Rdata/haemodialysis.Rdata")
haemodialysis$yr <- as.numeric(haemodialysis$yr)
d <- merge(d, haemodialysis, all = T, by = c("serial_no", "yr"))
d[is.na(d)] <- 0



# NEP Fees revised in on 1 April 2013!
###################################################
# A&E $990
# GOPC $385
# SOPC $1100
# Inpatient general $4680

table(d$yr, exclude = NULL)
d$ae <- d$ae * 990
d$gopc <- d$gopc * 385
d$sopc_new <- d$sopc_new * 1100
d$sopc_fu <- d$sopc_fu * 1100
d$ip_gen <- d$ip_gen * 4680

# Inpatient private 1st class (acute hospital) = $5640
d$ip_pri <- d$ip_pri * 5640

# ICU = $23000
# HDU = $12000
d$icu <- d$icu * 23000
d$hdu <- d$hdu * 12000
d$psy <- d$psy * 1940

# Haemodialysis Acute = $6000; Chronic = $3000
d$h.acute <- d$h.acute * 6000
d$h.chronic <- d$h.chronic * 3000

d$cost <- rowSums(d[, subset(d, select = -c(serial_no, yr))])

# merge spending with cohort serial no
d <- d[d$yr>2006] 
d <- merge(mylist[[1]][, c("serial_no", "entry.date", "death.date")], d[, c("serial_no", "yr", "cost")], all.x=T)

# adjust for late diagnosis/early death in periods
d$visit.start <- as.Date(paste0(d$yr, "-01-01"))
d$adj1 <- ifelse(d$entry.date>d$visit.start, d$entry.date-d$visit.start, 0)
d$visit.end <- as.Date(paste0(d$yr, "-12-31"))
d$adj2 <- ifelse(d$death.date<d$visit.end, d$visit.end-d$death.date, 0)

summary(d$adj1)
summary(d$adj2)
d$adj <- rowSums(d[, c("adj1", "adj2")], na.rm=T)

d <- data.table(d)
d$yr[d$yr==2007|d$yr==2008] <- "yr20072008"
d$yr[d$yr==2009|d$yr==2010] <- "yr20092010"
d$yr[d$yr==2011|d$yr==2012] <- "yr20112012"
d$yr[d$yr==2013|d$yr==2014] <- "yr20132014"

d <- d[, list(cost=sum(cost), adj=sum(adj)), by=c("serial_no", "yr")]
d$cost.adj <- d$cost/((365*2-d$adj)/(365*2))
d <- d[order(yr)]

cost <- reshape(d[, c("serial_no", "yr", "cost.adj")], idvar="serial_no", timevar="yr", direction = "wide")

names(cost) <- c("serial_no", "cost20072008", "cost20092010", "cost20112012", "cost20132014", "NA")
l <- merge(l, data.frame(cost), all.x = T)



'
# NEP fees 2006 to March 2013
#################################
ae$yr[ae$adate >= as.Date("2013-04-01")]<-"2013new"
gopc$yr[gopc$adate >= as.Date("2013-04-01")]<-"2013new"
sopc$yr[sopc$adate >= as.Date("2013-04-01")]<-"2013new"
inpatient$yr[inpatient$adate >= as.Date("2013-04-01")]<-"2013new"
# A&E/A&E follow-up = $570, 
# GOPC = $215
# SOPC = $700
# Inpatient general = $3300
# Inpatient psychiatry = $1940
# d <- subset(visits, visits$yr == "2013new")
table(d$yr, exclude = NULL)
d$ae <- d$ae * 570
d$gopc <- d$gopc * 215
d$sopc_new <- d$sopc_new * 700; d$sopc_fu <- d$sopc_fu * 700
d$ip_gen <- d$ip_gen * 3300

ae <- ae[,list(ae = sum(!is.na(adate))), by = list(serial_no, yr)]
ae

gopc <- gopc[,list(gopc = sum(!is.na(adate))), by = list(serial_no, yr)]
gopc

sopc <- sopc[,list(sopc_new = sum(case_type == "N"), 
sopc_fu = sum(case_type == "S")), by = list(serial_no, yr)]
sopc
'


#################################################################
# ANALYSIS
##################################################################

# DAY hospitals -> NO DATA

d$cost <- rowSums(d[,subset(d, select = -c(serial_no, yr))])
colSums(d[, 3:12, with = F])

'   # Ignore  below:
#################################

# GDP price deflator to 2013 prices (no need if same stable cost)
#######################################
# HK CSD using 2013 as base = 100, 2013 revised figures
# http://www.censtatd.gov.hk/hkstat/sub/sp250.jsp?tableId = 130&ID = 0&productType = 8

price <- read.csv("D:/source/gdp_deflator.csv")
d$cost[d$yr == 2012]<-d$cost[d$yr == 2012] * price$deflator[price$year == 2012]
d$cost[d$yr == 2011]<-d$cost[d$yr == 2011] * price$deflator[price$year == 2011]
d$cost[d$yr == 2010]<-d$cost[d$yr == 2010] * price$deflator[price$year == 2010]
d$cost[d$yr == 2009]<-d$cost[d$yr == 2009] * price$deflator[price$year == 2009]
d$cost[d$yr == 2008]<-d$cost[d$yr == 2008] * price$deflator[price$year == 2008]
d$cost[d$yr == 2007]<-d$cost[d$yr == 2007] * price$deflator[price$year == 2007]
d$cost[d$yr == 2006]<-d$cost[d$yr == 2006] * price$deflator[price$year == 2006]

costs <- d[, list(serial_no, yr, cost)]
costs$yr[costs$yr == "2013new"]<-"2013"
'

