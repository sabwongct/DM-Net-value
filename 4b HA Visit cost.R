# (P.2) Spending #2: All spending for a given individual is included, with no attempt to isolate DM-specific spending

### Load visit data
library(data.table)
load("~/Codes/DM Project/4 vfm/visits.Rdata")
ae <- ae[, .(ae = sum(!is.na(adate))), by = .(serial_no, yr)]
gopc <- gopc[, .(gopc = sum(!is.na(adate))), by = .(serial_no, yr)]
sopc <- sopc[, .(sopc = sum(!is.na(adate))), by = .(serial_no, yr)]
inpatient$type <- "NA"
inpatient[a_spec=="INF", c("type")] <- "INF"
inpatient[a_spec=="PSY", c("type")] <- "MEN_ILL"
inpatient[a_spec=="MH", c("type")] <- "MEN_HAND"
inpatient[type=="NA", c("type")] <- "GEN"
ip <- inpatient[, .(ip = sum(!is.na(adate))), by = .(serial_no, yr, type)]

### Input unit costs for visits from HA annual reports

#### Unit costs for A&E visits, from 2006 to 2014

ae <- ae[order(yr)]
ae$cost <- ""
ae$cost <- as.numeric(ae$cost)
ae$ae <- as.numeric(ae$ae)
ae[yr=="2006", c("cost")] <- 700
ae[yr=="2007", c("cost")] <- 750
ae[yr=="2008", c("cost")] <- 820
ae[yr=="2009", c("cost")] <- 800
ae[yr=="2010", c("cost")] <- 800
ae[yr=="2011", c("cost")] <- 875
ae[yr=="2012", c("cost")] <- 935
ae[yr=="2013", c("cost")] <- 1040
ae[yr=="2014", c("cost")] <- 1140
ae$cost <- ae$cost * ae$ae

#### Unit costs for GOPC visits, from 2006 to 2014

gopc <- gopc[order(yr)]
gopc$cost <- ""
gopc$cost <- as.numeric(gopc$cost)
gopc$gopc <- as.numeric (gopc$gopc)
gopc[yr=="2006", c("cost")] <- 260
gopc[yr=="2007", c("cost")] <- 270
gopc[yr=="2008", c("cost")] <- 280
gopc[yr=="2009", c("cost")] <- 290
gopc[yr=="2010", c("cost")] <- 290
gopc[yr=="2011", c("cost")] <- 335
gopc[yr=="2012", c("cost")] <- 360
gopc[yr=="2013", c("cost")] <- 385
gopc[yr=="2014", c("cost")] <- 410
gopc$cost <- gopc$cost * gopc$gopc

#### Unit costs for SOPC visits, from 2006 to 2014

sopc <- sopc[order(yr)]
sopc$cost <- ""
sopc$cost <- as.numeric(sopc$cost)
sopc$sopc <- as.numeric(sopc$sopc)
str(sopc)
sopc[yr=="2006", c("cost")] <- 740
sopc[yr=="2007", c("cost")] <- 780
sopc[yr=="2008", c("cost")] <- 840
sopc[yr=="2009", c("cost")] <- 880
sopc[yr=="2010", c("cost")] <- 910
sopc[yr=="2011", c("cost")] <- 985
sopc[yr=="2012", c("cost")] <- 1050
sopc[yr=="2013", c("cost")] <- 1080
sopc[yr=="2014", c("cost")] <- 1130
sopc$cost <- sopc$cost * sopc$sopc

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
