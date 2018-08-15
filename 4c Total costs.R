#(P.2) Spending #1: Convert nominal spending into real spending in local currency units (preferred method is using GDP deflator, and preferred base year is 2010)
#(P.2) Spending #2: All spending for a given individual is included, with no attempt to isolate DM-specific spending

library(data.table)
medcost <- readRDS("4a medcost.rds") # medcost = medication costs
visit <- readRDS("4b visitcost.rds") # all = visit costs
mylist <- readRDS("pt_clinicalvalues.rds") # mylist = clinical values

# costs are already converted to real spending

# create empty dataframe (d) for combining medication and visit costs, where each serial id have 9 separate entries from 2006 to 2014
d <- mylist[[1]][, c("serial_no", "entry.date")]
d <- as.data.table(d)
d <- d[rep(seq_len(nrow(d)), each=9),]
d$yr <- rep(2006:2014, length.out = nrow(d))
d$yr <- as.factor(d$yr)

# insert visit and medication costs into the dataframe
d <- merge(d, visit[, c("serial_no", "visit_cost", "yr")], all.x = TRUE, by = c("serial_no", "yr"))
d <- merge(d, medcost[, c("serial_no", "med_cost", "yr")], all.x = TRUE, by = c("serial_no", "yr"))

# change all NA costs to 0
d[is.na(d)] <- 0

## spending = visit cost + drug cost
d$spending <- d$med_cost + d$visit_cost
setkeyv(d, c("serial_no", "yr"))

### Adjust for late enrolment into cohort
# (P.2 Spending 3.) Aggregate up to total annual spending, for each year in the study period (using fraction of year enrolled/observed, if individual only in sample part of a year)

# Calculate difference between yr-01-01 and dm date
# Sum up total costs for each individual
# Adjust cost by (original cost / (missed period / total period)), where period = 365

d$yr.begins <- as.Date(paste0(d$yr, "-01-01"))
d$adj <- ifelse((d$entry.date>d$yr.begins & d$yr == format(d$entry.date, "%Y")), d$entry.date-d$yr.begins, 0)

d$spend.adj <- d$spending
spending_adjust <- d[!(d$adj==0), c("spending")]
days_adjust <- d[!(d$adj==0), c("adj")]
d[!(d$adj==0), c("spend.adj")] <- spending_adjust / ((365-days_adjust)/365)

# tidy up dataframe
setkey(d, "serial_no")
d <- d[, c("serial_no", "yr", "spend.adj")]

saveRDS(d, file="4c total adjusted spending.rds")
