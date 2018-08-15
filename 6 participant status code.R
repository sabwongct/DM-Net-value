library(data.table)
mylist <- readRDS("pt_clinicalvalues.rds")

# 2-year periods
tmp <- as.data.table(mylist[[1]])
decedent_no <- tmp[tmp$entry.date < "2009-01-01" & tmp$death.date < "2015-01-01" & tmp$death.date > "2006-12-31", serial_no]
incident_no <- tmp[tmp$entry.date > "2008-12-31", serial_no]
survivor_no <- tmp[tmp$entry.date < "2009-01-01" & (tmp$death.date > "2014-12-31" | is.na(tmp$death.date)), serial_no]

save(decedent_no, incident_no, survivor_no, file = "6 participant status.Rdata")