# load participants' clinical values, survivor status, and predicted spending
mylist <- readRDS("pt_clinicalvalues.rds")
load("6 participant status.Rdata")
s <- readRDS("4c total adjusted spending.rds")

library(data.table); library(dplyr)

# Convert spending from HKD to USD
s$spend.adj <- s$spend.adj / 7.84975

## (P.1) Denote the average total spending of the decedents in the year that they died as MDt. Todo so, sum all the spending of all those who died at any point in the year, and divide by the number who died in that year

# find average spending in last year of life
d <- mylist[[1]]
d <- d[d$serial_no %in% decedent_no,] # choose only decedents
d$yr <- substr(d$death.date,1,4) # get decedents' year of death
d$yr <- as.factor(d$yr)

last <- merge(d[, c("serial_no", "yr")], s[, c("serial_no", "yr", "spend.adj")], all.x = TRUE) # merge spending data with participants' death yr to get spending in last year of life
last <- as.data.table(last)
last <- last[, mean(spend.adj), keyby = yr] # get average spending of last yr of life for each yr
names(last) <- c("yr", "last")

# (P.2) Subtract decedents' actual (or, if not available, assumed average) spending, and express as a per-decedent net value ((Total value - total spending)/(# of decedents)). This gives the average net value of a life year for the decedents in a given year. 
# For people who die in Y2, Y3 and Y4, give them 1.5, 2.5, and 3.5 years of life, respectively, and tabulate their expenses for each year (MY1, MY2, MY3).
nonlast <- merge(d[, c("serial_no", "yr")], s[, c("serial_no", "yr", "spend.adj")], all.x = TRUE, by = "serial_no")
nonlast <- as.data.table(nonlast)
nonlast$yr.x <- as.numeric(nonlast$yr.x) + 2006 # convert factor to numeric

# tabulate spending for each year, classified into "last yr of life" and "non-last"
# yr.x = death yr
# yr.y = spending yr
# for each year, filter out participants who die before or in that year, then calculate the average spending for the remaining participants
eachyr <- nonlast[!(yr.x == "2007") & yr.y == "2007", mean(spend.adj)]

for (i in 2008:2013){
  eachyr <- rbind(eachyr, nonlast[!(yr.x <= i) & yr.y == i, mean(spend.adj)])
}
eachyr <- rbind(eachyr, NA) # add NA for yr 2014, which is not a "non-last" year for any of the decedents

eachyr <- cbind(last, eachyr)
colnames(eachyr) <- c("yr", "last", "non-last")
eachyr <- as.data.table(eachyr)

# Assume Cutler coefficient = 0.5
a <- 0.5
# Assume value per life-year = 50000
v <- 50000

# Per-decedent net value for decedents from 2007-2014
# for decedents in 2007, assume they live 0.5 life-years
nv <- 0.5 * v * a - eachyr[[1, c("non-last")]]

# for decedents in 2008-2014 (assume they live 1.5-7.5 life-years)
# e.g. for decedents in 2009 (i = 2), life-yr is 2.5 (i + 0.5), spending is the sum of non-last avg spending in 2007-08 (rows 1:i) and last yr avg spending in 2009 (row i+1)
for (i in 1:7){
nv <- rbind(nv, (i + 0.5) * v * a - sum(eachyr[1:i, c("non-last")]) - eachyr[[(i + 1), c("last")]])
}
rownames(nv) <- seq(from = 0.5, to = 7.5) # add corresponding no. of life-years to net value

# (P.3) Then compute a weighted average net value for decedents of the Y1 cohort as follows: NVD =???DYt/D*NVDt

# find number of decedents who lived the life-years
d <- mylist[[1]]
d <- d[d$serial_no %in% decedent_no, c("serial_no", "entry.date", "death.date")] # choose only decedents
d$entry.yr <- as.numeric(substr(d$entry.date,1,4)) # get decedents' year of entry into cohort
d$death.yr <- as.numeric(substr(d$death.date,1,4)) # get decedents' year of death
d$ly <- d$death.yr - d$entry.yr + 0.5
d <- as.data.table(d)
d <- d[, .N, keyby = ly]

# Combine net value with no. of decedents
nv <- cbind(nv, d)
colnames(nv) <- c("netvalue", "lifeyears", "number")

# Convert number of decedents to percentage
total <- sum(nv$number) # Total number of decedents
nv$percentage <- nv$number / sum(nv$number)

# Weighted average net value for decedents
nvd50k <- sum(nv$netvalue * nv$percentage)

# (P.3) Finally, compute a weighted average net value for the panel of survivors and the decedents as follows: NV = (D/(S + D))*NVD + (S/(S + D))*NVS, Where S = Total Y1 cohort of survivors, the survivor panel

# Weighted average net value for survivors and decedents
nvs50k <- 3589.419 # crude result from method 1
survivor <- length(survivor_no)
decedent <- length(decedent_no)
nv50k <- nvs50k * (survivor / (survivor + decedent)) + nvd50k * (decedent / (survivor + decedent))

###############################
## repeat with value of 1 life-year = 100k ##

# Assume value per life-year = 50000
v <- 100000

# Per-decedent net value for decedents from 2007-2014
nv <- 0.5 * v * a - eachyr[[1, c("non-last")]]

for (i in 1:7){
      nv <- rbind(nv, (i + 0.5) * v * a - sum(eachyr[1:i, c("non-last")]) - eachyr[[(i + 1), c("last")]])
}
rownames(nv) <- seq(from = 0.5, to = 7.5)

# Combine net value with no. of decedents
nv <- cbind(nv, d)
colnames(nv) <- c("nv", "ly", "N")

# Convert number of decedents to percentage
total <- sum(nv$N) # Total number of decedents
nv$percentage <- nv$N / total

# Weighted average net value for decedents
nvd100k <- sum(nv$nv * nv$percentage)

# (P.3) Finally, compute a weighted average net value for the panel of survivors and the decedents as follows: NV = (D/(S + D))*NVD + (S/(S + D))*NVS, Where S = Total Y1 cohort of survivors, the survivor panel
# Weighted average net value for survivors and decedents
nvs100k <- 9204.521 # crude result from method 1
survivor <- length(survivor_no)
decedent <- length(decedent_no)
nv100k <- nvs100k * (survivor / (survivor + decedent)) + nvd100k * (decedent / (survivor + decedent))


######################################
## repeat with value of 1 life-year = 200k ##

# Assume value per life-year = 50000
v <- 200000

# Per-decedent net value for decedents from 2007-2014
nv <- 0.5 * v * a - eachyr[[1, c("non-last")]]

for (i in 1:7){
      nv <- rbind(nv, (i + 0.5) * v * a - sum(eachyr[1:i, c("non-last")]) - eachyr[[(i + 1), c("last")]])
}
rownames(nv) <- seq(from = 0.5, to = 7.5)

# Combine net value with no. of decedents
nv <- cbind(nv, d)
colnames(nv) <- c("nv", "ly", "N")

# Convert number of decedents to percentage
total <- sum(nv$N) # Total number of decedents
nv$percentage <- nv$N / total

# Weighted average net value for survivors and decedents
nvd200k <- sum(nv$nv * nv$percentage)

# (P.3) Finally, compute a weighted average net value for the panel of survivors and the decedents as follows: NV = (D/(S + D))*NVD + (S/(S + D))*NVS, Where S = Total Y1 cohort of survivors, the survivor panel
# Weighted average net value for survivors and decedents
nvs200k <- 22460.41 # crude result from method 1
survivor <- length(survivor_no)
decedent <- length(decedent_no)
nv200k <- nvs200k * (survivor / (survivor + decedent)) + nvd200k * (decedent / (survivor + decedent))

# final results
paste0("No. of decedents: ", decedent)
paste0("No. of survivors: ", survivor)
paste0("Assuming $50k/LY, average net value of decedents, survivors and weighted net value = ", nvd50k, ", ", nvs50k, ", ", nv50k)
paste0("Assuming $100k/LY, average net value of decedents, survivors and weighted net value = ", nvd100k, ", ", nvs100k, ", ", nv100k)
paste0("Assuming $200k/LY, average net value of decedents, survivors and weighted net value = ", nvd200k, ", ", nvs200k, ", ", nv200k)
