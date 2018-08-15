library(data.table); library(dplyr)

# load required dataset
nv <- readRDS("9 net value.rds")

# Net value before standardisation
mean(nv$nv_50k)
mean(nv$nv_100k)
mean(nv$nv_200k)

# classify participants by age groups and gender
df <- mutate(nv, group = cut(age, breaks=c(seq(from = 14, to = 84, by = 5), Inf), labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))) %>% as.data.table
df <- df[, .(nv_50k = mean(nv_50k), nv_100k = mean(nv_100k), nv_200k = mean(nv_200k), diff.spend = mean(diff.spend.mod)), keyby = .(group, female)]

# Net Value 2: Age- and sex-standardized population, using Hong Kong diabetes patient reference population for adults up to age 79 (for comparability with Japan sample)
# Net Value 3: Age- and sex-standardized population, using WHO world population referencepopulation (modified for adult population 15 - 85+ years old)

df$hkdm <- c(0.001340, 0.000997, 0.001315, 0.001293, 0.001794, 0.002307, 0.003605, 0.004583, 0.007690, 0.008642, 0.016029, 0.014628, 0.029435, 0.023546, 0.059159, 0.046097, 0.082705, 0.068599, 0.090264, 0.081155, 0.090036, 0.084927, 0.070938, 0.066088, 0.067006, 0.075821, 0, 0, 0, 0)

df$who <- rep(c(0.057318597, 0.055626862, 0.053664342, 0.051498732, 0.048386049, 0.0445964, 0.040874449, 0.03634014, 0.03079106, 0.025174285, 0.020031384, 0.014955503, 0.010286478, 0.006158347, 0.004297372), each = 2)

df$nv_50k_hkdm <- df$nv_50k * df$hkdm
df$nv_100k_hkdm <- df$nv_100k * df$hkdm
df$nv_200k_hkdm <- df$nv_200k * df$hkdm

df$nv_50k_who <- df$nv_50k * df$who
df$nv_100k_who <- df$nv_100k * df$who
df$nv_200k_who <- df$nv_200k * df$who

# Overall weighted net value, standardised to HKDM reference population
sum(df$nv_50k_hkdm) # assuming value of 1 life-year = 50k
sum(df$nv_100k_hkdm) # assuming value of 1 life-year = 100k
sum(df$nv_200k_hkdm) # assuming value of 1 life-year = 200k

# Overall weighted net value, standardised to WHO reference population
sum(df$nv_50k_who) # assuming value of 1 life-year = 50k
sum(df$nv_100k_who) # assuming value of 1 life-year = 100k
sum(df$nv_200k_who) # assuming value of 1 life-year = 200k