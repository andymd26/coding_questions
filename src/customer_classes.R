install.packages("dplyr")

rm(list=ls())
options(digits=15)
options(scipen=999)

require(dplyr)

path.processed = "C:/Users/ablohm/Documents/coding_questions/data/"
path.results = "C:/Users/ablohm/Documents/coding_questions/data/"
setwd(path.processed)

dist.price = readRDS(file = "logrnml_price.rds")
dist.load = readRDS(file = "logrnml_load.rds")
dist.temp = readRDS(file = "logrnml_temp.rds")
coef.removed = readRDS(file="removed_kwh_reg.rds")
coef.recovery = readRDS(file = "recovery_kwh_reg.rds")
# Necessary data inputs

dist.price = dist.price %>%
  select(., month_group, hour, mu.lb.price, mu.ub.price, sigma.lb.price, sigma.ub.price)
dist.load = dist.load %>%
  select(., month_group, hour, mu.lb.load, mu.ub.load, sigma.lb.load, sigma.ub.load)
dist.temp = dist.temp %>%
  select(., month_group, hour, mu.lb.temp, mu.ub.temp, sigma.lb.temp, sigma.ub.temp)
# This code could be moved to the creation of these files (and done once there). We could then import 
# the new file instead of the current approach of importing and modifying.
time.taken = data.frame(n = c(100000, 1000000, 5000000,10000000),
                        Seconds = NA)

gc()
# Frees up contiguous blocks of RAM for storing the larger matrices (otherwise R can indicate insuffienct memory
# when in fact more than enough RAM exists).
start.time.m2 = Sys.time()
n = 1000
k = 3
# Size of simulation

# The idea behind this code is that there are discrete blocks of code, a lot of which can be accomplished
# simultaneously (i.e., no dependence between them). To maintain consistency in the underlying assumptions 
# between customer classes (i.e., load, price, and weather) a 24 hour sample of each of these variables is
# created for each random draw (i.e., each simulation). All subsequent calculations for each simulation are 
# derived from this sample. The advantage of this approach extends beyond consistency, as there are discrete 
# activities that can be parallelized, greatly improving the execution speed. At this point the code has not
# been parallelized.
# To do:
#

data.mc = data.frame(month_group = matrix(round(runif(n*k, min= 0.5, max= 2.5)), ncol= 1, nrow= n),
                     hour = matrix(round(runif(n*k, min= 14.5, max= 22.5)), ncol= k, nrow= n),
                     duration = matrix(round(runif(n*k, min= -0.5, max= 4.5)), ncol= k, nrow= n))
data.mc = data.frame(month_group = matrix(round(runif(n*k, min= 0.5, max= 2.5)), ncol= 1, nrow= n))
# Generates a random sample of demand response events (i.e., hour and duration) during each month group.

# Generates a random sample of demand response events (i.e., hour and duration) during each month group.
data.mc = data.mc %>%
  group_by(month_group) %>%
  mutate(id = 1:length(month_group)) %>%
  ungroup()
kk = table(data.mc$month_group)
# Customer classes: Issue ensure that the same load an price are used within each sample across customer classes. I think
# that the only way to do this is to simulate 24 hours of temperature and load of dimension n. Then use this as input to 
# the rest of the analysis. Because there is significant probability of an overlap between DR events of different customer 
# classes at some point during the DR event. Would need to create a temperature, load, and price sample for each month_group
# of dimension n.

# Potentially: Assign to a cluster
dist.price.v2 = dist.price %>%
  filter(., month_group==1) 
price.m1 = data.frame(hour = matrix(0, nrow= kk[1], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 1) %>%
  mutate(hour.1 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[1], max = dist.price.v2$mu.ub.price[1]), 
                           sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[1], max = dist.price.v2$sigma.ub.price[1]))) %>%
  mutate(hour.2 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[2], max = dist.price.v2$mu.ub.price[2]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[2], max = dist.price.v2$sigma.ub.price[2]))) %>%
  mutate(hour.3 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[3], max = dist.price.v2$mu.ub.price[3]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[3], max = dist.price.v2$sigma.ub.price[3]))) %>%
  mutate(hour.4 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[4], max = dist.price.v2$mu.ub.price[4]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[4], max = dist.price.v2$sigma.ub.price[4]))) %>%
  mutate(hour.5 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[5], max = dist.price.v2$mu.ub.price[5]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[5], max = dist.price.v2$sigma.ub.price[5]))) %>%
  mutate(hour.6 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[6], max = dist.price.v2$mu.ub.price[6]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[6], max = dist.price.v2$sigma.ub.price[6]))) %>%
  mutate(hour.7 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[7], max = dist.price.v2$mu.ub.price[7]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[7], max = dist.price.v2$sigma.ub.price[7]))) %>%
  mutate(hour.8 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[8], max = dist.price.v2$mu.ub.price[8]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[8], max = dist.price.v2$sigma.ub.price[8]))) %>%
  mutate(hour.9 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[9], max = dist.price.v2$mu.ub.price[9]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[9], max = dist.price.v2$sigma.ub.price[9]))) %>%
  mutate(hour.10 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[10], max = dist.price.v2$mu.ub.price[10]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[10], max = dist.price.v2$sigma.ub.price[10]))) %>%
  mutate(hour.11 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[11], max = dist.price.v2$mu.ub.price[11]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[11], max = dist.price.v2$sigma.ub.price[11]))) %>%
  mutate(hour.12 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[12], max = dist.price.v2$mu.ub.price[12]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[12], max = dist.price.v2$sigma.ub.price[12]))) %>%
  mutate(hour.13 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[13], max = dist.price.v2$mu.ub.price[13]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[13], max = dist.price.v2$sigma.ub.price[13]))) %>%
  mutate(hour.14 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[14], max = dist.price.v2$mu.ub.price[14]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[14], max = dist.price.v2$sigma.ub.price[14]))) %>%
  mutate(hour.15 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[15], max = dist.price.v2$mu.ub.price[15]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[15], max = dist.price.v2$sigma.ub.price[15]))) %>%
  mutate(hour.16 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[16], max = dist.price.v2$mu.ub.price[16]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[16], max = dist.price.v2$sigma.ub.price[16]))) %>%
  mutate(hour.17 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[17], max = dist.price.v2$mu.ub.price[17]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[17], max = dist.price.v2$sigma.ub.price[17]))) %>%
  mutate(hour.18 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[18], max = dist.price.v2$mu.ub.price[18]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[18], max = dist.price.v2$sigma.ub.price[18]))) %>%
  mutate(hour.19 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[19], max = dist.price.v2$mu.ub.price[19]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[19], max = dist.price.v2$sigma.ub.price[19]))) %>%
  mutate(hour.20 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[20], max = dist.price.v2$mu.ub.price[20]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[20], max = dist.price.v2$sigma.ub.price[20]))) %>%
  mutate(hour.21 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[21], max = dist.price.v2$mu.ub.price[21]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[21], max = dist.price.v2$sigma.ub.price[21]))) %>%
  mutate(hour.22 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[22], max = dist.price.v2$mu.ub.price[22]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[22], max = dist.price.v2$sigma.ub.price[22]))) %>%
  mutate(hour.23 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[23], max = dist.price.v2$mu.ub.price[23]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[23], max = dist.price.v2$sigma.ub.price[23]))) %>%
  mutate(hour.24 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.price.v2$mu.lb.price[24], max = dist.price.v2$mu.ub.price[24]), 
                         sdlog = runif(kk[1], min = dist.price.v2$sigma.lb.price[24], max = dist.price.v2$sigma.ub.price[24]))) %>%
  rowwise() %>%
  mutate(price.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                             hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                             hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, price.list)

# Potentially: Assign to cluster 2
dist.price.v2 = dist.price %>%
  filter(., month_group == 2) 
price.m2 = data.frame(hour = matrix(0, nrow= kk[2], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 2) %>%
  mutate(hour.1 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[1], max = dist.price.v2$mu.ub.price[1]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[1], max = dist.price.v2$sigma.ub.price[1]))) %>%
  mutate(hour.2 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[2], max = dist.price.v2$mu.ub.price[2]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[2], max = dist.price.v2$sigma.ub.price[2]))) %>%
  mutate(hour.3 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[3], max = dist.price.v2$mu.ub.price[3]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[3], max = dist.price.v2$sigma.ub.price[3]))) %>%
  mutate(hour.4 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[4], max = dist.price.v2$mu.ub.price[4]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[4], max = dist.price.v2$sigma.ub.price[4]))) %>%
  mutate(hour.5 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[5], max = dist.price.v2$mu.ub.price[5]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[5], max = dist.price.v2$sigma.ub.price[5]))) %>%
  mutate(hour.6 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[6], max = dist.price.v2$mu.ub.price[6]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[6], max = dist.price.v2$sigma.ub.price[6]))) %>%
  mutate(hour.7 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[7], max = dist.price.v2$mu.ub.price[7]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[7], max = dist.price.v2$sigma.ub.price[7]))) %>%
  mutate(hour.8 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[8], max = dist.price.v2$mu.ub.price[8]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[8], max = dist.price.v2$sigma.ub.price[8]))) %>%
  mutate(hour.9 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[9], max = dist.price.v2$mu.ub.price[9]), 
                         sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[9], max = dist.price.v2$sigma.ub.price[9]))) %>%
  mutate(hour.10 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[10], max = dist.price.v2$mu.ub.price[10]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[10], max = dist.price.v2$sigma.ub.price[10]))) %>%
  mutate(hour.11 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[11], max = dist.price.v2$mu.ub.price[11]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[11], max = dist.price.v2$sigma.ub.price[11]))) %>%
  mutate(hour.12 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[12], max = dist.price.v2$mu.ub.price[12]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[12], max = dist.price.v2$sigma.ub.price[12]))) %>%
  mutate(hour.13 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[13], max = dist.price.v2$mu.ub.price[13]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[13], max = dist.price.v2$sigma.ub.price[13]))) %>%
  mutate(hour.14 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[14], max = dist.price.v2$mu.ub.price[14]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[14], max = dist.price.v2$sigma.ub.price[14]))) %>%
  mutate(hour.15 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[15], max = dist.price.v2$mu.ub.price[15]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[15], max = dist.price.v2$sigma.ub.price[15]))) %>%
  mutate(hour.16 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[16], max = dist.price.v2$mu.ub.price[16]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[16], max = dist.price.v2$sigma.ub.price[16]))) %>%
  mutate(hour.17 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[17], max = dist.price.v2$mu.ub.price[17]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[17], max = dist.price.v2$sigma.ub.price[17]))) %>%
  mutate(hour.18 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[18], max = dist.price.v2$mu.ub.price[18]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[18], max = dist.price.v2$sigma.ub.price[18]))) %>%
  mutate(hour.19 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[19], max = dist.price.v2$mu.ub.price[19]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[19], max = dist.price.v2$sigma.ub.price[19]))) %>%
  mutate(hour.20 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[20], max = dist.price.v2$mu.ub.price[20]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[20], max = dist.price.v2$sigma.ub.price[20]))) %>%
  mutate(hour.21 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[21], max = dist.price.v2$mu.ub.price[21]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[21], max = dist.price.v2$sigma.ub.price[21]))) %>%
  mutate(hour.22 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[22], max = dist.price.v2$mu.ub.price[22]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[22], max = dist.price.v2$sigma.ub.price[22]))) %>%
  mutate(hour.23 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[23], max = dist.price.v2$mu.ub.price[23]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[23], max = dist.price.v2$sigma.ub.price[23]))) %>%
  mutate(hour.24 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.price.v2$mu.lb.price[24], max = dist.price.v2$mu.ub.price[24]), 
                          sdlog = runif(kk[2], min = dist.price.v2$sigma.lb.price[24], max = dist.price.v2$sigma.ub.price[24]))) %>%
  rowwise() %>%
  mutate(price.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                             hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                             hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, price.list)

price.m = rbind(price.m1, price.m2)

# Load 
dist.load.v2 = dist.load %>%
  filter(., month_group==1) 
load.m1 = data.frame(hour = matrix(0, nrow= kk[1], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 1) %>%
  mutate(hour.1 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[1], max = dist.load.v2$mu.ub.load[1]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[1], max = dist.load.v2$sigma.ub.load[1]))) %>%
  mutate(hour.2 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[2], max = dist.load.v2$mu.ub.load[2]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[2], max = dist.load.v2$sigma.ub.load[2]))) %>%
  mutate(hour.3 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[3], max = dist.load.v2$mu.ub.load[3]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[3], max = dist.load.v2$sigma.ub.load[3]))) %>%
  mutate(hour.4 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[4], max = dist.load.v2$mu.ub.load[4]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[4], max = dist.load.v2$sigma.ub.load[4]))) %>%
  mutate(hour.5 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[5], max = dist.load.v2$mu.ub.load[5]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[5], max = dist.load.v2$sigma.ub.load[5]))) %>%
  mutate(hour.6 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[6], max = dist.load.v2$mu.ub.load[6]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[6], max = dist.load.v2$sigma.ub.load[6]))) %>%
  mutate(hour.7 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[7], max = dist.load.v2$mu.ub.load[7]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[7], max = dist.load.v2$sigma.ub.load[7]))) %>%
  mutate(hour.8 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[8], max = dist.load.v2$mu.ub.load[8]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[8], max = dist.load.v2$sigma.ub.load[8]))) %>%
  mutate(hour.9 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[9], max = dist.load.v2$mu.ub.load[9]), 
                         sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[9], max = dist.load.v2$sigma.ub.load[9]))) %>%
  mutate(hour.10 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[10], max = dist.load.v2$mu.ub.load[10]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[10], max = dist.load.v2$sigma.ub.load[10]))) %>%
  mutate(hour.11 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[11], max = dist.load.v2$mu.ub.load[11]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[11], max = dist.load.v2$sigma.ub.load[11]))) %>%
  mutate(hour.12 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[12], max = dist.load.v2$mu.ub.load[12]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[12], max = dist.load.v2$sigma.ub.load[12]))) %>%
  mutate(hour.13 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[13], max = dist.load.v2$mu.ub.load[13]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[13], max = dist.load.v2$sigma.ub.load[13]))) %>%
  mutate(hour.14 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[14], max = dist.load.v2$mu.ub.load[14]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[14], max = dist.load.v2$sigma.ub.load[14]))) %>%
  mutate(hour.15 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[15], max = dist.load.v2$mu.ub.load[15]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[15], max = dist.load.v2$sigma.ub.load[15]))) %>%
  mutate(hour.16 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[16], max = dist.load.v2$mu.ub.load[16]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[16], max = dist.load.v2$sigma.ub.load[16]))) %>%
  mutate(hour.17 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[17], max = dist.load.v2$mu.ub.load[17]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[17], max = dist.load.v2$sigma.ub.load[17]))) %>%
  mutate(hour.18 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[18], max = dist.load.v2$mu.ub.load[18]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[18], max = dist.load.v2$sigma.ub.load[18]))) %>%
  mutate(hour.19 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[19], max = dist.load.v2$mu.ub.load[19]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[19], max = dist.load.v2$sigma.ub.load[19]))) %>%
  mutate(hour.20 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[20], max = dist.load.v2$mu.ub.load[20]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[20], max = dist.load.v2$sigma.ub.load[20]))) %>%
  mutate(hour.21 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[21], max = dist.load.v2$mu.ub.load[21]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[21], max = dist.load.v2$sigma.ub.load[21]))) %>%
  mutate(hour.22 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[22], max = dist.load.v2$mu.ub.load[22]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[22], max = dist.load.v2$sigma.ub.load[22]))) %>%
  mutate(hour.23 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[23], max = dist.load.v2$mu.ub.load[23]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[23], max = dist.load.v2$sigma.ub.load[23]))) %>%
  mutate(hour.24 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.load.v2$mu.lb.load[24], max = dist.load.v2$mu.ub.load[24]), 
                          sdlog = runif(kk[1], min = dist.load.v2$sigma.lb.load[24], max = dist.load.v2$sigma.ub.load[24]))) %>%
  rowwise() %>%
  mutate(load.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                            hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                            hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, load.list)

# Potentially: Assign to cluster 2
dist.load.v2 = dist.load %>%
  filter(., month_group == 2) 
load.m2 = data.frame(hour = matrix(0, nrow= kk[2], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 2) %>%
  mutate(hour.1 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[1], max = dist.load.v2$mu.ub.load[1]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[1], max = dist.load.v2$sigma.ub.load[1]))) %>%
  mutate(hour.2 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[2], max = dist.load.v2$mu.ub.load[2]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[2], max = dist.load.v2$sigma.ub.load[2]))) %>%
  mutate(hour.3 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[3], max = dist.load.v2$mu.ub.load[3]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[3], max = dist.load.v2$sigma.ub.load[3]))) %>%
  mutate(hour.4 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[4], max = dist.load.v2$mu.ub.load[4]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[4], max = dist.load.v2$sigma.ub.load[4]))) %>%
  mutate(hour.5 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[5], max = dist.load.v2$mu.ub.load[5]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[5], max = dist.load.v2$sigma.ub.load[5]))) %>%
  mutate(hour.6 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[6], max = dist.load.v2$mu.ub.load[6]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[6], max = dist.load.v2$sigma.ub.load[6]))) %>%
  mutate(hour.7 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[7], max = dist.load.v2$mu.ub.load[7]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[7], max = dist.load.v2$sigma.ub.load[7]))) %>%
  mutate(hour.8 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[8], max = dist.load.v2$mu.ub.load[8]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[8], max = dist.load.v2$sigma.ub.load[8]))) %>%
  mutate(hour.9 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[9], max = dist.load.v2$mu.ub.load[9]), 
                         sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[9], max = dist.load.v2$sigma.ub.load[9]))) %>%
  mutate(hour.10 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[10], max = dist.load.v2$mu.ub.load[10]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[10], max = dist.load.v2$sigma.ub.load[10]))) %>%
  mutate(hour.11 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[11], max = dist.load.v2$mu.ub.load[11]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[11], max = dist.load.v2$sigma.ub.load[11]))) %>%
  mutate(hour.12 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[12], max = dist.load.v2$mu.ub.load[12]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[12], max = dist.load.v2$sigma.ub.load[12]))) %>%
  mutate(hour.13 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[13], max = dist.load.v2$mu.ub.load[13]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[13], max = dist.load.v2$sigma.ub.load[13]))) %>%
  mutate(hour.14 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[14], max = dist.load.v2$mu.ub.load[14]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[14], max = dist.load.v2$sigma.ub.load[14]))) %>%
  mutate(hour.15 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[15], max = dist.load.v2$mu.ub.load[15]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[15], max = dist.load.v2$sigma.ub.load[15]))) %>%
  mutate(hour.16 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[16], max = dist.load.v2$mu.ub.load[16]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[16], max = dist.load.v2$sigma.ub.load[16]))) %>%
  mutate(hour.17 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[17], max = dist.load.v2$mu.ub.load[17]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[17], max = dist.load.v2$sigma.ub.load[17]))) %>%
  mutate(hour.18 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[18], max = dist.load.v2$mu.ub.load[18]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[18], max = dist.load.v2$sigma.ub.load[18]))) %>%
  mutate(hour.19 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[19], max = dist.load.v2$mu.ub.load[19]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[19], max = dist.load.v2$sigma.ub.load[19]))) %>%
  mutate(hour.20 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[20], max = dist.load.v2$mu.ub.load[20]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[20], max = dist.load.v2$sigma.ub.load[20]))) %>%
  mutate(hour.21 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[21], max = dist.load.v2$mu.ub.load[21]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[21], max = dist.load.v2$sigma.ub.load[21]))) %>%
  mutate(hour.22 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[22], max = dist.load.v2$mu.ub.load[22]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[22], max = dist.load.v2$sigma.ub.load[22]))) %>%
  mutate(hour.23 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[23], max = dist.load.v2$mu.ub.load[23]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[23], max = dist.load.v2$sigma.ub.load[23]))) %>%
  mutate(hour.24 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.load.v2$mu.lb.load[24], max = dist.load.v2$mu.ub.load[24]), 
                          sdlog = runif(kk[2], min = dist.load.v2$sigma.lb.load[24], max = dist.load.v2$sigma.ub.load[24]))) %>%
  rowwise() %>%
  mutate(load.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                            hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                            hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, load.list)

load.m = rbind(load.m1, load.m2)

# Temperature
dist.temp.v2 = dist.temp %>%
  filter(., month_group==1) 
temp.m1 = data.frame(hour = matrix(0, nrow= kk[1], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 1) %>%
  mutate(hour.1 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[1], max = dist.temp.v2$mu.ub.temp[1]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[1], max = dist.temp.v2$sigma.ub.temp[1]))) %>%
  mutate(hour.2 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[2], max = dist.temp.v2$mu.ub.temp[2]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[2], max = dist.temp.v2$sigma.ub.temp[2]))) %>%
  mutate(hour.3 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[3], max = dist.temp.v2$mu.ub.temp[3]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[3], max = dist.temp.v2$sigma.ub.temp[3]))) %>%
  mutate(hour.4 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[4], max = dist.temp.v2$mu.ub.temp[4]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[4], max = dist.temp.v2$sigma.ub.temp[4]))) %>%
  mutate(hour.5 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[5], max = dist.temp.v2$mu.ub.temp[5]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[5], max = dist.temp.v2$sigma.ub.temp[5]))) %>%
  mutate(hour.6 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[6], max = dist.temp.v2$mu.ub.temp[6]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[6], max = dist.temp.v2$sigma.ub.temp[6]))) %>%
  mutate(hour.7 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[7], max = dist.temp.v2$mu.ub.temp[7]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[7], max = dist.temp.v2$sigma.ub.temp[7]))) %>%
  mutate(hour.8 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[8], max = dist.temp.v2$mu.ub.temp[8]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[8], max = dist.temp.v2$sigma.ub.temp[8]))) %>%
  mutate(hour.9 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[9], max = dist.temp.v2$mu.ub.temp[9]), 
                         sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[9], max = dist.temp.v2$sigma.ub.temp[9]))) %>%
  mutate(hour.10 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[10], max = dist.temp.v2$mu.ub.temp[10]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[10], max = dist.temp.v2$sigma.ub.temp[10]))) %>%
  mutate(hour.11 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[11], max = dist.temp.v2$mu.ub.temp[11]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[11], max = dist.temp.v2$sigma.ub.temp[11]))) %>%
  mutate(hour.12 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[12], max = dist.temp.v2$mu.ub.temp[12]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[12], max = dist.temp.v2$sigma.ub.temp[12]))) %>%
  mutate(hour.13 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[13], max = dist.temp.v2$mu.ub.temp[13]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[13], max = dist.temp.v2$sigma.ub.temp[13]))) %>%
  mutate(hour.14 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[14], max = dist.temp.v2$mu.ub.temp[14]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[14], max = dist.temp.v2$sigma.ub.temp[14]))) %>%
  mutate(hour.15 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[15], max = dist.temp.v2$mu.ub.temp[15]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[15], max = dist.temp.v2$sigma.ub.temp[15]))) %>%
  mutate(hour.16 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[16], max = dist.temp.v2$mu.ub.temp[16]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[16], max = dist.temp.v2$sigma.ub.temp[16]))) %>%
  mutate(hour.17 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[17], max = dist.temp.v2$mu.ub.temp[17]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[17], max = dist.temp.v2$sigma.ub.temp[17]))) %>%
  mutate(hour.18 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[18], max = dist.temp.v2$mu.ub.temp[18]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[18], max = dist.temp.v2$sigma.ub.temp[18]))) %>%
  mutate(hour.19 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[19], max = dist.temp.v2$mu.ub.temp[19]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[19], max = dist.temp.v2$sigma.ub.temp[19]))) %>%
  mutate(hour.20 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[20], max = dist.temp.v2$mu.ub.temp[20]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[20], max = dist.temp.v2$sigma.ub.temp[20]))) %>%
  mutate(hour.21 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[21], max = dist.temp.v2$mu.ub.temp[21]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[21], max = dist.temp.v2$sigma.ub.temp[21]))) %>%
  mutate(hour.22 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[22], max = dist.temp.v2$mu.ub.temp[22]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[22], max = dist.temp.v2$sigma.ub.temp[22]))) %>%
  mutate(hour.23 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[23], max = dist.temp.v2$mu.ub.temp[23]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[23], max = dist.temp.v2$sigma.ub.temp[23]))) %>%
  mutate(hour.24 = rlnorm(kk[1], meanlog = runif(kk[1], min = dist.temp.v2$mu.lb.temp[24], max = dist.temp.v2$mu.ub.temp[24]), 
                          sdlog = runif(kk[1], min = dist.temp.v2$sigma.lb.temp[24], max = dist.temp.v2$sigma.ub.temp[24]))) %>%
  rowwise() %>%
  mutate(temp.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                            hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                            hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, temp.list)

# Potentially: Assign to cluster 2
dist.temp.v2 = dist.temp %>%
  filter(., month_group == 2) 
temp.m2 = data.frame(hour = matrix(0, nrow= kk[2], ncol= 24)) %>%
  mutate(id = 1:length(hour.1)) %>%
  mutate(month_group = 2) %>%
  mutate(hour.1 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[1], max = dist.temp.v2$mu.ub.temp[1]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[1], max = dist.temp.v2$sigma.ub.temp[1]))) %>%
  mutate(hour.2 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[2], max = dist.temp.v2$mu.ub.temp[2]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[2], max = dist.temp.v2$sigma.ub.temp[2]))) %>%
  mutate(hour.3 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[3], max = dist.temp.v2$mu.ub.temp[3]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[3], max = dist.temp.v2$sigma.ub.temp[3]))) %>%
  mutate(hour.4 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[4], max = dist.temp.v2$mu.ub.temp[4]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[4], max = dist.temp.v2$sigma.ub.temp[4]))) %>%
  mutate(hour.5 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[5], max = dist.temp.v2$mu.ub.temp[5]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[5], max = dist.temp.v2$sigma.ub.temp[5]))) %>%
  mutate(hour.6 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[6], max = dist.temp.v2$mu.ub.temp[6]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[6], max = dist.temp.v2$sigma.ub.temp[6]))) %>%
  mutate(hour.7 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[7], max = dist.temp.v2$mu.ub.temp[7]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[7], max = dist.temp.v2$sigma.ub.temp[7]))) %>%
  mutate(hour.8 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[8], max = dist.temp.v2$mu.ub.temp[8]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[8], max = dist.temp.v2$sigma.ub.temp[8]))) %>%
  mutate(hour.9 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[9], max = dist.temp.v2$mu.ub.temp[9]), 
                         sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[9], max = dist.temp.v2$sigma.ub.temp[9]))) %>%
  mutate(hour.10 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[10], max = dist.temp.v2$mu.ub.temp[10]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[10], max = dist.temp.v2$sigma.ub.temp[10]))) %>%
  mutate(hour.11 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[11], max = dist.temp.v2$mu.ub.temp[11]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[11], max = dist.temp.v2$sigma.ub.temp[11]))) %>%
  mutate(hour.12 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[12], max = dist.temp.v2$mu.ub.temp[12]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[12], max = dist.temp.v2$sigma.ub.temp[12]))) %>%
  mutate(hour.13 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[13], max = dist.temp.v2$mu.ub.temp[13]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[13], max = dist.temp.v2$sigma.ub.temp[13]))) %>%
  mutate(hour.14 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[14], max = dist.temp.v2$mu.ub.temp[14]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[14], max = dist.temp.v2$sigma.ub.temp[14]))) %>%
  mutate(hour.15 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[15], max = dist.temp.v2$mu.ub.temp[15]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[15], max = dist.temp.v2$sigma.ub.temp[15]))) %>%
  mutate(hour.16 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[16], max = dist.temp.v2$mu.ub.temp[16]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[16], max = dist.temp.v2$sigma.ub.temp[16]))) %>%
  mutate(hour.17 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[17], max = dist.temp.v2$mu.ub.temp[17]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[17], max = dist.temp.v2$sigma.ub.temp[17]))) %>%
  mutate(hour.18 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[18], max = dist.temp.v2$mu.ub.temp[18]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[18], max = dist.temp.v2$sigma.ub.temp[18]))) %>%
  mutate(hour.19 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[19], max = dist.temp.v2$mu.ub.temp[19]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[19], max = dist.temp.v2$sigma.ub.temp[19]))) %>%
  mutate(hour.20 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[20], max = dist.temp.v2$mu.ub.temp[20]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[20], max = dist.temp.v2$sigma.ub.temp[20]))) %>%
  mutate(hour.21 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[21], max = dist.temp.v2$mu.ub.temp[21]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[21], max = dist.temp.v2$sigma.ub.temp[21]))) %>%
  mutate(hour.22 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[22], max = dist.temp.v2$mu.ub.temp[22]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[22], max = dist.temp.v2$sigma.ub.temp[22]))) %>%
  mutate(hour.23 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[23], max = dist.temp.v2$mu.ub.temp[23]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[23], max = dist.temp.v2$sigma.ub.temp[23]))) %>%
  mutate(hour.24 = rlnorm(kk[2], meanlog = runif(kk[2], min = dist.temp.v2$mu.lb.temp[24], max = dist.temp.v2$mu.ub.temp[24]), 
                          sdlog = runif(kk[2], min = dist.temp.v2$sigma.lb.temp[24], max = dist.temp.v2$sigma.ub.temp[24]))) %>%
  rowwise() %>%
  mutate(temp.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                            hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                            hour.24))) %>%
  ungroup() %>%
  select(., id, month_group, temp.list)

temp.m = rbind(temp.m1, temp.m2)

hour.mc = data.frame(hour = matrix(round(runif(n*k, min= 14.5, max= 22.5)), ncol= k, nrow= n))
duration.mc = data.frame(duration = matrix(round(runif(n*k, min= -0.5, max= 4.5)), ncol= k, nrow= n))
# Generates a random sample of demand response events (i.e., hour and duration) during each month group.

cs_size = 5000

a = data.mc %>%
  # Join but keep all columns of both but only rows of x
  left_join(., price.m, by= c("id", "month_group")) %>%
  left_join(., load.m, by= c("id", "month_group")) %>%
  left_join(., temp.m, by= c("id", "month_group")) %>%
  mutate(hour = round(runif(n, min= 14.5, max= 22.5))) %>%
  mutate(duration = round(runif(n, min= -0.5, max= 4.5))) %>%
  rowwise() %>%
  mutate(load.t0 = load.list[hour]) %>%
  mutate(load.avg = mean(load.list[hour:(hour+duration)], na.rm=TRUE)) %>%
  mutate(load.end = load.list[hour + duration + 1]) %>%
  mutate(price.t0 = price.list[hour]) %>%
  mutate(price.avg = mean(price.list[hour:(hour+duration)], na.rm=TRUE)) %>%
  mutate(price.end = price.list[hour + duration + 1]) %>%
  mutate(temp_f = temp.list[1]) %>%
  # The temperature data is not properly synched Hour 15 is in position one.
  mutate(temp_f.duration = temp_f*duration) %>%
  mutate(load.removed = 1*coef.removed$intercept + temp_f*coef.removed$temp_f + 
           duration*coef.removed$duration + temp_f.duration*coef.removed$temp_f.duration) %>%
  mutate(load.removed = ifelse(duration==0, 0, load.removed)) %>%
  mutate(load.removed.square = load.removed^2) %>%
  mutate(duration.square = duration^2) %>%
  mutate(load.recovered = 1*coef.recovery$intercept + temp_f*coef.recovery$temp_f + 
           duration*coef.recovery$duration + load.removed*coef.recovery$removed_kwh + 
           duration.square*coef.recovery$duration_square + 
           load.removed*coef.recovery$removed_square) %>%
  mutate(load.recovered = ifelse(duration==0, 0, load.recovered)) %>%
  # Ridiculous command but the rename command isn't working
  mutate(profit.0 = (load.t0*duration - load.removed*cs_size*(1/1000))*(90 - price.t0) + (load.t1 + load.recovered*cs_size*(1/1000))*(90 - price.t1)) %>%
  # This calculation of profit is the actual profit for each demand response simulation
  mutate(profit.1 = (load.t0 + load.t1)*(0.5)*duration*(90 - (price.t0 + price.t1)*(0.5))) %>%
  # This calculation of profit is the profit in the event that no action was taken (no Demand Response event implemented)
  mutate(profit.dr = profit.0 - profit.1)
  
end.time.m2 = Sys.time()
time.taken[4, 2] = difftime(end.time.m2, start.time.m2, units = "secs")
View(time.taken)

saveRDS(time.taken, paste0(path.results,"time.customer.classes.rds"), ascii=TRUE)

