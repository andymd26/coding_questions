install.packages("dplyr")

rm(list=ls())
options(digits=15)
options(scipen=999)

require(dplyr)

path.processed = "C:/Users/ablohm/Documents/earth_network/data/processed/"
path.results = "C:/Users/ablohm/Documents/earth_network/data/results/"
setwd(path.processed)

dist.price.v2 = readRDS(file = "logrnml_price.rds")
dist.price.v2 = dist.price.v2 %>%
  select(., month_group, hour, mu.lb.price, mu.ub.price, sigma.lb.price, sigma.ub.price)

start.time.m2 = Sys.time()
gc()
n = 100000
k = 2
# Size of simulation
customer = data.frame(customer_group = seq(from=1, to= k, by= 1),
                     cs_size = c(4000, 4000))
# Number of participants in the DR event
data.mc = data.frame(month_group = matrix(round(runif(n*k, min= 0.5, max= 2.5)), ncol= k, nrow= n),
                     hour = matrix(round(runif(n*k, min= 14.5, max= 22.5)), ncol= k, nrow= n),
                     duration = matrix(round(runif(n*k, min= -0.5, max= 4.5)), ncol= k, nrow= n))
# Customer classes: Issue ensure that the same load an price are used within each sample across customer classes. I think
# that the only way to do this is to simulate 24 hours of temperature and load of dimension n. Then use this as input to 
# the rest of the analysis. Because there is significant probability of an overlap between DR events of different customer 
# classes at some point during the DR event. Would need to create a temperature, load, and price sample for each month_group
# of dimension n.

start.time.m2 = Sys.time()
price = data.frame(hour = matrix(0, nrow=n, ncol= 24)) %>%
  mutate(hour.1 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[1], max = dist.price.v2$mu.ub.price[1]), 
                           sdlog = runif(n, min = dist.price.v2$sigma.lb.price[1], max = dist.price.v2$sigma.ub.price[1]))) %>%
  mutate(hour.2 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[2], max = dist.price.v2$mu.ub.price[2]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[2], max = dist.price.v2$sigma.ub.price[2]))) %>%
  mutate(hour.3 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[3], max = dist.price.v2$mu.ub.price[3]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[3], max = dist.price.v2$sigma.ub.price[3]))) %>%
  mutate(hour.4 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[4], max = dist.price.v2$mu.ub.price[4]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[4], max = dist.price.v2$sigma.ub.price[4]))) %>%
  mutate(hour.5 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[5], max = dist.price.v2$mu.ub.price[5]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[5], max = dist.price.v2$sigma.ub.price[5]))) %>%
  mutate(hour.6 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[6], max = dist.price.v2$mu.ub.price[6]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[6], max = dist.price.v2$sigma.ub.price[6]))) %>%
  mutate(hour.7 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[7], max = dist.price.v2$mu.ub.price[7]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[7], max = dist.price.v2$sigma.ub.price[7]))) %>%
  mutate(hour.8 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[8], max = dist.price.v2$mu.ub.price[8]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[8], max = dist.price.v2$sigma.ub.price[8]))) %>%
  mutate(hour.9 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[9], max = dist.price.v2$mu.ub.price[9]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[9], max = dist.price.v2$sigma.ub.price[9]))) %>%
  mutate(hour.10 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[10], max = dist.price.v2$mu.ub.price[10]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[10], max = dist.price.v2$sigma.ub.price[10]))) %>%
  mutate(hour.11 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[11], max = dist.price.v2$mu.ub.price[11]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[11], max = dist.price.v2$sigma.ub.price[11]))) %>%
  mutate(hour.12 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[12], max = dist.price.v2$mu.ub.price[12]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[12], max = dist.price.v2$sigma.ub.price[12]))) %>%
  mutate(hour.13 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[13], max = dist.price.v2$mu.ub.price[13]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[13], max = dist.price.v2$sigma.ub.price[13]))) %>%
  mutate(hour.14 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[14], max = dist.price.v2$mu.ub.price[14]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[14], max = dist.price.v2$sigma.ub.price[14]))) %>%
  mutate(hour.15 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[15], max = dist.price.v2$mu.ub.price[15]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[15], max = dist.price.v2$sigma.ub.price[15]))) %>%
  mutate(hour.16 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[16], max = dist.price.v2$mu.ub.price[16]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[16], max = dist.price.v2$sigma.ub.price[16]))) %>%
  mutate(hour.17 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[17], max = dist.price.v2$mu.ub.price[17]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[17], max = dist.price.v2$sigma.ub.price[17]))) %>%
  mutate(hour.18 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[18], max = dist.price.v2$mu.ub.price[18]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[18], max = dist.price.v2$sigma.ub.price[18]))) %>%
  mutate(hour.19 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[19], max = dist.price.v2$mu.ub.price[19]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[19], max = dist.price.v2$sigma.ub.price[19]))) %>%
  mutate(hour.20 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[20], max = dist.price.v2$mu.ub.price[20]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[20], max = dist.price.v2$sigma.ub.price[20]))) %>%
  mutate(hour.21 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[21], max = dist.price.v2$mu.ub.price[21]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[21], max = dist.price.v2$sigma.ub.price[21]))) %>%
  mutate(hour.22 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[22], max = dist.price.v2$mu.ub.price[22]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[22], max = dist.price.v2$sigma.ub.price[22]))) %>%
  mutate(hour.23 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[23], max = dist.price.v2$mu.ub.price[23]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[23], max = dist.price.v2$sigma.ub.price[23]))) %>%
  mutate(hour.24 = rlnorm(n, meanlog = runif(n, min = dist.price.v2$mu.lb.price[24], max = dist.price.v2$mu.ub.price[24]), 
                         sdlog = runif(n, min = dist.price.v2$sigma.lb.price[24], max = dist.price.v2$sigma.ub.price[24]))) %>%
  rowwise() %>%
  mutate(price.list = list(c(hour.1, hour.2, hour.3, hour.4, hour.5, hour.6, hour.7, hour.8, hour.9, hour.10, hour.11, hour.12,
                             hour.13, hour.14, hour.15, hour.16, hour.17, hour.18, hour.19, hour.20, hour.21, hour.22, hour.23,
                             hour.24))) %>%
  ungroup() 

end.time.m2 = Sys.time()
time.taken = difftime(end.time.m2, start.time.m2, units = "secs")
View(time.taken)

  
hour = matrix(round(runif(n, min= 14.5, max= 22.5)), ncol= k, nrow= n)
duration = matrix(round(runif(n, min= -0.5, max= 4.5)), ncol= k, nrow= n)


data.mc = data.frame(month_group = month_group, 
                     hour = hour, 
                     duration = duration)
  rep(month_group = round(runif(n, min= 0.5, max= 2.5)),2)),
  hour = round(runif(n, min= 14.5, max= 22.5)),
  # Eventually could open this up to all hours (to fully explore the solution space)
  duration = round(runif(n, min= -0.5, max= 4.5))) 






dist.load.v2 = readRDS(file = "logrnml_load.rds")
dist.temp = readRDS(file = "logrnml_temp.rds")
coef.removed = readRDS(file="removed_kwh_reg.rds")
coef.recovery = readRDS(file = "recovery_kwh_reg.rds")

dist.load.v2 = dist.load.v2 %>%
  select(., month_group, hour, mu.lb.load, mu.ub.load, sigma.lb.load, sigma.ub.load)
dist.temp = dist.temp %>%
  select(., month_group, hour, mu.lb.temp, mu.ub.temp, sigma.lb.temp, sigma.ub.temp)
# This code could be moved to the creation of these files (and done once there). We could then import 
# the new file instead of the current approach of importing and modifying.
