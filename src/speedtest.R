install.packages("dplyr")

rm(list=ls())
options(digits=15)
options(scipen=999)
require(dplyr)

path.processed = "C:/Users/ablohm/Documents/earth_network/data/processed/"
# Change to the folder with the list_logrnml.rds dafile 
setwd(path.processed)

dist.price.v2 = readRDS(file = "logrnml_price.rds")
dist.load.v2 = readRDS(file = "logrnml_load.rds")
dist.price = readRDS(file = "list_logrnml_price.rds")
dist.load = readRDS(file = "list_logrnml_load.rds")
dist.price.v2 = readRDS(file="logrnml_price.rds")
dist.load.v2 = readRDS(file="logrnml_load.rds")
# Model 1 uses the first set of inputs; model 2 the second set.

time.taken = data.frame(n = c(1000, 5000000, 10000000),
                        model.1 = c(0,0,0),
                        model.2 = c(0,0,0))
n= 1000000

start.time = Sys.time()
# Sample size
data.mc = data.frame(
  month_group = round(runif(n, min= 0.5, max= 2.5)),
  hour = round(runif(n, min= 14.5, max= 22.5)),
  duration = round(runif(n, min= -0.5, max= 4.5))) 

data.mc = data.mc %>%
  left_join(dist.price, by = "month_group") %>%
  # Joins the list structure we created based on the month_group variable
  mutate(hour.temp = hour + 1) %>%
  # To match up the indices
  rowwise() %>%
  mutate(price.t0 = rlnorm(1, meanlog= runif(1, min=price.list[[hour.temp]][2], max=price.list[[hour.temp]][3]),
                           sdlog = runif(1, min=price.list[[hour.temp]][4], max=price.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(price.t1 = rlnorm(1, meanlog= runif(1, min=price.list[[hour.temp]][2], max=price.list[[hour.temp]][3]),
                           sdlog = runif(1, min=price.list[[hour.temp]][4], max=price.list[[hour.temp]][5])))  %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(price.t2 = rlnorm(1, meanlog= runif(1, min=price.list[[hour.temp]][2], max=price.list[[hour.temp]][3]),
                           sdlog = runif(1, min=price.list[[hour.temp]][4], max=price.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(price.t3 = rlnorm(1, meanlog= runif(1, min=price.list[[hour.temp]][2], max=price.list[[hour.temp]][3]),
                           sdlog = runif(1, min=price.list[[hour.temp]][4], max=price.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(price.t4 = rlnorm(1, meanlog= runif(1, min=price.list[[hour.temp]][2], max=price.list[[hour.temp]][3]),
                           sdlog = runif(1, min=price.list[[hour.temp]][4], max=price.list[[hour.temp]][5]))) %>%
  mutate(price.t.list = list(c(price.t0, price.t1, price.t2, price.t3, price.t4))) %>%
  mutate(price.avg = mean(price.t.list[1:duration])) %>%
  mutate(price.end = price.t.list[duration + 1]) %>%
  select(., month_group, hour.temp, hour, duration, price.list, price.t.list, price.avg, price.t0, price.end) %>%
  # Determines the hour that the load recovered reenters load demand (i.e., the hour following the DR event).
  # Given that load and price are for hour ending a one hour event scheduled for 14:00 would have the load recovery
  left_join(dist.load, by = "month_group") %>%
  # Joins the list structure we created based on the month_group variable
  mutate(hour.temp = hour + 1) %>%
  # To match up the indices
  mutate(load.t0 = rlnorm(1, meanlog= runif(1, min=load.list[[hour.temp]][2], max=load.list[[hour.temp]][3]),
                           sdlog = runif(1, min=load.list[[hour.temp]][4], max=load.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(load.t1 = rlnorm(1, meanlog= runif(1, min=load.list[[hour.temp]][2], max=load.list[[hour.temp]][3]),
                           sdlog = runif(1, min=load.list[[hour.temp]][4], max=load.list[[hour.temp]][5])))  %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(load.t2 = rlnorm(1, meanlog= runif(1, min=load.list[[hour.temp]][2], max=load.list[[hour.temp]][3]),
                           sdlog = runif(1, min=load.list[[hour.temp]][4], max=load.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(load.t3 = rlnorm(1, meanlog= runif(1, min=load.list[[hour.temp]][2], max=load.list[[hour.temp]][3]),
                           sdlog = runif(1, min=load.list[[hour.temp]][4], max=load.list[[hour.temp]][5]))) %>%
  mutate(hour.temp = ifelse(hour.temp < 24, hour.temp + 1, hour.temp + 1 - 23)) %>%
  mutate(load.t4 = rlnorm(1, meanlog= runif(1, min=load.list[[hour.temp]][2], max=load.list[[hour.temp]][3]),
                           sdlog = runif(1, min=load.list[[hour.temp]][4], max=load.list[[hour.temp]][5]))) %>%
  mutate(load.t.list = list(c(load.t0, load.t1, load.t2, load.t3, load.t4))) %>%
  mutate(load.avg = mean(load.t.list[1:duration])) %>%
  mutate(load.end = load.t.list[duration + 1]) %>%
  select(., month_group, hour, hour.temp, duration, price.list, price.t.list, price.avg, price.t0, price.end,
         load.list, load.t.list, load.avg, load.t0, load.end)
# This first approach uses a structured list to store information on the parameters of the load and price distribution parameters for additional later sampling operations. 

end.time = Sys.time()
time.taken[2, 2] = difftime(end.time, start.time, units= "secs")

# Second model
dist.price.v2 = dist.price.v2 %>%
  select(., month_group, hour, mu.lb.price, mu.ub.price, sigma.lb.price, sigma.ub.price)
dist.load.v2 = dist.load.v2 %>%
  select(., month_group, hour, mu.lb.load, mu.ub.load, sigma.lb.load, sigma.ub.load)

start.time.m2 = Sys.time()
data.mc = data.frame(
  month_group = round(runif(n, min= 0.5, max= 2.5)),
  hour = round(runif(n, min= 14.5, max= 22.5)),
  duration = round(runif(n, min= -0.5, max= 4.5))) 

data.mc = data.mc %>%
  mutate(hour.original = hour) %>%
  # Price portion
  left_join(dist.price.v2, by = c("month_group","hour")) %>%
  mutate(price.t0 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., -(mu.lb.price:sigma.ub.price)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.price.v2, by = c("month_group","hour")) %>%
  mutate(price.t1 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., -(mu.lb.price:sigma.ub.price)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.price.v2, by = c("month_group","hour")) %>%
  mutate(price.t2 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., -(mu.lb.price:sigma.ub.price)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.price.v2, by = c("month_group","hour")) %>%
  mutate(price.t3 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., -(mu.lb.price:sigma.ub.price)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.price.v2, by = c("month_group","hour")) %>%
  mutate(price.t4 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., -(mu.lb.price:sigma.ub.price)) %>%
  rowwise() %>%
  mutate(price.list = list(c(price.t0, price.t1, price.t2, price.t3, price.t4))) %>%
  mutate(price.avg = mean(price.list[1:duration])) %>%
  mutate(price.end = price.list[duration + 1]) %>%
  ungroup() %>%
  # Cancels out the rowwise()
  select(., -(price.t1:price.t4)) %>%
  # Load portion
  mutate(hour = hour.original) %>%
  left_join(dist.load.v2, by = c("month_group","hour")) %>%
  mutate(load.t0 = rlnorm(n, meanlog = runif(n, min = mu.lb.load, max = mu.ub.load), 
                           sdlog = runif(n, min = sigma.lb.load, max = sigma.ub.load))) %>%
  select(., -(mu.lb.load:sigma.ub.load)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.load.v2, by = c("month_group","hour")) %>%
  mutate(load.t1 = rlnorm(n, meanlog = runif(n, min = mu.lb.load, max = mu.ub.load), 
                           sdlog = runif(n, min = sigma.lb.load, max = sigma.ub.load))) %>%
  select(., -(mu.lb.load:sigma.ub.load)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.load.v2, by = c("month_group","hour")) %>%
  mutate(load.t2 = rlnorm(n, meanlog = runif(n, min = mu.lb.load, max = mu.ub.load), 
                          sdlog = runif(n, min = sigma.lb.load, max = sigma.ub.load))) %>%
  select(., -(mu.lb.load:sigma.ub.load)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.load.v2, by = c("month_group","hour")) %>%
  mutate(load.t3 = rlnorm(n, meanlog = runif(n, min = mu.lb.load, max = mu.ub.load), 
                          sdlog = runif(n, min = sigma.lb.load, max = sigma.ub.load))) %>%
  select(., -(mu.lb.load:sigma.ub.load)) %>%
  mutate(hour = ifelse(hour < 23, hour + 1, hour + 1 - 24)) %>%
  left_join(dist.load.v2, by = c("month_group","hour")) %>%
  mutate(load.t4 = rlnorm(n, meanlog = runif(n, min = mu.lb.load, max = mu.ub.load), 
                          sdlog = runif(n, min = sigma.lb.load, max = sigma.ub.load))) %>%
  select(., -(mu.lb.load:sigma.ub.load)) %>%
  rowwise() %>%
  mutate(load.list = list(c(load.t0, load.t1, load.t2, load.t3, load.t4))) %>%
  mutate(load.avg = mean(load.list[1:duration])) %>%
  mutate(load.end = load.list[duration + 1]) %>%
  ungroup() %>%
  select(., -(load.t1:load.t4)) 
  
end.time.m2 = Sys.time()
time.taken[1, 3] = difftime(end.time.m2, start.time.m2, units = "secs")

View(time.taken)
