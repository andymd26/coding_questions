install.packages("dplyr")

rm(list=ls())
options(digits=15)
options(scipen=999)
require(dplyr)

path.processed = "C:/Users/ablohm/Documents/earth_network/data/processed/"
# Change to the folder with the list_logrnml.rds dafile 
setwd(path.processed)

dist.price = readRDS(file = "list_logrnml_price.rds")
dist.load = readRDS(file = "list_logrnml_load.rds")
dist.price.v2 = readRDS(file="logrnml_price.rds")
dist.load.v2 = readRDS(file="logrnml_load.rds")
# Model 1 uses the first set of inputs; model 2 the second set.

start.time = Sys.time()
n= 100
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
time.taken = data.frame(model.1 = 0,
                        model.2 = 0)
time.taken[1, 1] = end.time - start.time

start.time = Sys.time()
data.mc = data.frame(
  month_group = round(runif(n, min= 0.5, max= 2.5)),
  hour = round(runif(n, min= 14.5, max= 22.5)),
  duration = round(runif(n, min= -0.5, max= 4.5))) 

data.mc = data.mc %>%
  mutate(hour.end = hour + duration) %>% 
  mutate(hour.end = ifelse(hour.end >= 24, hour.end - 24, hour.end)) %>%
  mutate(hour.b = hour) %>%
  mutate(price.list = list(c(price.t0, price.t1, price.t2, price.t3))) %>%
  mutate(price.avg = mean(price.list[1:duration])) %>%  
  left_join(dist.price, by = c("month_group","hour")) %>%
  mutate(price.t0 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., month_group, hour.b, hour, duration, hour.end, price.t0) %>%
  mutate(hour = hour + 1) %>%
  left_join(dist.price, by = c("month_group","hour")) %>%
  mutate(price.t1 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., month_group, hour.b, hour, duration, hour.end, price.t0, price.t1) %>%
  mutate(hour = hour + 1) %>%
  mutate(hour = ifelse(hour>=24,hour-24, hour)) %>%
  left_join(dist.price, by = c("month_group","hour")) %>%
  mutate(price.t2 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., month_group, hour.b, hour, duration, hour.end, price.t0, price.t1, price.t2) %>%
  mutate(hour = hour + 1) %>%
  mutate(hour = ifelse(hour >= 24, hour-24, hour)) %>%
  left_join(dist.price, by = c("month_group","hour")) %>%
  mutate(price.t3 = rlnorm(n, meanlog = runif(n, min = mu.lb.price, max = mu.ub.price), 
                           sdlog = runif(n, min = sigma.lb.price, max = sigma.ub.price))) %>%
  select(., month_group, hour.b, hour, duration, hour.end, price.t0, price.t1, price.t2, price.t3) %>%
  
end.time = Sys.time()
time.taken = end.time - start.time
time.taken
    # dist.price = dist.price %>%
    #  select(., month_group, hour, mu.lb.price, mu.ub.price, sigma.lb.price, sigma.ub.price) %>%
    #  mutate(data = list(c(mu.lb.price, mu.ub.price, sigma.lb.price, sigma.ub.price)))
    
