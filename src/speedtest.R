path.processed = ""
setwd(path.processed)

dist.price = readRDS(file = "list_logrnml_price.rds")
dist.load = readRDS(file = "list_logrnml_load.rds")

data.mc = data.frame(
  month_group = round(runif(n, min=0.5, max=2.5)),
  hour = round(runif(n, min=14.5, max=22.5)),
  duration = round(runif(n, min=-0.5, max= 4.5))) 

start.time = Sys.time()

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

end.time = Sys.time()
time.taken = end.time - start.time
time.taken
