install.packages("dplyr")

rm(list=ls())
options(digits=15)
options(scipen=999)
require(dplyr)

n=100
# Sample size
path.processed = "C:/Users/ablohm/Documents/earth_network/data/processed/"
# Change to the folder with the list_logrnml.rds dafile 
setwd(path.processed)

dist.price = readRDS(file = "list_logrnml_price.rds")
# The data has two columns: the first column is numeric (either 1 or 2) for the month_group. The second column is a list of lists.
# In another piece of code (not provided here) I estimate the information found in "list_logrnml_price.rds": the upper and lower bound of the lognormal parameters using MLE for each hour of the day (for each month_group). Thus for each hour we have logmu, upper and lower bound, and logsigma, upper and lower bound. Once I had these estimates I generated a new variable for each row that has a list structure (each list has five elements: hour, mu.ub, mu.lb, sigma.ub, and sigma.lb and there is a list for each hour of the day and each month_group). Next, I made a list of lists, whereby we previously had a column of lists, we now have two lists (one for each month_group). I did this for both month groups and saved it as the data.frame that was just imported. The data frame has two rows and two columns. If you wanted to know the log mean parameter of the lognormal distribution for midnight (hour zero but index one) during month group 2 you would do the following: dist.price$price.list[[2]][[1]][2] whereby the first [[2]] is the month_group variable, the second [[]] is the hour variable (but the index starts at 1), and the third [] is the place in the list. If my memory is correct I think that each list is ordered as hour, mu.lb, mu.ub, sigma.lb, sigma.ub.

data.mc = data.frame(
  month_group = round(runif(n, min=0.5, max=2.5)),
  hour = round(runif(n, min=14.5, max=22.5)),
  duration = round(runif(n, min=-0.5, max= 4.5))) 
# This code randomly selects a month grouping, hour, and a duration of a demand response event

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
  mutate(price.end = price.t.list[duration + 1])
