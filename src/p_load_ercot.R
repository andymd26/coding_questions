install.packages("bbmle")
install.packages("dplyr")

rm(list = ls())
options(scipen=999)
options(digits=15)
require(bbmle)
require(dplyr)

path = "~/Documents/coding_questions/data/"
setwd(path)

mle.func.lnorm = function(data) {
  g = data$month_group[1]
  hr = data$hour[1]
  model = mle2(load~dlnorm(meanlog=mu, sdlog=sigma), start=(list(mu=mean(log(data$load)), sigma=sd(log(data$load)))), data=data)
  c = model@details$convergence
  ci.model = confint(model)
  # Convergence code 1 means convergence failed
  model.out = data.frame(group = g, hour = hr, convergence= c, mu.lb = ci.model[1], mu.ub = ci.model[3], 
                         sigma.lb = ci.model[2], sigma.ub = ci.model[4])
  return(model.out)
}

data.load = readRDS(file = "ercot_load.rds")
data.load = data.load %>%
  filter(zone=="north") %>%
  # Filters out a representative region from the data
  filter(as.numeric(format(date, "%m")) >= 6 & as.numeric(format(date, "%m")) <= 9) %>%
  # Filters out the summer months
  filter(format(date, "%H") >= 15 & format(date, "%H") <= 22) %>%
  # Filters out 2pm to 9pm (numbers are hour ending)
  # The above code filters out nonpeak load hours, as well as weekends.
  filter(weekdays(date)!= "Saturday" & weekdays(date)!="Sunday") %>%
  # Filters out weekdays
  mutate(month_group = ifelse(as.numeric(format(date, "%m"))==6 | as.numeric(format(date, "%m"))==9,1,2)) %>%
  # Not from my work but supposedly June and Septemer load profiles look similar, as do July and August
  mutate(hour = format(date, "%H")) %>%
  # Creates a new column hour for the group_by below (not necessary but creates a more attractive dataframe than the alternative).
  group_by(month_group, hour) %>%
  # Group_by is similar to subset. In this case I would like to calculate the mle parameters for the lognormal distribution (params = mu and sigma) for each hour (i.e., 2pm-9pm) for each month group (i.e., June/September and July/August).
  do(mle.func.lnorm(data=.))
  # The mle2 function requires two bits of information: log-likelihood function and initial values for all parameters in the log-likelihood function. It then optimizes the log-likelihood to find the most likely parameter estimates for the specified distribution. The issue is that mle2 returns an untidy object (if you've ever run a regression in R, mle2 produces similar output. 'confint' produces a n x 2 matrix (or dataframe, can't remember off the top of my head). Whereby n is the number of parameters being fit and the columns are the lower and upper bounds based on the alpha value (confidence level). Dplyr does not know what to do with a returned dataframe. One way to address this issue is to use the do() wrapper. Basically, for each group formed by the group_by command run the mle2, determine the confidence interval, and either add this data to the existing dataframe as new columns or generate a new dataframe. 
