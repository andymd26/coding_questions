rm(list = ls())
options(scipen=999)
options(digits=15)
install.packages("earth")
install.packages("caret")
install.packages("bbmle")
install.packages("dplyr")
install.packages("emdbook")
require(bbmle)
require(earth)
require(dplyr)
require(caret)
require(emdbook)

path = "C:/Users/ablohm/Documents/earth_network/data/processed/"
setwd(path)

data.load = readRDS(file = "ercot_load.rds")
LL = function(param, data) {
  param[1]=mu
  param[2]=sigma
  data[[1]]=load
  R = dlnorm(load, mu, sigma)
  loglik = -sum(log(R))
  return(loglik)
} 

LL = function(mu, sigma) {
  R = suppressWarnings(dlnorm(a$load, mu, sigma))
  loglik = -sum(log(R))
  return(loglik)
}
mle.func = function(data) {
  model = mle2(LL(data=data), start=(list(mu=7, sigma=1)), data=list(data))
  b = data.frame(mu.lb = a[1], mu.ub=a[2], sigma.lb=a[3], sigma.ub=a[4])
  return(b)
}

data.load = data.load %>%
  filter(zone=="north") %>%
  filter(as.numeric(format(date, "%m")) >= 6 & as.numeric(format(date, "%m")) <= 9) %>%
  filter(format(date, "%H") >= 15 & format(date, "%H") <= 22) %>%
  filter(weekdays(date)!= "Saturday" & weekdays(date)!="Sunday") %>%
  mutate(month_group = ifelse(as.numeric(format(date, "%m"))==6 | as.numeric(format(date, "%m"))==9,1,2)) %>%
  mutate(hour = format(date, "%H")) %>%
  group_by(month_group) %>%
  #group_by(month_group, hour) %>%
  mutate(mu.lb=confint(mle2(LL, start=(list(mu=mean(log(.$load)), sigma=sd(log(.$load)))), data=.))[1])
  do(confint(mle2(LL(mu1=7, sigma1=0.5,data1=.), start=(list(mu=mu, sigma=sigma)))))

a = data.load
  
  


  -sum(log(dlnorm(data1, mu, sigma)))

%>%
  do(head(.))
  mutate(mu = suppressWarnings(confint(mle2(LL, start=(list(mu=mean(log(load)), sigma=sd(log(load)))))))[1])
  
  
# The above code filters out nonpeak load hours, as well as weekends.
# The load data is in the format 'hour ending' so the code above selects the hours from 2pm to 9pm

  
marsFit = earth(data.load$date, data.load$load)
marsGrid = expand.grid(.degree=1:2, .nprune=2:5)
set.seed(100)
marsTuned = train(data.load$date, data.load$load, method="earth",
                tuneGrid = marsGrid, 
                trControl = trainControl(method="cv"))

x1 <- rbetabinom(n=1000,prob=0.1,size=50,theta=10)
m0f <- mle2(x1~dbetabinom(prob,size=50,theta),
            start=list(prob=0.2,theta=9),data=data.frame(x1))
m0cf <- mle2(x1~dbetabinom(prob=plogis(lprob),size=50,theta=exp(ltheta)),
             start=list(lprob=0,ltheta=2),data=data.frame(x1))
lprob
load(system.file("vignetteData","orob1.rda",package="bbmle"))
ML1 <- function(prob1,prob2,prob3,theta,x) {
  prob <- c(prob1,prob2,prob3)[as.numeric(x$dilution)]
  size <- x$n
  -sum(dbetabinom(x$m,prob,size,theta,log=TRUE))
}
m1 <- mle2(ML1,start=list(prob1=0.5,prob2=0.5,prob3=0.5,theta=1),
           data=list(x=orob1))
