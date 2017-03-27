rm(list=ls())
options(digits=15)
options(scipen=999)

require(parallel)

path.processed = "C:/Users/ablohm/Documents/earth_network/data/processed/"
path.results = "C:/Users/ablohm/Documents/earth_network/data/results/"
setwd(path.processed)

num_cores = detectCores() - 1
cl = makeCluster(num_cores)
 parLapply(cl, 2:4, function(exponent)
   2^exponent)

 stopCluster(cl)
 
