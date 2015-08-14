# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# parallel setup
library(foreach)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cores = 3)
getDoParWorkers()

# options
subset_data <- FALSE
ntree <- 1000
n_values <- 20

# run code
system.time ({
  source("R/clean-data.R")
  source("R/random-forests.R")
  source("R/plot-variable-importance.R")
  source("R/plot-predictions.R")  
})

# terminate cluster
stopCluster(cl)

# beep when done
beepr::beep("facebook")
