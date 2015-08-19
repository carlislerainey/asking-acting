# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# sink output
# sink("output.txt")

# options
subset_data <- FALSE
ntree <- 2500
n_values <- 30

# run code
system.time ({
  source("R/clean-data.R")
  source("R/random-forests.R")
  source("R/plot-variable-importance.R")
  source("R/plot-predictions.R")  
  source("R/plot-pc-estimb.R")  
  source("R/predictive-performance.R")
  source("R/plot-example-data.R")
  source("R/scatterplots.R")
})

# beep when done
beepr::beep("facebook")

