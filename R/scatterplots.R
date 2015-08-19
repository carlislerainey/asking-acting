# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)


# ---------------- #
# state names data #
# ---------------- #

# load data 
d <- readRDS("data/budget-tall.RData")


gg <- ggplot(d, aes(x = estimated_imbalance, y = action)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~ class) +
  labs(title = "Standardized Enacted Revenue Changes by Estimated Imbalance and Citizen Ideology",
       x = "Estimated Imbalance",
       y = "Standardized Tax Change") + 
  theme_bw()

ggsave("doc/figs/scatterplots.png", gg, height = 8, width = 10)

