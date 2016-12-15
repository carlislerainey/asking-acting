# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# install old version of ggplot so that titles match other plots for AJPS submission
devtools::install_github("tidyverse/ggplot2", ref = "338e82a4052c9f2298ad5b072562c184d7161fec")

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)


# ---------------- #
# state names data #
# ---------------- #

# load data 
d <- readRDS("data/budget-tall.RData")
d$change[d$action > 0] <- "Increase"
d$change[d$action == 0] <- "No Change"
d$change[d$action < 0] <- "Decrease"
d$change <- factor(d$change, levels = c("Decrease", "No Change", "Increase"))


100*table(d$change)/length(d$change)

incdec_df <- summarize(group_by(d, class), incdec = sum(change == "Increase")/sum(change == "Decrease"),
                       perc_changed = 100*(1 - sum(change == "No Change")/length(change)))
gg <- ggplot(d, aes(x = change)) +
  geom_bar(width = .5) + 
  facet_wrap(~ class) +
  labs(title = "Direction of Enacted Revenue Changes",
       x = "Direction of Change",
       y = "Count") + 
  geom_text(data = incdec_df, aes(label = paste("Inc./Dec. =", round(incdec, 1), "\n% Changed =", round(perc_changed, 0))), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.4, size = 3) +  
  theme_bw()
ggsave("doc/figs/barplot.png", gg, height = 8, width = 10)




gg <- ggplot(d, aes(x = estimated_imbalance, y = action)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~ class) +
  labs(title = "Standardized Enacted Revenue Changes by Estimated Imbalance and Citizen Ideology",
       x = "Estimated Imbalance",
       y = "Standardized Tax Change") + 
  theme_bw()

ggsave("doc/figs/scatterplots.png", gg, height = 8, width = 10)



