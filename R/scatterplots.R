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
d$change[d$action > 0] <- "Increase"
d$change[d$action == 0] <- "No Change"
d$change[d$action < 0] <- "Decrease"
d$change <- factor(d$change, levels = c("Increase", "No Change", "Decrease"))


incdec_df <- summarize(group_by(d, class), incdec = sum(change == "Increase")/sum(change == "Decrease"))
gg <- ggplot(d, aes(x = change)) +
  geom_bar(width = .5) + 
  facet_wrap(~ class) +
  labs(title = "Direction of Enacted Revenue Changes",
       x = "Direction of Change",
       y = "Count") + 
  geom_text(data = incdec_df, aes(label = paste("Inc./Dec. =", round(incdec, 1))), 
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



