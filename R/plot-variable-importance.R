
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)

# load data
vis_df <- readRDS("output/variable-importance.RData")

# plot variable importance
gg <- ggplot(vis_df, aes(x = expl_var_name, y = importance)) + 
  geom_bar(stat = "identity", width = .05, color = "grey70") + 
  geom_point() + 
  #geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  facet_wrap(~ out_var_name) +
  labs(title = "Variable Importance of Potential Explanatory Variables Across the Nine Outcomes",
       y = "Variable Importance",
       x = "") + 
  theme_bw()
scale <- 3
ggsave("doc/figs/variable-importance.png", height = 3*scale, width = 4*scale)

