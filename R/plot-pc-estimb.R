
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)
library(dplyr)

# load list of list of data sets
load("output/predictions.RData")
data_to_join <- readRDS("data/budget.RData")

# estimated imbalance by partisan control



for (i in 1:length(out_vars)) {
  df <- pred_dfs[[out_vars[i]]][["estimated_imbalance"]]
  df <- left_join(df, data_to_join)
  # house control
  df$house_control[df$house_dem_share > .5] <- "Democratic House"
  df$house_control[df$house_dem_share < .5] <- "Republican House"
  df$house_control[df$house_dem_share == .5] <- NA
  df$house_control <- factor(df$house_control, levels = c("Democratic House", "Republican House"))
  df <- df[!is.na(df$house_control), ]
  # senate control
  df$senate_control[df$senate_dem_share > .5] <- "Democratic Senate"
  df$senate_control[df$senate_dem_share < .5] <- "Republican Senate"
  df$senate_control[df$senate_dem_share == .5] <- NA
  df$senate_control <- factor(df$senate_control, levels = c("Democratic Senate", "Republican Senate"))
  df <- df[!is.na(df$senate_control), ]
  # calculate the mean
  df_mean <- dplyr::summarize(group_by(df, value, gov_party, house_control, senate_control), mean = mean(prediction))
  # plot
  title <- paste("Partisan Control and Responses to Imbalances with", out_var_names[i])
  gg <- ggplot(df, aes(x = value, y = prediction, color = gov_party)) + 
    geom_line(aes(group = case_id), alpha = 0.1) + 
    facet_grid(senate_control ~ house_control) + 
    scale_color_manual(values = c("#a50026", "#313695")) +
    labs(title = title, 
         x = "Estimated Imbalance",
         y = paste("Predicted Change in", out_var_names[i], "(Standardized)"),
         color = "Governor's Party") + 
    geom_line(data = df_mean, aes(x = value, y = mean, color = gov_party), size = 2) +
    theme_bw()
  print(gg)
  scale <- 2
  filename <- paste("doc/figs/estimated-imbalance-partisan-control-", out_vars[i], ".png", sep = "")
  ggsave(filename, height = 3*scale, width = 4*scale)
}

