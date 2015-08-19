
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)
library(dplyr)

# load list of list of data sets
load("output/predictions.RData")
data_to_join <- readRDS("data/budget.RData")[, c("state", "year", "citizen_ideology")]

# plot for each expl variable
for (i in 1:length(expl_vars)) {
#for (i in 6:6) {
  # combine tax classes into single data frame
  df <- NULL
  for (j in 1:length(out_vars)) {
    df <- rbind(df, pred_dfs[[j]][[expl_vars[i]]])
  }
  df <- left_join(df, data_to_join)
  # change out_var to factor with an nice ordering
  df$out_var <- factor(df$out_var, levels = out_var_names)
  # calcualte mean prediction at each value of the expl variable
  df_mean <- dplyr::summarize(group_by(df, value, out_var), mean = mean(prediction))
  # calculate the min-max first-differrence
  maxfd <- function(x) {
  	max(x) - min(x)
  }
  maxfd_df <- dplyr::summarize(group_by(df_mean, out_var), 
  														 maxfd = maxfd(mean),
  														 plot_at_y = mean[length(mean)],
  														 plot_at_x = value[length(value)])
  maxfd_df$plot_text <- paste("Max FD =", round(maxfd_df$maxfd, 2))
  
  # create title of plot
  title <- paste("Predicting Changes in Taxes with", expl_var_names[i])
  # draw plot
  gg <- ggplot(df, aes(x = value, y = prediction))
  if (expl_vars[i] == "citizen_ideology") {
    gg <- gg + geom_line(aes(group = case_id), color = "grey70", alpha = 0.5)
  }
  if (expl_vars[i] != "citizen_ideology") {
    gg <- gg + geom_line(aes(group = case_id, color = citizen_ideology), alpha = 0.5) + 
      scale_color_gradient2(low = "#a50026", 
                           mid = "#ffffbf",
                           high = "#313695",
                           midpoint = 50)
  } 
    gg <- gg + facet_wrap(~ out_var) + 
    labs(title = title,
         x = expl_var_names[i],
         y = "Predicted Change (Standardized)",
         color = "Citizen Ideology") +
  	geom_line(data = df_mean, aes(x = value, y = mean, group = out_var),
  						color = "black", size = 2) +
  	geom_text(data = maxfd_df, aes(label = plot_text, group = out_var), 
  						x = Inf, y = Inf, hjust = 1.1, vjust = 1.4, size = 3.5) + 
  	theme_bw()
  filename <- paste("doc/figs/", expl_vars[i], ".png", sep = "")
  #print(gg)
  scale <- 3
  ggsave(filename, height = 3*scale, width = 4*scale)
}



  

