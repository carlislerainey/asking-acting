
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)
library(party)
library(dplyr)
library(tidyr)


# load data and listwise delete
d <- readRDS("data/budget.RData")
d <- na.omit(d)


# ------------- #
# preliminaries #
# ------------- #

out_vars <- c("leg_total")
out_var_names <- c("Total Taxes")
expl_vars <- c("gov_party", 
               "change_in_gov_party", 
               "house_dem_share", 
               "change_in_house_dem_share", 
               "senate_dem_share", 
               "change_in_senate_dem_share",
               "citizen_ideology", 
               "taxes_last_year", 
               "personal_income", 
               "population", 
               "estimated_imbalance", 
               "percent_change_personal_income", 
               "percent_change_population")
expl_var_names <- c("Governor's Party", 
                    "Change in Governor's Party",
                    "Democrats' Share of House", 
                    "Change in Democrats' Share of House",
                    "Democrats' Share of Senate", 
                    "Change in Democrats' Share of Senate",
                    "Citizen Ideology", 
                    "Taxes Last Year", 
                    "Personal Income", 
                    "Population", 
                    "Estimated Imbalance", 
                    "Percent Change in Personal Income", 
                    "Percent Change in Population")

# create formula
f <- as.formula(paste(out_vars, "~", paste(expl_vars, collapse=" + ")))

# reduce data to only variables used
d <- d[, c("state", "state_abbr", "year", out_vars, expl_vars)]

# ------ #
# models #
# ------ #

# split into test and training sets
set.seed(7192172)
train <- rbinom(nrow(d), 1, 0.7)
d$training[train == 1] <- "Training Set"
d$training[train == 0] <- "Test Set"
d$training <- factor(d$training, levels = c("Training Set", "Test Set"))

# estimate models
rf <- cforest(f, data = subset(d, training = "Training Set"), control = cforest_unbiased(ntree = 1500, mtry = 4))
ls <- lm(f, data = subset(d, training = "Training Set"))

# make in-sample and out-sample predictions
d$rf_pred <- predict(rf, newdata = d)
d$ls_pred <- predict(ls, newdata = d)

# calculate in-sample and out-sample mse
1 - with(subset(d, d$training == "Training Set"), mean((rf_pred - leg_total)^2))/with(subset(d, d$training == "Training Set"), mean((ls_pred - leg_total)^2))
1 - with(subset(d, d$training == "Test Set"), mean((rf_pred - leg_total)^2))/with(subset(d, d$training == "Test Set"), mean((ls_pred - leg_total)^2))


# heighten data set
d_tall <- gather(d, method, prediction, rf_pred, ls_pred)
d_tall$method <- as.character(d_tall$method)
d_tall$method[d_tall$method == "rf_pred"] <- "Random Forest"
d_tall$method[d_tall$method == "ls_pred"] <- "Least Squares"
d_tall$method <- factor(d_tall$method, levels = c("Random Forest", "Least Squares"))

# plot
gg <- ggplot(d_tall, aes(x = prediction, y = leg_total)) + 
  geom_point(alpha = 0.5) + 
  facet_grid (training ~ method) +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Comparing In-Sample and Out-of-Sample Predictions\nfor Random Forests and Least Squares",
       y = "Observed Revenue Change",
       x = "Predicted Revenue Change") +
  geom_abline(intercept = 0, slope = 1) + 
  theme_bw()
ggsave("doc/figs/rf-vs-ls-predictions.png", gg, height = 6, width = 8)
gg


mse_df <- data.frame(expand.grid(c("Training Set", "Test Set"), c("Random Forest", "Least Squares")))
names(mse_df) <- c("training", "method")
for (i in 1:4) {
  mse_df$mse[i] <- with(subset(d_tall, d$training == mse_df$training[i] &
                               d_tall$method == mse_df$method[i]), mean((prediction - leg_total)^2))
}

gg <- ggplot(mse_df, aes(x = method, y = mse)) + 
  geom_bar(stat = "identity", width = .5) + 
  facet_wrap(~ training) +
  labs(title = "Comparing In-Sample and Out-of-Sample Performance\nfor Random Forests and Least Squares",
       x = "Method",
       y = "Mean Squared Error") +
  theme_bw()
ggsave("doc/figs/rf-vs-ls-performance.png", gg, height = 3.5, width = 8)
gg
  