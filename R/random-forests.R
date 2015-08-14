
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)
library(party)
library(dplyr)
library(tidyr)

# load data and listwise delete
d <- readRDS("data/budget.RData")
if (subset_data == TRUE) {
  set.seed(1234); d <- sample_n(d, 100)
}
d <- na.omit(d)


# ------------- #
# preliminaries #
# ------------- #

out_vars <- c("leg_total", "leg_sales", "leg_income", "leg_corporate",
              "leg_cigarette_tobacco", "leg_fuel", "leg_alcohol",
              "leg_fees", "leg_others")
out_var_names <- c("Total Taxes", "Sales Taxes", "Income Taxes", "Corporate Taxes",
                   "Cigarette and Tobacco Taxes", "Fuel Taxes", "Alcohol Taxes",
                   "Fees", "Other Taxes")
expl_vars <- c(#"gov_party", 
               "change_in_gov_party", 
               #"house_dem_share", 
               "change_in_house_dem_share", 
               #"senate_dem_share", 
               "change_in_senate_dem_share",
               "citizen_ideology", 
               #"change_in_citizen_ideology",
               "taxes_last_year", 
               #"personal_income", 
               #"population", 
               "estimated_imbalance", 
               "percent_change_personal_income", 
               "percent_change_population")
               #"year_of_biennium", 
               #"ranney_10yrs")
expl_var_names <- c(#"Governor's Party", 
                    "Change in Governor's Party",
                    #"Democrats' Share of House", 
                    "Change in Democrats' Share of House",
                    #"Democrats' Share of Senate", 
                    "Change in Democrats' Share of Senate",
                    "Citizen Ideology", 
                    #"Change in Citizen Ideology",
                    "Taxes Last Year", 
                    #"Personal Income", 
                    #"Population", 
                    "Estimated Imbalance", 
                    "Percent Change in Personal Income", 
                    "Percent Change in Population")
                    #"Year of Biennium", 
                    #"Ranney Index (10 Year)")
formulas <- list()
for (i in 1:length(out_vars)) {
  formulas[[i]] <- as.formula(paste(out_vars[i], "~", paste(expl_vars, collapse=" + ")))
}
names(formulas) <- out_vars

# reduce data to only variables used
d <- d[, c("state", "state_abbr", "year", out_vars, expl_vars)]

# -------------- #
# random forests #
# -------------- #

# a single tree
ct <- ctree(formulas[[1]], data = d, 
            control = ctree_control(mincriterion = 0.9))

# plot the single tree
scale <- 3
png("present/figs/example-tree.png", res = 300, units = "in",
    height = 3*scale, width = 4*scale)
plot(ct, type = "simple")
dev.off()

# random forests (in parallel)
rfs <- foreach(i = 1:length(out_vars), .packages = "party") %dopar%
  cforest(formulas[[i]], data = d, control = cforest_unbiased(ntree = ntree,
  																														mtry = 5))
names(rfs) <- out_vars


# ------------------- #
# variable importance #
# ------------------- #

# compute variable importance
vis <- foreach(i = 1:length(out_vars), .packages = "party") %dopar%
  varimp(rfs[[i]])
names(vis) <- out_vars

# pull results into a tall data frame
vis_df <- NULL
for (i in 1:length(out_vars)) {
  vis_df0 <- data.frame(expl_var = names(vis[[i]]),
                        expl_var_name = expl_var_names[which(names(vis[[i]]) == expl_vars)],
                        out_var = names(vis)[i],
                        out_var_name = out_var_names[which(names(vis)[i] == out_vars)],
                        importance = vis[[i]])
  rownames(vis_df0) <- NULL
  vis_df <- rbind(vis_df, vis_df0)
}
vis_df$out_var_name <- factor(vis_df$out_var_name, levels = out_var_names)
vis_df$expl_var_name <- reorder(vis_df$expl_var_name, vis_df$importance*(vis_df$out_var_name == "Total Taxes"))

# save variable importance data
saveRDS(vis_df, file = "output/variable-importance.RData")


# ---------- #
# prediction #
# ---------- #

build_prediction_df <- function(expl_var, values, data) {
  fixed_values <- dplyr::select(data, -one_of(expl_var))
  states <- unique(fixed_values$state_abbr)
  years <- unique(fixed_values$year)
  eg <- data.frame(expand.grid(states, years, values))
  names(eg) <- c("state_abbr", "year", expl_var)
  if(is.factor(data[[expl_var]])) {
    eg[, expl_var] <- factor(eg[, expl_var], levels = levels(data[[expl_var]]))
  }
  if(is.integer(data[[expl_var]])) {
    eg[, expl_var] <- as.integer(eg[, expl_var])
  }
  pred_df <- left_join(fixed_values, eg)
  return(pred_df)
}

values <- list()
#values[[1]] <- c("Democrat", "Republican") # levels(d$gov_party)
values[[1]] <- levels(d$change_in_gov_party)
#values[[3]] <- seq(min(d$house_dem_share), max(d$house_dem_share), length.out = n_values)
values[[2]] <- seq(min(d$change_in_house_dem_share), max(d$change_in_house_dem_share), length.out = n_values)
#values[[5]] <- seq(min(d$senate_dem_share), max(d$senate_dem_share), length.out = n_values)
values[[3]] <- seq(min(d$change_in_senate_dem_share), max(d$change_in_senate_dem_share), length.out = n_values)
values[[4]] <- seq(min(d$citizen_ideology), max(d$citizen_ideology), length.out = n_values)
#values[[8]] <- seq(min(d$change_in_citizen_ideology), max(d$change_in_citizen_ideology), length.out = n_values)
values[[5]] <- seq(min(d$taxes_last_year), max(d$taxes_last_year), length.out = n_values)
#values[[10]] <- seq(min(d$personal_income), max(d$personal_income), length.out = n_values)
#values[[11]] <- seq(min(d$population), max(d$population), length.out = n_values)
values[[6]] <- round(seq(min(d$estimated_imbalance), max(d$estimated_imbalance), length.out = n_values))
values[[7]] <- seq(min(d$percent_change_personal_income), max(d$percent_change_personal_income), length.out = n_values)
values[[8]] <- seq(min(d$percent_change_population), max(d$percent_change_population), length.out = n_values)
#values[[15]] <- levels(d$year_of_biennium)
#values[[16]] <- seq(min(d$ranney_10yrs), max(d$ranney_10yrs), length.out = n_values)
names(values) <- expl_vars

n
pred_dfs <- foreach(i = 1:length(out_vars)) %:%
  foreach(j = 1:length(expl_vars), .packages = "dplyr") %dopar% {
    X <- build_prediction_df(expl_vars[j], values[[j]], data = d)
    pred <- predict(rfs[[i]], newdata = X)
    pred_df <- data.frame(X$state, X$year, paste(X$state_abbr, X$year), X[, expl_vars[j]], pred = pred)
    names(pred_df) <- c("state", "year", "case_id", "value", "prediction")
    pred_df$expl_var <- expl_var_names[j]
    pred_df$out_var <- out_var_names[i]
    pred_df
  }  

names(pred_dfs) <- out_vars
for (i in 1:length(out_vars)) {
  names(pred_dfs[[i]]) <- expl_vars
}
  
save(expl_vars, expl_var_names, out_vars, out_var_names, pred_dfs, file = "output/predictions.RData")






