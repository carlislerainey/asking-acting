
# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(ggplot2)
require(party)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)

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
cat("\nsetting up clusters for random forests... \n")
cl <- makeCluster(2)
registerDoParallel(cores = 3)
cat(paste("--counting workers... ", getDoParWorkers(), "\n"))
cat("\n building random forests... \n")
start <- Sys.time()
rfs <- foreach(i = 1:length(out_vars), .packages = "party") %dopar% {
	cforest(formulas[[i]], data = d, 
					control = cforest_unbiased(ntree = ntree, mtry = 4))
}
stopCluster(cl)
end <- Sys.time()
cat(paste("(that took", round(difftime(end, start, units = "mins"), 2), "minutes)\n"))
names(rfs) <- out_vars


# ------------------- #
# variable importance #
# ------------------- #

# compute variable importance
cat("\nsetting up clusters for variable importance... \n")
cl <- makeCluster(2)
registerDoParallel(cores = 3)
cat(paste("--counting workers... ", getDoParWorkers(), "\n"))
cat("\ncalculating variable importance... \n")
start <- Sys.time()
vis <- foreach(i = 1:length(out_vars), .packages = "party") %dopar% {
	varimp(rfs[[i]])	
	
}
stopCluster(cl)
names(vis) <- out_vars
end <- Sys.time()
cat(paste("(that took", round(difftime(end, start, units = "mins"), 2), "minutes)\n"))


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
values[[1]] <- c("Democrat", "Republican") # levels(d$gov_party)
values[[2]] <- levels(d$change_in_gov_party)
values[[3]] <- seq(min(d$house_dem_share), max(d$house_dem_share), length.out = n_values)
values[[4]] <- seq(min(d$change_in_house_dem_share), max(d$change_in_house_dem_share), length.out = n_values)
values[[5]] <- seq(min(d$senate_dem_share), max(d$senate_dem_share), length.out = n_values)
values[[6]] <- seq(min(d$change_in_senate_dem_share), max(d$change_in_senate_dem_share), length.out = n_values)
values[[7]] <- seq(min(d$citizen_ideology), max(d$citizen_ideology), length.out = n_values)
values[[8]] <- seq(min(d$taxes_last_year), max(d$taxes_last_year), length.out = n_values)
values[[9]] <- seq(min(d$personal_income), max(d$personal_income), length.out = n_values)
values[[10]] <- seq(min(d$population), max(d$population), length.out = n_values)
values[[11]] <- round(seq(min(d$estimated_imbalance), max(d$estimated_imbalance), length.out = n_values))
values[[12]] <- seq(min(d$percent_change_personal_income), max(d$percent_change_personal_income), length.out = n_values)
values[[13]] <- seq(min(d$percent_change_population), max(d$percent_change_population), length.out = n_values)
names(values) <- expl_vars

cat("\nsetting up clusters for predictions... \n")
cl <- makeCluster(2)
registerDoParallel(cores = 3)
cat(paste("--counting workers... ", getDoParWorkers(), "\n"))
cat("\ncalculating predictions... \n")
start <- Sys.time()
#total_iter <- length(out_vars)*length(expl_vars)
#iter <- 1
pred_dfs <- foreach(i = 1:length(out_vars)) %:%
  foreach(j = 1:length(expl_vars), .packages = c("dplyr")) %dopar% {
    #cat(paste("working on iteration", iter, "of", total_iter, "\n"))
    X <- build_prediction_df(expl_vars[j], values[[j]], data = d)
    X$case_id <- paste(X$state_abbr, X$year)
    pred <- predict(rfs[[i]], newdata = X)
    pred_df <- data.frame(X$state, X$year, X$case_id, X[, expl_vars[j]], pred = pred)
    names(pred_df) <- c("state", "year", "case_id", "value", "prediction")
    pred_df$expl_var <- expl_var_names[j]
    pred_df$out_var <- out_var_names[i]
    #iter <- iter + 1
    pred_df
  }  
stopCluster(cl)
end <- Sys.time()
cat(paste("(that took", round(difftime(end, start, units = "mins"), 2), "minutes)\n"))

names(pred_dfs) <- out_vars
for (i in 1:length(out_vars)) {
  names(pred_dfs[[i]]) <- expl_vars
}
  
save(expl_vars, expl_var_names, out_vars, out_var_names, pred_dfs, file = "output/predictions.RData")


