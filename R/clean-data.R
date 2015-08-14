# set working directory to something like
# setwd("~/Dropbox/projects/asking-acting")

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(arm)


# ---------------- #
# state names data #
# ---------------- #

# load data 
state_names <- read.csv("data/raw/state-names.csv", stringsAsFactors = FALSE)


# ---------- #
# taxes data #
# ---------- #

# load data
budget <- read.csv("data/raw/budget-raw.csv", stringsAsFactors = FALSE)

# keep these varaiables
budget_keep <- c("zip", "year", "fy", 
                 # actions
                 "legtotals", "legsales", "leginctax", "legcorpinc",
                 "legcigtob", "legmotofuel", "legalkie", "legothers",
                 "legfees",
                 # proposals
                 "govtotals", "govsales", "govinctax", "govcorpinc",
                 "govcigtob", "govmotofuel", "govalkie", "govothers",
                 "govfees", 
                 # other variables
                 "personal_income1000s_annual", "pop_annual", "taxes",
                 "dpi_pc_change", "pop_pc_change", "estimbalance")
budget <- budget[, budget_keep]

# but call the variables this
budget_names <- c("state_abbr", "year", "fiscal_year",
                  "leg_total", "leg_sales", "leg_income",
                  "leg_corporate", "leg_cigarette_tobacco", "leg_fuel", 
                  "leg_alcohol", "leg_others", "leg_fees",
                  "gov_total", "gov_sales", "gov_income",
                  "gov_corporate",
                  "gov_cigarette_tobacco", "gov_fuel", "gov_alcohol", "gov_others", "gov_fees",
                  "personal_income", "population", 
                  "taxes_last_year", 
                  "percent_change_personal_income", "percent_change_population",
                  "estimated_imbalance")
colnames(budget) <- budget_names


# rescale the leg_* and gov_* variables by taxes_last year
stdz <- function(x) {
  x_new <- (x - mean(x))/sd(x)
  return(x_new)
}
budget <- mutate_each(budget, 
                      funs(rs = stdz(./taxes_last_year)), 
                      starts_with("leg_"), starts_with("gov"))

# rescale some other variables
budget$personal_income <- budget$personal_income/1000000000000 # in trillions 
budget$population <- budget$population/1000000  # in millions
budget$taxes_last_year <- budget$taxes_last_year/1000000000  # in billions

# fixed Montana as MY typo in budget data
budget$state_abbr[budget$state_abbr == "MY"] <- "MT"

# make sure that all states have all years
min_year <- min(budget$year)
max_year <- max(budget$year)
all_years <- min_year:max_year
all_states <- sort(unique(state_names$state_abbr))
all_years_and_states <- data.frame(expand.grid(all_years, all_states), stringsAsFactors = FALSE)
names(all_years_and_states) <- c("year", "state_abbr")
state_names <- left_join(all_years_and_states, state_names)

# merge in state names (the state variable in budget-raw.csv has no spaces!)
budget <- left_join(budget, state_names)


# ------------- #
# ideology data #
# ------------- #

# load data
fording <- read.csv("data/raw/fording-ideology.csv", stringsAsFactors = FALSE)

# keep these variables
fording_keep <- c("statename", "year", "citi6013")
fording <- fording[, fording_keep]

# but use these names
fording_names <- c("state", "year", "citizen_ideology")
names(fording) <- fording_names


# ------------ #
# klarner data #
# ------------ #

# load data 
klarner <- read.csv("data/raw/klarner-partisan-balance.csv")

# clean up some variables
## governor's party
klarner$gov_party[klarner$govparty_c2 == -1] <- "Republican"
klarner$gov_party[klarner$govparty_c2 ==  0] <- "Other"
klarner$gov_party[klarner$govparty_c2 == 1] <- "Democrat"
klarner$gov_party <- factor(klarner$gov_party, 
                            levels = c("Republican", 
                                       "Democrat",
                                       "Other"))
## house democrat share
klarner$house_dem_share <- klarner$hs_dem_prop_all/
  (klarner$hs_dem_prop_all + klarner$hs_rep_prop_all)
## senate democrat share
klarner$senate_dem_share <- klarner$sen_dem_prop_all/
  (klarner$sen_dem_prop_all + klarner$sen_rep_prop_all)

# keep these variables
klarner_keep <- c("year", "state", "gov_party",
                  "house_dem_share", "senate_dem_share")
klarner <- klarner[, klarner_keep]

# but use these names
klarner_names <- c("year", "state", "gov_party",
                  "house_dem_share", "senate_dem_share")
names(klarner) <- klarner_names


# ---------------------- #
# klarner governors data #
# ---------------------- #

# load data 
governors <- read.csv("data/raw/klarner-governors.csv", stringsAsFactors = FALSE)

# clean up the data a bit
## make a year_of biennium variable
governors$year_of_biennium[governors$Biennium_b == governors$year] <- "First Year of Biennium"
governors$year_of_biennium[governors$Biennium_b != governors$year] <- "Second Year of Biennium"
governors$year_of_biennium <- factor(governors$year_of_biennium, 
                                           levels = c("First Year of Biennium", 
                                                      "Second Year of Biennium"))

# keep these variables
governors_keep <- c("state", "year", "year_of_biennium")
governors <- governors[, governors_keep]

# but use these names
governors_names <- c("state", "year", 
                     "year_of_biennium")
names(governors) <- governors_names

# for some reason, this data set contains a lot of empty rows
governors <- na.omit(governors) 


# ----------------------------- #
# competitiveness measures data #
# ----------------------------- #

# load data 
competitiveness <- read.csv("data/raw/competitiveness-measures.csv", stringsAsFactors = FALSE)

# keep these variables
competitiveness_keep <- c("state_abrev", "elect_year", "ranney_10yrs")
competitiveness <- competitiveness[, competitiveness_keep]

# but all them this
competitiveness_names <- c("state_abbr", "year", "ranney_10yrs")
names(competitiveness) <- competitiveness_names


# ----------- #
# merge data  #
# ----------- #

data <- left_join(budget, klarner)
data <- left_join(data, fording)
data <- left_join(data, governors)
data <- left_join(data, competitiveness)

# ----------------------------------- #
# add differenced variables and lags  #
# ----------------------------------- #

data <- mutate(group_by(data, state), 
       lag_gov_party = lag(gov_party, order_by = year),
       lag_house_dem_share = lag(house_dem_share, order_by = year),
       lag_senate_dem_share = lag(senate_dem_share, order_by = year),
       lag_citizen_ideology = lag(citizen_ideology, order_by = year))

# change in governor's partisanship
data$change_in_gov_party <- NA
data$change_in_gov_party[data$gov_party == data$lag_gov_party] <- "No Change"
data$change_in_gov_party[data$gov_party == "Republican" & data$lag_gov_party == "Democrat"] <- "Change to Republican"
data$change_in_gov_party[data$gov_party == "Democrat" & data$lag_gov_party == "Republican"] <- "Change to Democrat"
data$change_in_gov_party <- factor(data$change_in_gov_party, levels = c("Change to Republican",
                                                                        "No Change",
                                                                        "Change to Democrat"))


# change in legislature composition
data$change_in_house_dem_share <- data$house_dem_share - data$lag_house_dem_share
data$change_in_senate_dem_share <- data$senate_dem_share - data$lag_senate_dem_share
data$change_in_citizen_ideology <- data$citizen_ideology - data$lag_citizen_ideology

# drop lags
data <- dplyr::select(data, -starts_with("lag_"))

# ungroup data
data <- ungroup(data)

# ---------------- #
# create tall data #
# ---------------- #

# heighten gov_* variables
d_tall1 <- gather(data, class, proposal, gov_total:gov_fees)
d_tall1$class <- as.character(d_tall1$class)
d_tall1$class[d_tall1$class == "gov_total"] <- "Total"
d_tall1$class[d_tall1$class == "gov_sales"] <- "Sales"
d_tall1$class[d_tall1$class == "gov_cigarette_tobacco"] <- "Cigarette and Tobacco"
d_tall1$class[d_tall1$class == "gov_corporate"] <- "Corporate"
d_tall1$class[d_tall1$class == "gov_fees"] <- "Fees"
d_tall1$class[d_tall1$class == "gov_others"] <- "Others"
d_tall1$class[d_tall1$class == "gov_alcohol"] <- "Alcohol"
d_tall1$class[d_tall1$class == "gov_fuel"] <- "Fuel"
d_tall1$class[d_tall1$class == "gov_income"] <- "Income"
d_tall1 <- dplyr::select(d_tall1, -starts_with("leg_"))

# heighten leg_* variables
d_tall2 <- gather(data, class, action, leg_total:leg_fees)
d_tall2$class <- as.character(d_tall2$class)
d_tall2$class[d_tall2$class == "leg_total"] <- "Total"
d_tall2$class[d_tall2$class == "leg_sales"] <- "Sales"
d_tall2$class[d_tall2$class == "leg_cigarette_tobacco"] <- "Cigarette and Tobacco"
d_tall2$class[d_tall2$class == "leg_corporate"] <- "Corporate"
d_tall2$class[d_tall2$class == "leg_fees"] <- "Fees"
d_tall2$class[d_tall2$class == "leg_others"] <- "Others"
d_tall2$class[d_tall2$class == "leg_alcohol"] <- "Alcohol"
d_tall2$class[d_tall2$class == "leg_fuel"] <- "Fuel"
d_tall2$class[d_tall2$class == "leg_income"] <- "Income"
d_tall2 <- dplyr::select(d_tall2, -starts_with("gov_"))

# join heightened data sets
d_tall <- left_join(d_tall1, d_tall2, na.rm = TRUE)
d_tall$class <- factor(d_tall$class)
reorder_fn <- function(x) {  mean(abs(x))  } 
d_tall$class <- reorder(d_tall$class, d_tall$proposal, reorder_fn)
d_tall$class <- relevel(d_tall$class, ref = "Others")
d_tall$class <- factor(d_tall$class, levels = rev(levels(d_tall$class)))


# ----------- #
# write data  #
# ----------- #

# csv files
write_csv(data, "data/budget.csv")
write_csv(d_tall, "data/budget-tall.csv")

# .RData files
saveRDS(data, file = "data/budget.RData")
saveRDS(d_tall, file = "data/budget-tall.RData")

