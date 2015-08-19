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
state_names <- read.csv("data/raw/state-names.csv", stringsAsFactors = FALSE)


# ---------- #
# taxes data #
# ---------- #

# load data
budget <- read.csv("data/raw/budget-raw.csv", stringsAsFactors = FALSE)
# load data
budget <- read.csv("data/raw/budget-raw.csv", stringsAsFactors = FALSE)

# keep these varaiables
budget_keep <- c("zip", "year", "legtotals")
budget <- budget[, budget_keep]

# but call the variables this
budget_names <- c("state_abbr", "year", "leg_total")
colnames(budget) <- budget_names

# merge in state names (the state variable in budget-raw.csv has no spaces!)
budget <- left_join(budget, state_names)
#budget <- filter(budget, year == 2009)
budget <- na.omit(budget)

# rescale total in billioins
budget$leg_total <- budget$leg_total/1000000000

# order states by total
budget$state <- reorder(budget$state, budget$leg_total)

df_max <- summarize(group_by(budget, state), max = max(leg_total), max_year = max(year[max(leg_total) == leg_total]))
df_min <- summarize(group_by(budget, state), min = min(leg_total), min_year = min(year[min(leg_total) == leg_total]))


gg <- ggplot(budget, aes(x = state, y = leg_total, color = year)) + 
  #geom_bar(stat = "identity", width = .05, color = "grey50") + 
  geom_point() +
  coord_flip() + 
  labs(title = "Tax Changes in the States by Year",
       y = "Total Enacted Revenue Changes (in Billions)",
       x = "",
       color = "Year") +
  scale_color_gradient(low = "#99d594", 
                        high = "#fc8d59") + 
  geom_text(data = df_max, aes(x = state, y = max, label = max_year, color = max_year),
            size = 3.5, hjust = -.2) +
  geom_text(data = df_min, aes(x = state, y = min, label = min_year, color = min_year),
            size = 3.5, hjust = 1.2) +
  theme_bw()
ggsave("doc/figs/example-data.png", gg, height = 11, width = 11)
gg
