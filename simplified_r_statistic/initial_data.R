# This scrip calculates the world data for app
library(tidyverse)
source("./simplified_r_statistic/Estimate_R_Functions.R")
DATA = read_csv("./simplified_r_statistic/case_data.csv", col_types = "ccccDdld")


# Load Initial Data to Display ======================

data <- DATA %>% filter(region == "World")

# Smooth Data
data = smooth_new_cases(data, smoothing_window = 1)

# Calculate R
var_tau = 7
var_D = 4

data <- data %>%
  arrange(date) %>%
  group_by(region, region_type, regionID, regionID_type) %>%
  nest(CD = -c(region, region_type, regionID, regionID_type)) %>% 
  mutate(estimateR = map(CD, function(CD_) EstimateR.simple(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, tau = var_tau))) %>%
  mutate(CD = map2(CD, estimateR, left_join, by="date")) %>% 
  unnest(CD) %>% 
  ungroup() %>%
  dplyr::select(-estimateR)

write_csv(data, "./simplified_r_statistic/initial_data.csv")
