library("tidyverse")
library("RcppRoll")
library("furrr")
source("./simplified_r_statistic/Estimate_R_Functions.R")

no_cores <- availableCores() - 1
plan(multicore, workers = no_cores)

# Process Arguments
args = commandArgs(trailingOnly=TRUE)
SMOOTHING_FLAG_POSITION = which(args == "-smooth")
MEAN_FLAG_POSITION = which(args == "-mean")
SD_FLAG_POSITION = which(args == "-sd")
TAU_FLAG_POSITION = which(args == "-tau")
OUTPUT_FLAG_POSITION = which(args == "-o")

print(args[SMOOTHING_FLAG_POSITION + 1])
smoothing_window = eval(parse(text = args[SMOOTHING_FLAG_POSITION + 1]))
print(args[MEAN_FLAG_POSITION + 1])
GT_mean = eval(parse(text = args[MEAN_FLAG_POSITION + 1]))
print(args[SD_FLAG_POSITION + 1])
GT_SD = eval(parse(text = args[SD_FLAG_POSITION + 1]))
print(args[TAU_FLAG_POSITION + 1])
tau = eval(parse(text = args[TAU_FLAG_POSITION + 1]))
print(args[OUTPUT_FLAG_POSITION + 1])
OUTPUTFILE = args[OUTPUT_FLAG_POSITION + 1]

parameter_list = list(smoothing_window = smoothing_window, 
                      GT_mean = GT_mean, 
                      GT_SD = GT_SD, 
                      tau = tau)
print(parameter_list)
parameter_combos = cross(parameter_list)
#-----------
# Load Data
#------------

data = read_csv("./simplified_r_statistic/case_data.csv")
data = data %>%
  group_by(region, regionID, region_type, regionID_type) %>%
  filter(n() > 20) %>%
  filter(max(new_cases) > 10) %>%
  ungroup()

set.seed(19890616)
data_1 = data %>% filter(region_type == "state") %>% filter(region %in% sample(unique(.$region), 10))
data_2 = data %>% filter(region_type == "county") %>% filter(region %in% sample(unique(.$region), 10))
data_3 = data %>% filter(region_type == "nation") %>% filter(region %in% sample(unique(.$region), 10))

data = bind_rows(data_1, data_2, data_3)

#-----------
# Load Functions
#------------

RMSE_func <- function(v, v_ref) {
  RMSE = sqrt((v_ref - v)^2)
}

EstimateR.WL_safe = safely(EstimateR.WL)
EstimateR.cori_safe = safely(EstimateR.cori)
EstimateR.WT_safe = safely(EstimateR.WT)

R_estimations <- function(data, var_D, var_D_sd, var_tau, smoothing_window) {
  
  data <- data %>%
    arrange(date) %>%
    group_by(region, region_type, regionID, regionID_type) %>%
    nest(CD = -c(region, region_type, regionID, regionID_type))
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.simple(date = CD_$date, Is = CD_$new_cases, si_mean = var_D, tau = var_tau))) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.cori_safe(date = CD_$date, Is = CD_$new_cases, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
    filter(map_lgl(R_estimate, function(i) is.null(i[["error"]]))) %>%
    mutate(R_estimate = map(R_estimate, function(i) i[["result"]])) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.WT_safe(date = CD_$date, Is = CD_$new_cases, si_mean = var_D, si_sd = var_D_sd))) %>%
    filter(map_lgl(R_estimate, function(i) is.null(i[["error"]]))) %>%
    mutate(R_estimate = map(R_estimate, function(i) i[["result"]])) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.WL_safe(date = CD_$date, Is = CD_$new_cases, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
    filter(map_lgl(R_estimate, function(i) is.null(i[["error"]]))) %>%
    mutate(R_estimate = map(R_estimate, function(i) i[["result"]])) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data = data %>% unnest(CD) %>% ungroup()
  
  data = data %>%
    mutate(RMSE.KN_Cori = RMSE_func(KN.R_mean, Cori.R_mean)) %>%
    mutate(RMSE.KN_WT = RMSE_func(KN.R_mean, TD.R_mean)) %>%
    mutate(RMSE.KN_WL = RMSE_func(KN.R_mean, WL.R_mean)) %>%
    mutate(RMSE.Cori_WT = RMSE_func(Cori.R_mean, TD.R_mean)) %>%
    mutate(RMSE.Cori_WL = RMSE_func(Cori.R_mean, WL.R_mean)) %>%
    mutate(RMSE.WT_WL = RMSE_func(TD.R_mean, WL.R_mean))
  
  data <- data %>% 
    pivot_longer(cols = starts_with("RMSE"), names_to = "Comparison", values_to = "RMSE") %>%
    mutate(Comparison = factor(Comparison, levels = c("RMSE.KN_Cori", "RMSE.KN_WT", "RMSE.KN_WL", 
                                                      "RMSE.Cori_WT", "RMSE.Cori_WL", "RMSE.WT_WL")))
  
  data$Comparison <- data$Comparison %>% fct_recode(`Cori vs Wallinga & Lipsitch` = "RMSE.Cori_WL",
                                                `Cori vs Wallinga & Teunis` = "RMSE.Cori_WT",
                                                `Simple Ratio vs Cori` = "RMSE.KN_Cori",
                                                `Simple Ratio vs Wallinga & Lipsitch` = "RMSE.KN_WL",
                                                `Simple Ratio vs Wallinga & Tuenis` = "RMSE.KN_WT",
                                                `Walinga & Teunis vs Wallinga & Lipsitch` = "RMSE.WT_WL")
  
  return(data)
}

#------------------------------
# Apply Function with Parameters
#------------------------------

df = map(parameter_combos, function(i) R_estimations(data, var_D = i$GT_mean, var_D_sd = i$GT_SD, var_tau = i$tau, smoothing_window = i$smoothing_window))

names(df) = map_chr(parameter_combos, paste, collapse=",")
df = bind_rows(df, .id = "parameters")
df = df %>% separate(col = parameters, into = c("smoothing_window", "GT_mean", "GT_SD", "tau"))
df = df %>% mutate(smoothing_window = as.numeric(smoothing_window), 
                   GT_mean = as.numeric(GT_mean),
                   GT_SD = as.numeric(GT_SD), 
                   tau = as.numeric(tau))

# Save file
write_csv(df, OUTPUTFILE)
