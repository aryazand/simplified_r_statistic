library("tidyverse")
library("RcppRoll")
source("../simplified_r_statistic/Estimate_R_Functions.R")

#-----------
# Load Data
#------------

data = read_csv("../simplified_r_statistic/case_data.csv")
set.seed(19890616)
data = data %>% filter(region_type == "state") %>% filter(region %in% sample(unique(.$region), 10))
data = data %>% group_by(region, region_type, regionID, regionID_type) %>% filter(n() > 15) %>% ungroup()

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
  data = smooth_new_cases(data, smoothing_window)
  
  data <- data %>%
    arrange(date) %>%
    group_by(region, region_type, regionID, regionID_type) %>%
    nest(CD = -c(region, region_type, regionID, regionID_type))
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.simple(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, tau = var_tau))) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.cori_safe(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
    filter(map_lgl(R_estimate, function(i) is.null(i[["error"]]))) %>%
    mutate(R_estimate = map(R_estimate, function(i) i[["result"]])) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.WT_safe(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd))) %>%
    filter(map_lgl(R_estimate, function(i) is.null(i[["error"]]))) %>%
    mutate(R_estimate = map(R_estimate, function(i) i[["result"]])) %>%
    mutate(CD = map2(CD, R_estimate, left_join, by="date")) %>%
    dplyr::select(-R_estimate)
  
  data <- data %>% 
    mutate(R_estimate = map(CD, function(CD_) EstimateR.WL_safe(date = CD_$date, Is = CD_$new_cases_smoothed, si_mean = var_D, si_sd = var_D_sd, tau = var_tau))) %>%
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
# Calculate for Figure 2 and 3
#------------------------------

df = R_estimations(data, var_D = 4, var_D_sd = 3, var_tau = 7, smoothing_window = 7)
write_csv(df, "Fig2&3_data.csv")


#-------------------------
# Calculate for Figure 4
#-------------------------

smoothing_window.seq = 1:10
df = map(smoothing_window.seq, function(i) R_estimations(data, var_D = 4, var_D_sd = 3, var_tau = 7, smoothing_window = i))
names(df) <- smoothing_window.seq

df <- bind_rows(df, .id = "smoothing_window")
df <- df %>% group_by(smoothing_window, Comparison) %>% 
  summarise(Median = median(RMSE, na.rm=T),
            `95th_percentile` = quantile(RMSE, 0.95, na.rm=T))

df$smoothing_window = as.numeric(df$smoothing_window)

df = df %>% pivot_longer(cols = c("Median", "95th_percentile"), names_to = "error_type", values_to = "error")
df$error_type =  df$error_type %>% fct_recode(`Median Error` = "Median", 
                                              `95th Percentile of Error` = "95th_percentile")

write_csv(df, "Fig4_data.csv")


#-------------------------
# Calculate for Figure 5
#-------------------------

tau.seq = 1:10
df = map(tau.seq, function(i) R_estimations(data, var_D = 4, var_D_sd = 3, var_tau = i, smoothing_window = 7))
names(df) <- tau.seq

df <- bind_rows(df, .id = "tau")
df <- df %>% group_by(tau, Comparison) %>% summarise(Median = median(RMSE, na.rm=T),
                                                                   `95th_percentile` = quantile(RMSE, 0.95, na.rm=T))
df$tau = as.numeric(df$tau)

df = df %>% pivot_longer(cols = c("Median", "95th_percentile"), names_to = "error_type", values_to = "error")
df$error_type =  df$error_type %>% fct_recode(`Median Error` = "Median", 
                                                            `95th Percentile of Error` = "95th_percentile")
write_csv(df, "Fig5_data.csv")

#-------------------------
# Calculate for Figure 6
#-------------------------

mean_sd.seq = cross2(c(2,4,6,8,10), c(2,4,6,8,10))
df = map(mean_sd.seq, function(i) R_estimations(data, var_D = i[[1]], var_D_sd = i[[2]], var_tau = 7, smoothing_window = 7))

names(df) <- mean_sd.seq %>% map(., unlist) %>% map(., function(i) paste(i, collapse=",")) %>% unlist()

df <- bind_rows(df, .id = "GT")
df <- df %>% group_by(GT, Comparison) %>% summarise(Median = median(`Absolute Error`, na.rm=T),
                                                                  `95th_percentile` = quantile(`Absolute Error`, 0.95, na.rm=T))
df <- df %>% separate(col=GT, c("mean", "SD"))
df <- df %>% mutate(mean = as.numeric(mean),
                                  SD = as.numeric(SD))

write_csv(df, "Fig6_data.csv")