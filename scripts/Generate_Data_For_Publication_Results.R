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

sim_data = read_rds("./results/seir_ode_dec60-7_sim.rds")
sim_data.df = sim_data$sim_df %>% dplyr::select(time, incidence, obs_cases, true_r0, true_rt)
sim_data.df = sim_data.df[2:150,]
sim_data.df$time = as.Date(sim_data.df$time, origin = "2020-01-01")

#-----------
# Load Functions
#------------

R_estimations <- function(data, var_D, var_D_sd, var_tau, smoothing_window) {
  
  data <-left_join(data, 
                   EstimateR.simple(date = data$time, Is = data$obs_cases, si_mean = var_D, tau = var_tau),
                   by = c("time" = "date"))
  
  data <-left_join(data, 
                   EstimateR.cori(date = data$time, Is = data$obs_cases, si_mean = var_D, si_sd = var_D_sd, tau = var_tau),
                   by = c("time" = "date"))
  
  data <-left_join(data, 
                   EstimateR.WT(date = data$time, Is = data$obs_cases, si_mean = var_D, si_sd = var_D_sd, tau = var_tau),
                   by = c("time" = "date"))
  
  data <-left_join(data, 
                   EstimateR.WL(date = data$time, Is = data$obs_cases, si_mean = var_D, si_sd = var_D_sd, tau = var_tau),
                   by = c("time" = "date"))
  
  return(data)
}

#------------------------------
# Apply Function with Parameters
#------------------------------

df = map(parameter_combos, function(i) R_estimations(sim_data.df, var_D = i$GT_mean, var_D_sd = i$GT_SD, var_tau = i$tau, smoothing_window = i$smoothing_window))

names(df) = map_chr(parameter_combos, paste, collapse=",")
df = bind_rows(df, .id = "parameters")
df = df %>% separate(col = parameters, into = c("smoothing_window", "GT_mean", "GT_SD", "tau"))
df = df %>% mutate(smoothing_window = as.numeric(smoothing_window), 
                   GT_mean = as.numeric(GT_mean),
                   GT_SD = as.numeric(GT_SD), 
                   tau = as.numeric(tau))

# Save file
write_csv(df, OUTPUTFILE)
