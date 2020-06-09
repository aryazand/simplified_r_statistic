# This is a list of functions to estiamte R

##-----Smoothing-Function------

smooth_new_cases <- function(data, smoothing_window) {
  
  library("RcppRoll")
  
  #smoothing_window hold the user input on the size of the smoothing window
  data = data %>%
    group_by(region, region_type, regionID, regionID_type) %>%
    mutate(new_cases_smoothed = roll_mean(new_cases, n = smoothing_window, align="center", fill = c(NA, NA, NA), na.rm=T)) %>%
    mutate(new_cases_smoothed = replace(new_cases_smoothed, is.na(new_cases_smoothed), new_cases[is.na(new_cases_smoothed)])) %>%
    ungroup()
  
  # replace NAs in new_cases_smoothed with value from new_cases
  data$new_cases_smoothed[is.na(data$new_cases_smoothed)] = data$new_cases[is.na(data$new_cases_smoothed)]
  
  # Remove smoothed new cases that result in NAs or leading 0s
  data = data %>% filter(!is.na(new_cases_smoothed)) %>% 
    mutate(reference_date = date[min(which((new_cases_smoothed > 0)))] - 1) %>%
    filter(date >= reference_date) %>%
    dplyr::select(-reference_date)
  
  return(data)
}

## ---- Simple-R-Estimate ----

EstimateR.simple <- function(date, Is, si_mean, tau) {
  # This function takes a
  df = data.frame(date, Is)
    
  df = df %>%
    mutate(rolling_sum = roll_sum(Is,  tau, align="center", fill = c(NA, NA, NA))) %>%
    mutate(numerator = lead(rolling_sum, si_mean)) %>%
    mutate(numerator = replace(numerator, which(numerator == 0), NA)) %>%
    mutate(KN.R_mean = numerator/rolling_sum) %>%
    mutate(k = numerator) %>%
    mutate(theta = KN.R_mean/k) %>%
    mutate(KN.R_Quantile_025 = qgamma(0.025, shape = k, scale = theta)) %>%
    mutate(KN.R_Quantile_975 = qgamma(0.975, shape = k, scale = theta)) %>%
    dplyr::select(date, KN.R_mean, KN.R_Quantile_025, KN.R_Quantile_975)
  
  return(df)
}

## ---- Cori-R-Estimate----
library("EpiEstim")

EstimateR.cori <- function(date, Is, si_mean, si_sd, tau) {
  
  r_estimates = EpiEstim::estimate_R(Is, 
                                     method = "parametric_si", 
                                     config = make_config(list(mean_si = si_mean, 
                                                               std_si = si_sd,
                                                               t_start = seq(2, length(Is) - tau + 1),
                                                               t_end = seq(tau+1, length(Is))))
  )
  
  df = data.frame(
    date = tail(date-round(tau/2), -tau),
    Cori.R_mean = r_estimates$R$`Mean(R)`,
    Cori.R_Quantile_025 = r_estimates$R$`Quantile.0.025(R)`,
    Cori.R_Quantile_975 = r_estimates$R$`Quantile.0.975(R)`
  )

  return(df)
}

## ---- WT-R-Estimate----
library("R0")

EstimateR.WT <- function(date, Is, si_mean, si_sd) {
  
  mGT = generation.time("gamma", c(si_mean, si_sd))
  names(Is) = date
  r_estimates = estimate.R(Is, GT = mGT, methods="TD", begin=1, end=as.numeric(length(Is)), nsim=1000)
  
  df = data.frame(
    date = as.Date(names(r_estimates$estimates[["TD"]]$R), origin="1970-01-01"),
    TD.R_mean = r_estimates$estimates[[1]]$R,
    TD.R_Quantile_025 = r_estimates$estimates[[1]]$conf.int[[1]],
    TD.R_Quantile_975 = r_estimates$estimates[[1]]$conf.int[[2]]
  ) 
  
  # for some reason last R estimate from TD always ends up as 0. So we'll remove that
  df = df[-c(1, nrow(df)),]
  
  return(df)
}

## ---- WL-R-Estimate----
library("R0")

EstimateR.WL <- function(date, Is, si_mean, si_sd, tau) {
  library("R0")
  library("tidyverse")
  
  mGT = generation.time("gamma", c(si_mean, si_sd))
  
  #-----------
  # Use a negative binomial distribution to estimate the growth rate for each 
  # period size tau
  #------------
  df = data.frame(day = seq_along(Is), Is = round(Is))
  
  model = map(1:(length(Is)-tau), function(t) 
    glm(Is ~ day, family="poisson", data = slice(df, t:(t+tau)))
  )
  
  r = map_dbl(model, function(m) as.numeric(coef(m)["day"]))
  
  get_confint = safely(confint)
  r.confint = map(model, get_confint) %>% transpose() %>% .[[1]]
  names(r.confint) = date[(1+(floor(tau/2))):(length(date)-(ceiling(tau/2)))]
  r.confint <- map(r.confint, data.frame)
  r.confint <- map(r.confint, function(x) x[2,])
  r.confint = bind_rows(r.confint, .id="date")
  
  df = cbind(r.confint, r)
  
  colnames(df)[2:4] <- c("WL.R_Quantile_025", "WL.R_Quantile_975", "WL.R_mean")
  
  #-----------
  # use R0 package to get R from r 
  #------------
  
  df <- df %>% mutate_at(vars(starts_with("WL")), function(x) map_dbl(x, R0:::R.from.r, GT = mGT))
  
  df$date = as.Date(df$date, origin="1970-01-01")
  
  return(df)
}

