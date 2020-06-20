# This is a list of functions to estiamte R

##-----Smoothing-Function------

smooth_new_cases <- function(data, smoothing_window) {
  
  library("RcppRoll")
  
  #smoothing_window hold the user input on the size of the smoothing window
  data = data %>%
    group_by(region, region_type, regionID, regionID_type) %>%
    mutate(new_cases_smoothed = roll_mean(x=new_cases, n = smoothing_window, align="center", fill = c(NA, NA, NA), na.rm=T)) %>%
    ungroup()
  
  # replace NAs in new_cases_smoothed with value from new_cases
  data$new_cases_smoothed[is.na(data$new_cases_smoothed)] = data$new_cases[is.na(data$new_cases_smoothed)]
  
  # round the smoothed new cases to whole numbers
  data$new_cases_smoothed= round(data$new_cases_smoothed)
  
  # Remove smoothed new cases that result in NAs or leading 0s
  data = data %>% filter(!is.na(new_cases_smoothed)) %>% 
    group_by(region, region_type, regionID, regionID_type) %>%
    mutate(reference_date = date[min(which((new_cases_smoothed > 0)))]) %>%
    filter(date >= reference_date) %>%
    dplyr::select(-reference_date) %>%
    ungroup()
  
  return(data)
}

## ---- Simple-R-Estimate ----

EstimateR.simple <- function(date, Is, si_mean, tau) {
  
  df = data.frame(date, Is)
    
  # Basic Ratio
  df = df %>%
    mutate(denominator = roll_sum(Is,  tau, align="center", fill = c(NA, NA, NA))) %>%
    mutate(numerator = lead(denominator, si_mean)) %>%
    mutate(numerator = replace(numerator, which(numerator == 0), NA)) %>%
    mutate(denominator = replace(denominator, which(denominator == 0), NA)) %>%
    mutate(`Simple Ratio.R_mean` = numerator/denominator)
  
  # remove any ratios involving 0 new_cases in denominator or numerator
  df_2 = df %>% filter(!is.na(`Simple Ratio.R_mean`))
  
  # Confidence Interval. Compared by binomial test
  CIs = map2(df_2$numerator, df_2$denominator, function(i,j) binom.test(c(i,j), sum(i,j), p=0.5, alternative="two.sided"))
  CIs = transpose(CIs) %>% .$conf.int %>% unlist() %>% matrix(ncol=2, byrow=T)
  CIs = CIs/(1-CIs)
    
  df_2 = df_2 %>% mutate(`Simple Ratio.R_Quantile_025` = CIs[,1], 
                         `Simple Ratio.R_Quantile_975` = CIs[,2])
  
  df = left_join(df, df_2, by=colnames(df)[colnames(df) %in% colnames(df_2)])
  
  # Select columns to output
  df = df %>% dplyr::select(date, `Simple Ratio.R_mean`, `Simple Ratio.R_Quantile_025`, `Simple Ratio.R_Quantile_975`)
  
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
    `WT.R_mean` = r_estimates$estimates[[1]]$R,
    `WT.R_Quantile_025` = r_estimates$estimates[[1]]$conf.int[[1]],
    `WT.R_Quantile_975` = r_estimates$estimates[[1]]$conf.int[[2]]
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

