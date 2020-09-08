#*******************************
# These functions are from Gostic et al 2020
#*******************************


#' is.wholenumber
#'
#' For parameter checking
is.wholenumber <- function(x) {
  x == round(x)
}

check_args <- function(
  arnaught, t_E, t_I, N, S_init, E_init, I_init, n_t, n_steps_per_t
) {
  # Check all input parameters
  stopifnot(is.wholenumber(N) && length(N) == 1 && N >= 1)
  stopifnot(is.wholenumber(n_t) && length(n_t) == 1 && n_t >= 1)
  stopifnot(
    is.wholenumber(n_steps_per_t) && length(n_steps_per_t) == 1 &&
      n_steps_per_t >= 1
  )
  stopifnot(
    is.numeric(arnaught) && arnaught > 0 &&
      (length(arnaught) == 1 || length(arnaught) == n_t + 1)
  )
  stopifnot(
    is.numeric(t_E) && length(t_E) == 1 && t_E >= 0
  )
  stopifnot(
    is.numeric(t_I) && length(t_I) == 1 && t_I > 0
  )
}

construct_beta <- function(arnaught, t_I, n_t) {
  beta_t_all <- arnaught / t_I
  if(length(arnaught) == 1) {
    function(t) beta_t_all
  } else {
    approxfun(0:n_t, beta_t_all)
  }
}

simulate_seir <- function(
  arnaught, t_E, t_I,
  N, S_init, E_init, I_init,
  n_t, n_steps_per_t = 1,
  method = 'stochastic'
) {
  func <- if(method == 'stochastic') simulate_seir_stochastic else simulate_seir_ode
  func(
    arnaught, t_E, t_I,
    N, S_init, E_init, I_init,
    n_t, n_steps_per_t
  )
}

#' Simulate a discrete-time approximation of a continuous-time, discrete-state stochastic S(E)IR model.
#' 
#' Ed Baskerville
#' 15 April 2020
#' 
#' No age structure.
#' 
#' @param n_t Number of units of time to simulate.
#' @param n_steps_per_t Number of discrete simulation steps to take per
#' unit of simulation time.
#' @param arnaught Basic reproduction number (ratio), squiggly-R-0. Average number of new infections produced by an infection in a susceptible population. A scalar or a vector of length `n_t + 1`, which specifies R0 at the start of each timestep. R0 is linearly interpolated between timesteps.
#' @param t_E Mean latent period. If set to 0, the model reduces to an SIR.
#' @param t_I Mean duration of infectiousness.
# # for debugging
# arnaught
# N = N
# E_init = 0          # Initial in E
# I_init = 1e-5 * N     # Initial in I
# t_E = 3               # time from exposed to infected
# t_I = 5               # time from infected to recovery
# n_t = 1000            # total simulation time
# max_R0 = 2.0          # Initial, max R0
# min_R0 = 0.9         # Minimum with intervention
# end_max_time = 50     # time of intervention
# start_min_time = 70  #time when R0 hits lowest value
# n_steps_per_t = 10
# CONTINUOUS = TRUE       # If TRUE, R decreases according to arctan after intervention. If FALSE, R


simulate_seir_stochastic <- function(
  arnaught, 
  t_E, 
  t_I,
  N, 
  S_init, 
  E_init, 
  I_init,
  n_t, 
  n_steps_per_t
) {
  check_args(
    arnaught, t_E, t_I, N, S_init, E_init, I_init, n_t, n_steps_per_t
  ) 
  
  # Precompute a few things
  delta_t <- 1 / n_steps_per_t
  
  # Draws a binomial based on a rate
  draw <- function(n, rate) {
    p <- 1 - exp(-rate * delta_t)
    rbinom(1, n, p)
  }
  
  # Function to compute beta at a particular time
  beta <- construct_beta(arnaught, t_I, n_t)
  
  # Step forward from t to t + delta_t
  step <- function(t, S_prev, E_prev, I_prev) {
    dS <- draw(S_prev, beta(t) * I_prev / N)
    dIR <- draw(I_prev, 1 / t_I)
    
    if(t_E > 0) {
      # SEIR model
      dEI <- draw(E_prev, 1 / t_E)
      list(
        S = S_prev - dS,
        E = E_prev + dS - dEI,
        I = I_prev + dEI - dIR,
        dS = dS,
        dEI = dEI,
        dIR = dIR
      )
    }
    else {
      # SIR model
      list(
        S = S_prev - dS,
        E = 0,
        I = I_prev + dS - dIR,
        dS = dS,
        dEI = dS,
        dIR = dIR
      )
    }
  }
  
  # Set up state vectors over time
  S <- numeric(n_t + 1)
  S[1] <- S_init
  
  E <- numeric(n_t + 1)
  I <- numeric(n_t + 1)
  if(t_E > 0) {
    # SEIR model
    E[1] <- E_init
    I[1] <- I_init
  }else {
    # SIR model: all initial E get dumped in I
    E[1] <- 0
    I[1] <- E_init + I_init
  }
  
  # Track transitions over time
  dS <- rep(NA, n_t + 1)
  dEI <- rep(NA, n_t + 1)
  dIR <- rep(NA, n_t + 1)
  
  # Simulate
  for(tt in 1:(n_t-1)) {
    S_prev <- S[tt]
    E_prev <- E[tt]
    I_prev <- I[tt]
    
    dS[tt+1] <- 0
    dEI[tt+1] <- 0
    dIR[tt+1] <- 0
    for(i in 1:n_steps_per_t) {
      state_next <- step(tt + delta_t * (i - 1), S_prev, E_prev, I_prev)
      S_prev <- state_next$S
      E_prev <- state_next$E
      I_prev <- state_next$I
      dS[tt+1] <- dS[tt+1] + state_next$dS
      dEI[tt+1] <- dEI[tt+1] + state_next$dEI
      dIR[tt+1] <- dIR[tt+1] + state_next$dIR
    }
    
    S[tt+1] <- S_prev
    E[tt+1] <- E_prev
    I[tt+1] <- I_prev
  }
  
  # Return each compartment over time
  data.frame(
    time = 0:n_t,
    S = S,
    E = E,
    I = I,
    R = N - S - E - I,
    dS = dS,
    dEI = dEI,
    dIR = dIR
  )
}

#' Simulate a deterministic ODE approximation of a continuous-time, discrete-state stochastic S(E)IR model.
#' 
#' Ed Baskerville
#' 15 April 2020
#' 
#' No age structure.
#' 
#' @param arnaught Basic reproduction number (ratio), squiggly-R-0. Average number of new infections produced by an infection in a susceptible population. A scalar or a vector of length `n_t + 1`, which specifies R0 at the start of each timestep. R0 is linearly interpolated between timesteps.
#' @param t_E Mean latent period. If set to 0, the model reduces to an SIR.
#' @param t_I Mean duration of infectiousness.
#' @param n_t Number of units of time to simulate.
simulate_seir_ode <- function(
  arnaught, t_E, t_I,
  N, S_init, E_init, I_init,
  n_t,
  n_steps_per_t = 1 # Ignored; included so the function signature matches stochastic version
) {
  library(deSolve)
  
  check_args(
    arnaught, t_E, t_I, N, S_init, E_init, I_init, n_t, n_steps_per_t
  )
  
  beta <- construct_beta(arnaught, t_I, n_t)
  d_dt <- function(t, y, params) {
    dS <- y['S'] * beta(t) * y['I'] / N
    dIR <- y['I'] / t_I
    
    if(t_E > 0) {
      # SEIR model
      dEI <- y['E'] / t_E
      list(c(
        S = -dS,
        E = dS - dEI,
        I = dEI - dIR,
        R = dIR,
        cum_dS = dS,
        cum_dEI = dEI
      ), NULL)
    }
    else {
      # SIR model
      list(c(
        S = -dS,
        E = 0,
        I = dS - dIR,
        R = dIR,
        cum_dS = dS,
        cum_dEI = dS
      ), NULL)
    }
  }
  
  y_init <- c(
    S = S_init,
    E = if(t_E > 0) E_init else 0,
    I = if(t_E > 0) I_init else E_init + I_init,
    R = 0,
    cum_dS = 0,
    cum_dEI = 0
  )
  #automatic ode solver is lsoda, an "automatic stiff/non-stiff solver"
  as.data.frame(ode(y_init, 0:n_t, d_dt, NULL)) %>%
    mutate(dS = cum_dS - lag(cum_dS, 1)) %>%
    mutate(dEI = cum_dEI - lag(cum_dEI, 1)) %>%
    mutate(dIR = R - lag(R, 1))
}


sim_sweep <- function(PARAMS, dirname){
  
  if(!dir.exists(dirname)){
    sprintf('creating new output directory, %s', dirname)
    dir.create(dirname)
  }
  
  for(intervention_time_1 in PARAMS$intervention_time_1){
    for(decrease_duration in PARAMS$days_intervention_to_min){
      for(intervention_time_2 in PARAMS$intervention_time_2){
        for(increase_duration in PARAMS$days_to_Rt_rise){
          ## Specify time-varying R0
          arnaught <- specify_arnaught(R0_vec = c(PARAMS$pre_intervention_R0, PARAMS$intervention_R0, PARAMS$partially_lifeted_R0), 
                                       change_start_vec = c(intervention_time_1, intervention_time_2), 
                                       change_end_vec =  c(intervention_time_1+decrease_duration,intervention_time_2+increase_duration), 
                                       NT = PARAMS$n_t)
          do_one_arnaught(arnaught, intervention_time_1, decrease_duration, PARAMS, dirname)
        }
      }
    }
  }
}

do_one_arnaught <- function(arnaught, intervention_time, decrease_duration, PARAMS, dirname) {
  for(method in PARAMS$methods) {
    for(model_type in PARAMS$model_types) {
      sim_df <- if(model_type == 'sir') {
        simulate_sir_example(
          arnaught = arnaught,
          t_I = PARAMS$t_I,
          N = PARAMS$N, I_init = PARAMS$I_init,
          n_t = PARAMS$n_t,
          method = method
        ) %>%
          mutate(
            incidence = round(dS),
            obs_cases = NA
          )
      } else {
        simulate_seir_example(
          arnaught = arnaught,
          t_E = PARAMS$t_E, t_I = PARAMS$t_I,
          N = PARAMS$N, E_init = PARAMS$E_init, I_init = PARAMS$I_init,
          n_t = PARAMS$n_t,
          method = method
        ) %>%
          mutate(
            incidence = round(dS),
            obs_cases = round(dEI)
          )
      }
      
      saveRDS(
        list(
          sim_df = sim_df %>% 
            mutate(true_r0 = arnaught,
                   true_rt = arnaught*S/(S+E+I+R)),
          arnaught = arnaught,
          intervention_time = intervention_time,
          decrease_duration = decrease_duration,
          
          params = PARAMS,
          method = method,
          model_type = model_type
        ),
        sprintf('%s/%s_%s_dec%.0f-%.0f_sim.rds', dirname, model_type, method, intervention_time, decrease_duration)
      )
    }
  }
}


#sim_sweep(PARAMS)
#END UP WITH one folder for each R0, which contains: SEIR stoc {decreases}; SEIR ode {decreases}, parameters



## Check outputs
testplots <- function(PARAMS) {
  for(arnaught in PARAMS$pre_intervention_R0) {
    for(model_type in c('seir')) {
      for(method in PARAMS$methods) {
        # Load results from all interventions applied to a given R0, and bind into a single data frame
        sim_results <- data.frame(fns = list.files(path = sprintf('R0-%.1f', arnaught)), stringsAsFactors = FALSE) %>%
          filter(grepl(fns, pattern = method)) %>%
          filter(grepl(fns, pattern = '_sim')) %>%
          pull(fns) %>%
          lapply(FUN = function(xx){
            readRDS(paste0(sprintf('R0-%.1f/', arnaught), xx)) -> tmp
            tmp$sim_df %>% 
              mutate(int_time = as.character(tmp$intervention_time), 
                     dec_dur = as.character(tmp$decrease_duration))
          }) %>%
          bind_rows()
        
        cat(sprintf('Plotting %s, %s', model_type, method))
        # sim_result <- readRDS(sprintf('R0-%.1f/%s_%s_dec%.0f-%.0f_sim.rds', 
        #                               arnaught, model_type, method, intervention_time, decrease_duration))
        
        ## Plot prevalence in each compartment
        sim_results %>%
          select(time:R, int_time, dec_dur) %>%
          pivot_longer(S:R, names_to = 'Compartment', values_to = 'Prevalence') %>%
          ggplot() +
          geom_line(aes(x = time, y = Prevalence, color = int_time, lty = dec_dur)) +
          facet_wrap(.~Compartment, scales = 'free_y') +
          ggtitle(sprintf('%s - %s - R0=%.1f', model_type, method, arnaught))
        if(!dir.exists('simplots/')){ dir.create('simplots/')}
        ggsave(filename = sprintf('simplots/prevalence_R0-%.1f_%s_%s.png', arnaught, model_type, method), width = 11, height = 8.5)
        
        ## Plot incidence in each compartment
        sim_results %>%
          select(time, int_time, dec_dur, dEI, dIR, incidence) %>%
          pivot_longer(dEI:incidence, names_to = 'transition', values_to = 'incidence') %>%
          ggplot() +
          geom_line(aes(x = time, y = incidence, color = int_time, linetype = dec_dur), size = 1, alpha = .5) +
          theme_bw()+
          facet_wrap(.~transition) +
          ggtitle(sprintf('%s - %s - R0=%.1f', model_type, method, arnaught))
        ggsave(filename = sprintf('simplots/incidence_R0-%.1f_%s_%s.png', arnaught, model_type, method), width = 11, height = 8.5)
      }
    }
  }
}

#testplots(PARAMS)




## ADDITIONAL UTILITY FUNCTIONS
## Output underlying R0 values for input into the SIR or SEIR model across once change (increase or decrease)
get_arnaught_step <- function(start_R0, 
                              final_R0, 
                              start_change,  ## Time at which R0 first departs from its initial value, start_R0
                              end_change,   ## Time at which R0 first reaches its final value, final_R0
                              n_t){  ## total timesteps
  arnaught <- if(start_R0==final_R0){
    #constant arnaught
    start_R0
  } else if(start_change==end_change){
    #step function arnaught
    swap_time <- start_change + 1
    c(rep(start_R0, swap_time), rep(final_R0, n_t-swap_time))
    
  } else {
    #linearly decreasing intervention arnaught
    c(rep(start_R0, start_change), seq(start_R0, final_R0, length.out=(end_change-start_change+1)), rep(final_R0, n_t - end_change))
  } 
}

## Repeatedly call get_arnaught step to specify R0 across an arbitrary number of changes
specify_arnaught <- function(R0_vec, ## Vector of equlibrium R0 values. Should be 1 greater in length than the desired number of changes.
                             change_start_vec, ## Vector of timepoints at which R0 starts to change. Should be length(R0_vec) - 1
                             change_end_vec, ## Vector of timepoints at which R0 first reaches its new value. Should be length(R0_vec) - 1
                             NT){ ## Scalar - total number of timepoints.
  stopifnot(length(change_end_vec) == length(change_start_vec))
  stopifnot(length(change_start_vec) == length(R0_vec)-1)
  stopifnot(all(diff(change_start_vec)>0))
  stopifnot(all(diff(change_end_vec)>0))
  arnaught <- NULL
  n.changes <- length(R0_vec)-1
  breakpoints <- c(0, change_start_vec[-1]-1, NT+1)
  stopifnot(all(breakpoints[-1]>change_end_vec))
  for(ii in 1:n.changes){
    arnaught <- c(arnaught,
                  get_arnaught_step(start_R0 = R0_vec[ii], 
                                    final_R0 = R0_vec[ii+1], 
                                    start_change = change_start_vec[ii]-breakpoints[ii], 
                                    end_change = change_end_vec[ii]-breakpoints[ii], 
                                    n_t = diff(breakpoints)[ii]-1)
    )
  }
  #cbind(arnaught, 0:NT)
  arnaught
}

## Wrappers that call simulate_sir defined in simulation.R
simulate_sir_example <- function(
  arnaught, t_I, N, I_init, n_t, n_steps_per_t = 10,
  method = 'stochastic'
) {
  simulate_seir(
    arnaught = arnaught,
    t_E = 0,
    t_I = t_I,
    N = N,
    S_init = N - I_init,
    E_init = 0,
    I_init = I_init,
    n_t = n_t, n_steps_per_t = n_steps_per_t,
    method = method
  )
}


## Wrappers that call simulate_seir defined in simulation.R
simulate_seir_example <- function(
  arnaught, t_E, t_I, N, E_init, I_init, n_t, n_steps_per_t = 10,
  method = 'stochastic'
) {
  simulate_seir(
    arnaught = arnaught,
    t_E = t_E,
    t_I = t_I,
    N = N,
    S_init = N - E_init - I_init,
    E_init = E_init,
    I_init = I_init,
    n_t = n_t, n_steps_per_t = n_steps_per_t,
    method = method
  )
}


## Function to load saved outputs form a simulation run
load_sims_for_one_R0 <-  function(arnaught, model_type = 'seir', method = 'stochastic'){
  data.frame(fns = list.files(path = sprintf('R0-%.1f', arnaught))) %>%
    filter(grepl(fns, pattern = method)) %>%
    pull(fns) %>%
    lapply(FUN = function(xx){
      readRDS(paste0(sprintf('R0-%.1f/', arnaught), xx)) -> tmp
      tmp$sim_df %>% 
        mutate(int_time = as.character(tmp$intervention_time), 
               dec_dur = as.character(tmp$decrease_duration))
    }) %>%
    bind_rows()
}



## Write a function to extract the simulation results as a data frame
get_sim_df <- function(){
  readRDS(sprintf('R0-%.1f/seir_%s_dec%.0f-%.0f_sim.rds', 
                  parlist$pre_intervention_R0, 
                  parlist$methods,
                  parlist$intervention_time_1, 
                  parlist$days_intervention_to_min))$sim_df %>%
    mutate_all(.funs = function(xx){ifelse(is.na(xx), 0, xx)}) %>%
    mutate(incidence = round(dS))
}


## Function to replace NAs with 0s in simulation output
na_to_0 <- function(vec){
  if(any(is.na(vec))){
    warning(sprintf('WARNING: Replacing NAs in %s with 0s\n', deparse(substitute(vec))))
    vec[is.na(vec)] = 0
  }
  vec
}