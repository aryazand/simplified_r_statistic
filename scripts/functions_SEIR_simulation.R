# This is code to simulate a pandemic using an SEIR model
# I rely heavily EpiModel package to create

library(EpiModel)

SEIR <- function(t, t0, parms) {
  
  # This is a function taken from the EpiModel tutorial website 
  # It uses a deterministic SEIR model to simulate an pandemic 
  
  with(as.list(c(t0, parms)), {
    
    # Population size
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and FOI from a rearrangement of Beta * c * D
    ce <- R0 / i.dur
    lambda <- ce * i.num/num
    
    dS <- -lambda*s.num
    dE <- lambda*s.num - (1/e.dur)*e.num
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num,
         ei.prev = (e.num + i.num)/num)
  })
}

# Inputs:
#' @param ndays number of days to simulate
#' @param R0_vector a vector giving R0 values that occurred in the simulated pandemic
#' @param R0_days a vector giving the days start and stop day for each R0 value
#' @param s.num the size of starting susceptible population
#' @param e.num the size of starting exposed population  
#' @param i.num the size of starting infectious population
#' @param r.num the size of starting recovered population
#' @param se.flow the starting flow of s to e state   
#' @param ei.flow the starting flow of e to i state  
#' @param ir.flow the starting flow of i to r state 
#' @param d.flow the death rate  
#' @param e.dur duration in exposed state
#' @param i.dur duration in infectious state

SEIR_Variable_R0 <- function(PARAMS) {
  
  # Interpolate R0 at change points
  R0_over_time = approx(PARAMS$R0_days, PARAMS$R0_vector, n=PARAMS$ndays) 
  R0_over_time = R0_over_time$y
  
  # State an empty data.fram to store output data 
  df = list()
  
  
  CHANGING_VARS = PARAMS
  
  while(length(R0_over_time) > 0) {
    
    R0 = R0_over_time[1]
    
    if(any(R0_over_time != R0)) {
      num_days_to_model = which(R0_over_time != R0)[1]  
      R0_over_time = tail(R0_over_time, -1*(num_days_to_model-1))
    } else {
      num_days_to_model = length(R0_over_time)
      R0_over_time = NULL
    }
  
    param <- param.dcm(R0 = R0, e.dur = PARAMS$e.dur, i.dur = PARAMS$i.dur, cfr = PARAMS$cfr)
    
    init <- with(CHANGING_VARS, {
      
      init.dcm(s.num = s.num, e.num = e.num, i.num = i.num, r.num = r.num,
               se.flow = se.flow, ei.flow = ei.flow, ir.flow = ir.flow, d.flow = d.flow)
      
    })
    
    control <- control.dcm(nsteps = num_days_to_model, dt = 1, new.mod = SEIR)
    mod <- dcm(param, init, control)
    
    CHANGING_VARS = as.list(tail(as.data.frame(mod),1))
    
    mod = tail(as.data.frame(mod), -1)
    mod$R0 = R0 
    df = rbind(df, as.data.frame(mod))
  }
  
  return(df)
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

random_parameters_within_limits <- function() {
  PARAMS <- list(
    ndays = 300,
    R0_vector = seq(0.5,4,0.1) %>% sample(size=3) %>% map(., function(x) c(x, rnorm(1, mean=x, sd=0.2*x))) %>% unlist(),
    R0_days = sample(1:100, size=5) %>% c(1,.) %>% .[order(.)],
    s.num = sample(1e4:1e7, size=1),
    e.num = rnorm(1, mean=100) %>% round(),
    i.num = rnorm(1, mean=50) %>% round(),
    r.num = 0,
    se.flow = 0,
    ei.flow = 0,
    ir.flow = 0, 
    d.flow = 0,
    e.dur = 3,
    i.dur = 7
  )
  
  PARAMS$N = PARAMS$s.num + PARAMS$e.num + PARAMS$i.num + PARAMS$r.num    
  
  return(PARAMS)
}

adjust_parameters <- function(PARAMS) {
  PARAMS$ndays = round(PARAMS$ndays)
  PARAMS$R0_days = round(PARAMS$R0_days)
  PARAMS$R0_days = PARAMS$R0_days[order(PARAMS$R0_days)]
  PARAMS$s.num = round(PARAMS$s.num)
  PARAMS$e.num = round(PARAMS$e.num)
  PARAMS$i.num = round(PARAMS$i.num)
  PARAMS$r.num = round(PARAMS$r.num)
  PARAMS$e.dur = round(PARAMS$e.dur)
  PARAMS$i.dur = round(PARAMS$i.dur)
  PARAMS$N = PARAMS$s.num + PARAMS$e.num + PARAMS$i.num + PARAMS$r.num  
  return(PARAMS)
}

create_R0_trend <- function(PARAMS) {
  R0_over_time = approx(x=PARAMS$R0_days, y=PARAMS$R0_vector, n=PARAMS$ndays) 
  R0_over_time = R0_over_time$y
  R0_over_time = c(R0_over_time, tail(R0_over_time,1))
  return(R0_over_time)
}

simulate_seir_ode_wrapper <- function(PARAMS) {
  R0_over_time = create_R0_trend(PARAMS)
  
  df <- simulate_seir_ode(arnaught = R0_over_time, t_E = PARAMS$e.dur, t_I = PARAMS$i.dur, N = PARAMS$N, 
                          S_init = PARAMS$s.num, E_init = PARAMS$e.num, I_init = PARAMS$i.num, n_t = PARAMS$ndays) %>% 
    mutate(incidence = round(dS), obs_cases = round(dEI)) %>%
    mutate(N = (S+E+I+R)) %>%
    mutate(true_r0 = R0_over_time, true_rt = R0_over_time*S/N)  
  
  return(df)
}