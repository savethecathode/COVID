# Purpose: optimize beta and gamma for SIR fit

# The parameter space is restricted for simplicity.
optim_SIR <- function(init_params,
                      location = test_location,
                      delay_days = 0,
                      start_date = NULL,
                      stop_date  = NULL) {
 
  model <- optim(
    par = init_params,         # initial values
    location = location,       # optional parameter to get_SIR_fit_RSS()
    delay_days = delay_days,   # optional parameter to get_SIR_fit_RSS()
    start_date = start_date,   # optional parameter to get_SIR_fit_RSS()
    stop_date  = stop_date,    # optional parameter to get_SIR_fit_RSS()
    fn  = get_SIR_RSS,         # Function to be minimized: RSS of SIR fit
    method = "L-BFGS-B" ,      # Gradient projection method using BFGS matrix 
    lower  = c(0.1, 0.1),      # lower boundary for beta and gamma
    upper  = c(1, 1)           # upper boundary for beta and gamma
  )

  return(
    tibble(
      "location" = location,
      "delay_days"   = delay_days,
      "start_date" = ymd(start_date),
      "stop_date"  = ymd(stop_date),
      #rss_scan_results,
      "beta"  = model$par[1],
      "gamma" = model$par[2],
      "R0"  = model$par[1]/model$par[2],
      "RSS" = model$value,
      "convergence" = model$convergence,
      "message" = model$message
    )
  )
}


# Starting values for SIR parameters beta and gamma to perform optimization
starting_optim_params <-
  unlist(Cook_IL_SIR_scan$parameters[which.min(Cook_IL_SIR_scan$values)])


time_init <- Sys.time()


# Run optimization using date range.
Cook_IL_SIR_optim <- optim_SIR(init_params = starting_optim_params,
                               location    = test_location,
                               start_date  = start_date_spec,
                               stop_date   =  stop_date_spec)

time_elapsed <- Sys.time() - time_init
time_elapsed

# Show optimized parameters
Cook_IL_SIR_optim

# Apply optimized model
Cook_IL_SIR_ofit <- fit_SIR_model(
  
  c(Cook_IL_SIR_optim$beta, Cook_IL_SIR_optim$gamma),
  Cook_IL_SIR_optim$location,
  Cook_IL_SIR_optim$lag,
  start_date = start_date_spec,
  stop_date  = stop_date_spec,
  
)