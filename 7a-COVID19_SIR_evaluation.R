########################################################################
## Define function to evaluate SIR model based on (RSS) loss function ##
########################################################################

# Purpose: improve guess for parameters beta and gamma before using optim()
# Compute RSS values for a range of beta and gamma values.
# The beta and gamma pair with the lowest RSS will be used as the starting
# parameter set for optim().

#################################################################
# Specify a test location:
# To see a list of available locations use: unique(dat$Locale) #
# Cook, IL is chosen as a testing locale based on k-means clustering 
# because it serves as a source of predictive insight for NYC, NY
################################################################
test_location = "Cook, Illinois"  # default Locale spec


###########################################################################
# Set: delay_days time - the number of days to skip after the 1st cases are seen #
###########################################################################

# Bad default delay_days - begin modeling once 1st cases are observed
# default_delay_days <- 0

# Improved default
# Set delay_days based on when 5000 (default) people become infected/infectious
# Note: if locale does not have this many cases yet, then *AHEAD* get error: 
# Error in start_index:stop_index : argument of length 0 
# default_delay <- as.numeric(
#   date_of_X_infectious(test_location) - get_init_casess_date(test_location))

# Production default
# Set delay for the past two weeks
default_delay <- delay_at_past_X_days(location = test_location,
                                    x_days   = 14,
                                    ref_date = tail(
                                      get_SIR_data(location = test_location)$date,
                                      n=1))


################################
## Define SIR model for ode() ##
################################

SIR <- function(time, sir_variables, sir_parameters) {
  
  with(as.list(c(sir_variables, sir_parameters)), {
    
    dS <- - beta * S * I / N
    dI <-   beta * S * I / N - gamma * I
    dR <-  gamma * I
    
    return( list( c(dS, dI, dR) ) )
    
  })
}


#####################################################
# Define SIR fit function by composition with ode() #
#####################################################

# Input parameters:

# params <vector of doubles> - beta and gamma (in that order)
# Note: this function takes parameters beta and gamma as a single vector for
# compatibility with optim().
# Gamma can be estimated based on the average duration of the infectious period
# as 1/(infectious duration)
# Upper boundary of 0.2 based on guess of quickest recovery of 5 days.
# There is no good way to guess the value of beta other than perhaps by 
# comparison to other pandemics,
# ie: deduction from the ave. R_0 of ~ 2 for the 1918 Spanish Flu.
# Recall: R_0 = beta / gamma

# location  <string> - "city/town, state" format, ie: "New York City, New York".

# There are two ways to specify a region over which to apply fitting.
# 1st is to specify a delay time
# delay_days  <int> - skip this number of days since the 1st cases are seen. 
# 2nd is to specify a date-range in ymd format, which overrides the delay_days spec.

fit_SIR_model <- function(params,
                          location = test_location,
                          delay_days = 0,
                          infectious_period = default_infectious_period,
                          start_date = NULL,
                          stop_date  = NULL) {
  
  #####################################
  ## Define parameters for SIR model ##
  #####################################
  
  # Set beta and gamma parameter values from input
  # Convert parameters passed as strings to numbers
  parameter_values <- as.numeric(params)
  
  N <- get_pop(location)
  
  # Append population data to SIR parameters
  parameter_values <- append(parameter_values, N)
  
  
  # ode() requires parameters be passed as a vector with named components
  parameter_values <- setNames(parameter_values, c("beta", "gamma", "N"))
  
  
  #########################################
  ## Get raw data for specified location ##
  #########################################
  # Note: set a delay_days value to test the influence of starting at a later date
  raw_data <- get_SIR_data(location = location,
                           delay_days = delay_days,
                           infectious_period = infectious_period,
                           start_date = start_date,
                           stop_date  = stop_date)
  
  
  ######################################
  ## Get initial values for SIR model ##
  ######################################
  
  initial_values <- c(
    S = raw_data$raw_S[1],  # number of susceptible people
    I = raw_data$raw_I[1],  # no. infectious
    R = raw_data$raw_R[1]   # no. recovered (and immune), or deceased
  )
  
  
  ###########################################
  ## Make a vector to increment dates/time ##
  ###########################################
  # Note: The deSolve function ode() is old-school does not accept date objects.
  days <- 1:length(raw_data$raw_I)
  
  
  ########################
  ## Evaluate SIR Model ##
  ########################
  # Solve system of 3 simultaneous equations
  predictions <- ode(
    y     = initial_values,
    times = days,
    func  = SIR,
    parms = parameter_values
  )
  
  
  # Rename variables for future clarity
  predictions <- data.frame(predictions) %>%
    rename(
      index = time,
      fit_S = S,
      fit_I = I,
      fit_R = R
    )
  
  return(predictions)
}


###########################################################################
## Evaluate the Fit using the SIR Model based on the (RSS) Loss Function ##
###########################################################################

# Define loss function as residual sum of squares
RSS <- function(prediction, raw_data) {
  
  return( sum( (prediction - raw_data)^2 ) )
}

# Function for composition with optim()
get_SIR_RSS <- function(params,
                        location,
                        delay_days = 0,
                        infectious_period = default_infectious_period,
                        start_date = NULL,
                        stop_date  = NULL) {
  
  # Extract raw data
  raw_data <- get_SIR_data(location = location,
                           delay_days = delay_days,
                           infectious_period = infectious_period,
                           start_date = start_date,
                           stop_date  =  stop_date)
  
  # Compute fitted data
  fit_data <- fit_SIR_model(params   = params,
                            location = location,
                            delay_days = delay_days, 
                            infectious_period = infectious_period,
                            start_date = start_date,
                            stop_date  =  stop_date)
  
  # Compute the residual sum of squares for fit v. raw for infectious data
  return( RSS(fit_data$fit_I, raw_data$raw_I) )
}


#############################################################
## Evaluate the SIR Model based on the (RSS) Loss Function ##
#############################################################

# beta_min and beta_max set the lower and upper boundaries of beta to scan
# gamma_min and gamma_max set the lower and upper boundaries of gamma to scan
# n_betas and n_gammas are the number of values of beta and gamma to scan
# (n) must be large enough to achieve sufficient granularity such that a
# reasonable pair of beta and gamma values can be found to begin optimization
# with an expectation of a descent fit.
# n = 10 generally is not large enough.

# Set default scan values
beta_min  = 0.1
beta_max  = 1.0
n_betas   = 25
gamma_min = 0.1
gamma_max = 1.0
n_gammas  = 25
betas     = seq(beta_min , beta_max , len = n_betas)
gammas    = seq(gamma_min, gamma_max, len = n_gammas)

scan_SIR_RSS <- function(beta_seq  = betas,
                         gamma_seq = gammas,
                         location  = test_location,
                         delay_days = 0,
                         infectious_period = default_infectious_period,
                         start_date = NULL,
                         stop_date  = NULL) {
 
  # Parallelization option
  require("furrr")

  # Create a set of betas and gammas to test
  # generate all combinations of beta and gamma values
  # Object is of class: data.frame, and mode: list
  parameter_set <- expand.grid(beta_seq, gamma_seq)
 
  # Rename Var1 and Var2 columns to beta and gamma
  names(parameter_set) <- c("beta", "gamma")
 
 
  # Combine column entries into single row vector to pass elements as single
  # parameter vector to get_SIR_RSS() this is less of an aesthetic choice
  # than a need to conform to the same argument specifications as optim()
  # such that the same function that is used to evaluate the model can be used
  # for optimization.  The result is rows of string vectors (having trouble
  # converting to numeric vector).
  parameter_set <- parameter_set %>%
    transmute(parameters = as.vector(strsplit(paste(beta, gamma), " ")))
  #view(parameter_set)  # (testing) examine structure of parameter set
  
  
  # Compute RSS for SIR model for all combinations of beta and gamma test values
  results_rss_scan <- parameter_set %>%
    mutate(

      #values = map(        # Normal version
      values = future_map(  # Parallel version
        parameters,
        get_SIR_RSS,
        location = location,
        delay_days = delay_days,
        infectious_period = infectious_period,
        start_date = start_date,
        stop_date  = stop_date
      )
    )
  
  return(results_rss_scan)
}



################################
## Test RSS Scan on SIR Model ##
################################

# Note: the middle of October suggested the pandemic was not over
start_date_spec = 20201007
stop_date_spec  = 20201021

time_init <- Sys.time()  # Start timer

# Note: default delay_days time looks at the past 2 weeks worth of data
# Cook_IL_SIR_scan <- scan_SIR_RSS(location = test_location,
#                                  delay_days = default_delay)

Cook_IL_SIR_scan <- scan_SIR_RSS(location   = test_location,
                                 start_date = start_date_spec,
                                 stop_date  = stop_date_spec)

time_elapsed <- Sys.time() - time_init # Run time
time_elapsed


###########
## Tests ##
###########

# Test get_SIR_data() function
# raw <- get_SIR_data(location = test_location, delay_days = default_delay)
# raw

# Test fit_SIR_model() function
# fit <- fit_SIR_model(c(0.5, 0.5), location = test_location, delay = default_delay)
# fit

#OBS: raw and fit are not the same length!!!

# Test run on single set of beta and gamma values
# rss <- get_SIR_RSS(c(0.5, 0.5),
#                        location = test_location,
#                        delay = default_delay)
# rss
