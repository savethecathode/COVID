## Purpose: improve guess for parameters beta and gamma before using optim()
## Compute RSS values for a range of beta and gamma values.
## The beta and gamma pair with the lowest RSS will be used as the starting
##... parameter set for optim()


## Specify the name of the location to examine
## To see a list of available locations use: unique(dat$Locale)
the_location = "Cook, Illinois"


## Set number of days to skip since the 1st cases are seen before we begin modeling the data
## Set lag based on when 5000 people become infected/infectious
the_lag <- as.numeric(date_of_X_infectious(the_location) - get_first_cases_date(the_location))



## Set the number of beta and the number of gamma values to test.
## (n) must be large enough to achieve sufficient granularity such that a reasonable pair of
##... beta and gamma values can be found to begin optimization with an expectation of a descent fit.
## n = 10 is not large enough.
n = 100


## Set upper boundaries for beta and gamma values
gamma_min = 0.0 # 0.1 # 1 / 30
gamma_max = 1.0 # 0.8 # 1 / 5 #0.2
beta_min  = 0.0 # 0.1 # 4 * gamma_min
beta_max  = 1.0 # 2.0 # 4 * gamma_max #0.8


## Make sequences of beta and gamma values to test
betas  <- seq(beta_min , beta_max , len=n)
gammas <- seq(gamma_min, gamma_max, len=n)





########################################################################
## Define function to evaluate SIR model based on (RSS) loss function ##
########################################################################

## Input parameters:
## args  <vector of doubles> - beta and gamma (in that order) coefficients in SIR model.
## Note: this function takes parameters beta and gamma as a single vector for compatibility with optim().
## location  <string> - "city/town, state" format, ie: "New York City, New York".
## lag  <int> - skip this number of days since the 1st cases are seen before we begin modeling the data.

fit_SIR_model <- function(args, location = "New York City, New York", lag = 0) {

  #####################################
  ## Define parameters for SIR model ##
  #####################################

  ## Set beta and gamma parameter values from input
  parameter_values <- as.numeric(args)      # Convert parameters passed as strings to numbers


  ## Calculate the population for the given location
  N <- population_data_US %>%
    filter(Locale == location) %>%
    pull(Population)

  parameter_values <- append(parameter_values, N)   # Append population data to SIR parameters
 
 
  ## ode() requires parameters be passed as a vector with named components
  parameter_values <- setNames(parameter_values, c("beta", "gamma", "N"))
  
  
  
  #########################################
  ## Get raw data for specified location ##
  #########################################
  
  raw_data <- get_raw_data(location)
  
  
  
  ###########################################
  ## Make a vector to increment dates/time ##
  ###########################################
  
  ## Note: The deSolve function ode() is old-school and does not accept date objects.
  ## Set a lag value to test the influence on fitting of starting at a later date
  days <- 1:(length(raw_data$infected) - lag)
  
  
  
  #################################################
  ## Set initial conditions for the SIR model ##
  #################################################
  
  ## Set a lag value to test the influence on fitting of starting at a later date
  model_start_day <- 1 + lag
  
  initial_values <- c(
    
    S = N - raw_data$infected[model_start_day],  # number of sussceptible people
    I = raw_data$infected[model_start_day],      # no. infectious
    R = raw_data$recovered[model_start_day]      # no. recovered (and immune), or deceased
    
  )
  
  
  
  ########################
  ## Evaluate SIR Model ##
  ########################
  
  ## Solve system of 3 simultaneous equations
  predictions <- ode(
    
    y = initial_values,
    times = days,
    func  = SIR,
    parms = parameter_values
    
  )
  
  
  return(data.frame(predictions))
  
}

## Test fit_SIR_model() function
fit_SIR_model(c(0.5, 0.5), location = the_location, lag = the_lag)





###########################################################################
## Evaluate the Fit using the SIR Model based on the (RSS) Loss Function ##
###########################################################################

get_SIR_fit_RSS <- function(args, location, lag) {
  
  ## Extract raw data
  raw_data <- get_raw_data(location, lag)
  
  
  ## Compute fitted data
  fit_data <- fit_SIR_model(args, location, lag)
  
  
  ## Compute the residual sum of squares for fit v. raw for infectious data
  return( RSS(fit_data$I, raw_data$infected) )
  
}



###########
## Tests ##
###########


## Test get_raw_data() function
#get_raw_data(location = the_location, lag = the_lag)

## Test get_SIR_fit_RSS() function
#get_SIR_fit_RSS(c(0.5, 0.5), location = the_location, lag = the_lag)

## Test run on single set of beta and gamma values
#get_SIR_fit_RSS(c(0.5, 0.5), location = the_location, lag = the_lag)



#############################################################
## Evaluate the SIR Model based on the (RSS) Loss Function ##
#############################################################

## n  - is the number of values of beta and gamma to test
## Gamma can be estimated based on the average duration of the illness, as 1/(duration of illness)
## Upper boundary of 0.2 based on guess of quickest recovery of 5 days
## There is no good way to guess the value of beta
## Guess upper boundary of 0.8 based on the ave. R_0 of ~ 2 for the 1918 Spanish Flu
## Recall: R_0 = beta / gamma


run_sir_rss_tests <- function(beta_seq = betas, gamma_seq = gammas, n = 10, location, lag) {

  ## Create a set of betas and gammas to test
  ## generate all combinations of beta and gamma values
  parameter_set <- expand.grid(beta_seq, gamma_seq)  # Object is of class: data.frame, and mode: list

  names(parameter_set) <- c("beta", "gamma")   # Rename Var1 and Var2 columns to beta and gamma


  ## Combine column entries into single row vector to pass elements as single parameter vector to get_sir_fit_rss()
  ##... this is less of an aesthetic choice than a need to conform to the same argument specifications as optim().
  ##... such that the same function that is used to evaluate the model can be used for optimization.
  ## The result is rows of string vectors (having trouble converting to numeric vector).
  parameter_set <- parameter_set %>%
    transmute(parameters = as.vector(strsplit(paste(beta, gamma), " ")))
  #view(parameter_set)  # Examine structure of parameter set


  ## Compute RSS for SIR model on COVID-19 data for combinations of beta and gamma test values

  rss_tests_results <- parameter_set %>%
    mutate(

      values = map(

        parameters,
        get_SIR_fit_RSS,
        location = location,
        lag = lag

        )

      )


  return(rss_tests_results)

}


################################
## Run RSS Tests on SIR Model ##
################################


time_init <- Sys.time()  # Start timer


rss_tests_results <- run_sir_rss_tests(location = the_location, lag = the_lag)


time_elapsed <- Sys.time() - time_init # Run time
time_elapsed


