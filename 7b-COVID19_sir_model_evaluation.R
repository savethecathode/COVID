## Purpose: improve guess for parameters beta and gamma before using optim()
## 
##


## Specify the name of the location to examine
## To see a list of available locations use: unique(dat$Locale)
the_location = "New York City, New York"


## Set number of days to skip since the 1st cases are seen before we begin modeling the data
the_lag = 21




## Define loss function as residual sum of squares
RSS <- function(prediction, raw_data) {
  
  return( sum( (prediction - raw_data)^2 ) )
  
}


## Define SIR model
SIR <- function(time, sir_variables, sir_parameters) {
  
  with(as.list(c(sir_variables, sir_parameters)), {
    
    dS <- - beta * S * I / N
    dI <-   beta * S * I / N - gamma * I
    dR <-  gamma * I
    
    return( list( c(dS, dI, dR) ) )
    
  })
  
}


## Date that cases start to appear
get_first_cases_date <- function(location) {
  
  jhu_data %>%
    filter(Locale == location, Cases != 0) %>%
    select(Date)  %>%
    arrange(Date) %>%
    pull(Date) %>%
    head(1)
  
}


## Date of the most recently reported cases
get_recent_cases_date <- function(location) {
  
  jhu_data %>%
    filter(Locale == location) %>%
    select(Date) %>%
    arrange(desc(Date)) %>%
    pull(Date) %>%
    head(1)
  
}


## Cases between when 1st and last cases reported
get_cases_dates <- function(location) {
  
  first_cases_date  <- get_first_cases_date(location)
  
  recent_cases_date <- get_recent_cases_date(location)
  
  jhu_data %>%
    filter(Locale == location) %>%
    filter(between(Date, first_cases_date, recent_cases_date)) %>%
    pull(Date)
  
}


#################################################################################
# Estimate the number of infected individuals as:  Infected = Cases - Recovered #
#################################################################################

get_raw_data <- function(location, lag = 0) {
  
  ## Date that cases start to appear
  first_cases_date <- get_first_cases_date(location)
  
  
  ## Date of the most recently reported cases
  recent_cases_date <- get_recent_cases_date(location)
  
  
  ## Cases between when 1st and last cases reported
  dates <- get_cases_dates(location)
  
  
  ## Cases between when 1st and last cases reported
  cases <- jhu_data %>%
    filter(Locale == location) %>%
    filter(between(Date, first_cases_date, recent_cases_date)) %>%
    pull(Cases)
  
  
  ## Use the average recovery time to estimate the number of individuals who have recovered.
  ## WHO estimates an average recovery time of ~2 weeks (14 days).
  ## Recall: there is no distinction made between recovered and deceased.
  ## Shifts data to the right changing leading positions now with missing values to 0
  ##... and truncating trailing values past the length of the input vector.
  recovery_time <- 14 # in days
  recovered <- c(
    
    rep(0, recovery_time),
    cases[seq(1, length(cases) - recovery_time)]
    
  )
  
  
  ## Estimate Infected data
  infected <- cases - recovered
  
  
  ## Index by day since the 1st cases are seen to begin modeling
  start_index <- 1 + lag
  
  ## Index by day since the 1st cases are seen to stop modeling
  stop_index <- length(cases)
  
  
  return(data.frame(
    
    dates = dates[start_index:stop_index],
    cases = cases[start_index:stop_index],
    infected  = infected[start_index:stop_index],
    recovered = recovered[start_index:stop_index]
    
  )
  )
  
}


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
  ## Set initialial conditions for the SIR model ##
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


## Create a set of betas and gammas to test
n <- 100  # number of parameter values to test

## Gamma can be estimated based on the average duration of the illness, as 1/(duration of illness)
gammas <- seq(0.0, 0.2, len=n)            # Upper boundary of 0.2 based on guess of quickest recovery of 5 days

## There is no good way to guess the value of beta
## Guess upper boundary of 0.8 based on the ave. R_0 of ~ 2 for the 1918 Spanish Flu
betas  <- seq(0.0, 0.8, len=n)            # Recall: R_0 = beta / gamma

## generate all combinations of beta and gamma values
parameter_set <- expand.grid(betas, gammas)  # Object is of class: data.frame, and mode: list
names(parameter_set) <- c("beta", "gamma")   # Rename Var1 and Var2 columns to beta and gamma


## Combine column entries into single row vector to pass elements as single parameter vector to get_sir_fit_rss()
##... this is less of an aesthetic choice than a need to conform to the same argument specifications as optim().
##... such that the same function that is used to evaluate the model can be used for optimization.
## The result is rows of string vectors (having trouble converting to numeric vector).
parameter_set <- parameter_set %>%
  transmute(parameters = as.vector(strsplit(paste(beta, gamma), " ")))
#view(parameter_set)  # Examine structure of parameter set


## Compute RSS for SIR model on COVID-19 data for combinations of beta and gamma test values
time_init <- Sys.time()  # Start timer

rss_tests_results <- parameter_set %>%
  mutate(values = map(

                      parameters,
                      get_SIR_fit_RSS,
                      location = the_location,
                      lag = the_lag

                      )
         )

time_elapsed <- Sys.time() - time_init # Run time
time_elapsed





#########################################
## Visualize the Model Evaluation Data ##
#########################################


# ## Convert data from RSS tests into matrix format
rss_tests_values <- matrix(unlist(rss_tests_results$values), n)


## 3D perspective plot of RSS tests data
persp(betas, gammas, -rss_tests_values, theta = 40, phi = 30,
      xlab = "beta", ylab = "gamma", zlab = "- RSS")


## 2D contour plot of RSS tests data



image(betas, gammas, rss_tests_values,
      main = bquote("RSS("~beta~","~gamma~") :"~.(the_location)),
      sub  = list(bquote("*Days since the first cases are detected before modeling starts: " ~ .(the_lag)), cex=0.75),
      ylab = expression(paste("recovery rate  ", gamma, " (/day)")),
      xlab = "",
      xlim = c(min(betas), max(betas)),
      ylim = c(min(gammas), max(gammas)),
      xaxs = "i", yaxs = "i"
)

# Adjust x-axis label only: shift up from subtitle
mtext(text = expression(paste("infection rate ", beta,  " (/day)")),
      side = 1, # bottom x-axis
      line = 2.5
)

# Add contour lines
contour(betas, gammas, rss_tests_values, add = TRUE, nlevels = 8)

# Add box around plot
box(bty = "o")


