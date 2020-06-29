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
  
  jhu_US_data %>%
    filter(Locale == location, Cases != 0) %>%
    select(Date)  %>%
    arrange(Date) %>%
    pull(Date) %>%
    head(1)
  
}


## Date of the most recently reported cases
get_recent_cases_date <- function(location) {
  
  jhu_US_data %>%
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
  
  jhu_US_data %>%
    filter(Locale == location) %>%
    filter(between(Date, first_cases_date, recent_cases_date)) %>%
    pull(Date)
  
}


## Get date that X number of infectious individuals are "observed" for a given locale
date_of_X_infectious <- function(locale, x = 5000) {
  
  get_raw_data(locale) %>%
    filter(infected >= x) %>%
    arrange(date) %>%
    head(1) %>%
    pull(date)
  
}


#######################################################################################
## Estimate the number of infectious individuals as:  Infectious = Cases - Recovered ##
#######################################################################################

get_raw_data <- function(location, lag = 0) {
  
  ## Date that cases start to appear
  first_cases_date <- get_first_cases_date(location)
  
  
  ## Date of the most recently reported cases
  recent_cases_date <- get_recent_cases_date(location)
  
  
  ## Cases between when 1st and last cases reported
  dates <- get_cases_dates(location)
  
  
  ## Cases between when 1st and last cases reported
  cases <- jhu_US_data %>%
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
    
    date  = dates[start_index:stop_index],
    cases = cases[start_index:stop_index],
    infected  = infected[start_index:stop_index],
    recovered = recovered[start_index:stop_index]
    
    )

  )
  
}

