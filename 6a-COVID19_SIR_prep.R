# Purpose: generate SIR data from the raw cases data using the simplest set
# of assumptions.

###############################################################################
# Estimate the no. infectious individuals as:  Infectious = Cases - Recovered #
# A simplifying assumption applied is that one remains infectious while ill   #
###############################################################################

# Use the ave. recovery time to estimate the no. individuals who have recovered.
# WHO estimates one is infectious for 2 weeks, while symptoms can last up to 6+.
# Recall: there is no distinction made between recovered and deceased.
# Shifts data to the right changing leading positions now with missing values to
# 0 and truncating trailing values past the length of the input vector.
default_infectious_period = 14

# Note: infectious_period = 0 is unacceptable.  According to the current
# implementation this would lead to cases = recovered, which results in the
# population of infectious = 0 for all time! Therefore, infectious_period <= 0
# should be interpreted as no recoveries, which amounts to simply working with
# the raw cases data with an arbitrary start date.

# delay (default: 0) skip no. days after the 1st cases were observed.
# lag   (default: 0) omit no. days prior to current day.

# NOTE: date-range specs override delay/lag specs,
# ie: compute boundary indices using dates if they are supplied.

get_SIR_data <- function(location,
                         infectious_period = default_infectious_period,
                         delay_days = 0,
                           lag_days = 0,
                         start_date = NULL,
                          stop_date = NULL) {
  
  # Dates and cases between when 1st and last cases reported
  raw_data <- get_COVID_data(location)
  
  # Raw COVID data
  dates <- raw_data$dates
  cases <- raw_data$cases
  
  # Check start_date and stop_date (used to subset data)
  indices <- get_date_indices(location   = location,
                              delay_days = delay_days,
                                lag_days =   lag_days,
                              start_date = start_date,
                               stop_date =  stop_date)
  
  # Final dates/indices range check: auto-correct order start/stop specs
  start_index <- indices["start_index"]
  stop_index  <- indices[ "stop_index"]
  
  
  # Get population of given locale
  N <- get_pop(location)
  
  
  ##############################################################################
  # Estimate evolution of the recovered, infectious, and susceptible populations
  ##############################################################################
  
  if (infectious_period <= 0) {
    
    recovered <- rep(0, length(cases))
    
  }
  
  else {
    
    recovered <- c(
      
      rep(0, infectious_period),
      cases[seq(1, length(cases) - infectious_period)]

    )

  }
 
 
  infectious  <- cases - recovered

  susceptible <- N - infectious
 
 
  return(

    data.frame(

      index = seq(1, stop_index - start_index + 1),
      steps = seq(1, length(cases))[start_index:stop_index],
      dates = dates[start_index:stop_index],
      cases = cases[start_index:stop_index],
      raw_S = susceptible[start_index:stop_index],
      raw_I = infectious[ start_index:stop_index],
      raw_R = recovered[  start_index:stop_index]

    )

  )

}
# Lesson: when you know you have to perform a "join" in the future, you might
# just want to carry around the indices. 


# Plot SIR data
plot_SIR_data <- function(location,
                          infectious_period = default_infectious_period,
                         delay_days = 0,
                           lag_days = 0,
                         start_date = NULL,
                          stop_date = NULL) {
  
  raw_data <- get_SIR_data(location = location,
                           infectious_period = infectious_period,
                           delay_days = delay_days,
                             lag_days =   lag_days,
                           start_date = start_date,
                            stop_date =  stop_date)
  
  raw_data <- raw_data %>% pivot_longer(
    
    cols = - c("index", "steps", "dates", "cases"),
    names_to  = "key",
    values_to = "SIR"
    
  )
  
  raw_data %>%
    ggplot(aes(x = dates, y = SIR, color = key)) +
    geom_line() +
    theme_light() +
    labs(
      
      title = "Infectious Data v. Date",
      subtitle = bquote(.(location)),
      caption  = bquote(
        "*Days since the first cases are detected before modeling starts: "
        ~ .(delay_days)),
      x = "Date",
      y = "Number of People"
      
    ) +
    theme(
      
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 0.5)
      
    )
  
}

# Plot SIR Infectious data
plot_SIR_I_data <- function(location,
                            infectious_period = default_infectious_period,
                         delay_days = 0,
                           lag_days = 0,
                         start_date = NULL,
                          stop_date = NULL) {
  
  raw_data <- get_SIR_data(location = location,
                           infectious_period = infectious_period,
                           delay_days = delay_days,
                             lag_days = lag_days,
                           start_date = start_date,
                            stop_date =  stop_date)
  
  raw_data %>%
    ggplot(aes(x = dates, y = raw_I)) +
    geom_line() +
    theme_light() +
    labs(
      
      title = "Infectious Data v. Date",
      subtitle = bquote(.(location)),
      caption  = bquote(
        "*Days since the first cases are detected before modeling starts: "
        ~ .(lag)),
      x = "Date",
      y = "Number of Infectious People"
      
    ) +
    theme(
      
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      #plot.caption  = element_text(vjust = -2.0, hjust = 1.0)
      plot.caption  = element_text(vjust = -2.0, hjust = 0.5)
      
    )
  
}



#################################################
# Functions for Computing Model Delay (aka lag) #
#################################################


# Get the date that X no. infectious individuals are "observed" for a locale
# infectious_period is an argument to get_SIR_data() hence it can be passed here
date_of_X_infectious <- function(location,
                                 X = 300,
                                 infectious_period = default_infectious_period
                                 ) {
  
  sir_data <- get_SIR_data(location = location,
                           infectious_period = infectious_period)
  
  # Stop if x > max no. infectious
  if ( X > max(sir_data$raw_I)) {
    
    stop("Error: date_of_X_infectious() - 
         X value is above the max observed for ", paste(location))
    
  }
 
  # Get date of X no. infectious ("I" of SIR model)
  date <- sir_data %>%
    filter(raw_I >= X) %>%
    arrange(dates) %>%
    head(1) %>%
    pull(dates)


  return(date)

}


# Determine the lag (in days) for a given number of days (x_days) previous to
# a specific reference date (supply in "year-month-day" format)
delay_at_past_X_days <- function(location, x_days, ref_date) {

  require(lubridate)

  days <- as.numeric(ymd(ref_date) -
                     days(x_days)  -
                     get_init_cases_date(location)) 

 
  return(days) 

}
