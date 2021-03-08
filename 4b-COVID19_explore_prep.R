# Purpose: define the variables and functions needed to explore the 
# Johns Hopkins COVID-19 time series data set.
# A location is specified at the state or locale level.
# A locale is the location (place) associated with a corresponding time series. 
# Locale specifications include the state name separated by a comma.


# Create stamp (function) for stamping figures with date info.
# Note: using words with numbers throws off date-format detection, ie: COVID-19.
stampf <- stamp(
  x = "(JHU CSSE data obtained on 1 January 1970)",
  orders = "dmy",
  quiet = TRUE
)


# List of all states, commonwealths, and territories
states  <- levels(factor(jhu_US_data$State))

# List of all locales (cities/towns)
locales <- levels(factor(jhu_US_data$Locale))


# Get population
get_pop <- function(location) {
  
  # Calculate the population for the given location
  if (location %in% states) {
    
    N <- population_data_US %>%
      group_by(State) %>%
      summarize(state_pop = sum(Population), .groups = "drop") %>%
      filter(State == location) %>%
      pull(state_pop)
    
  }
  
  else if (location %in% locales) {
    
    N <- population_data_US %>%
      filter(Locale == location) %>%
      pull(Population)  
    
  }
  
  else {
    
    stop("Error: get_pop() - Invalid location spec")
    
  }
  
  
  return(N)
  
}


# Date that cases start to appear
get_init_cases_date <- function(location) {
  
  if (location %in% states) {
    
    date <- jhu_US_data  %>%
      filter(State == location, Cases != 0) %>%
      select(Date)  %>%
      arrange(Date) %>%
      pull(Date) %>%
      head(1)
    
  }
  
  else if (location %in% locales) {
    
    date <- jhu_US_data  %>%
      filter(Locale == location, Cases != 0) %>%
      select(Date)  %>%
      arrange(Date) %>%
      pull(Date) %>%
      head(1)
    
  }
  
  else {
    
    stop("Error: get_init_cases_date() - Invalid location spec")
    
  }
  
  
  return(date)
  
}


# Date of the most recently reported cases
get_last_cases_date <- function(location) {
  
  if (location %in% states) {
    
    date <- jhu_US_data %>%
      filter(State == location, Cases != 0) %>%
      select(Date) %>%
      arrange(desc(Date)) %>%
      pull(Date) %>%
      head(1)
    
  }
  
  else if (location %in% locales) {
    
    date <- jhu_US_data %>%
      filter(Locale == location, Cases !=0) %>%
      select(Date) %>%
      arrange(desc(Date)) %>%
      pull(Date) %>%
      head(1)
    
  }
  
  else {
    
    stop("Error: get_last_cases_date() - Invalid location spec")
    
  }
  
  
  return(date)
  
}


# Dates between when 1st and last cases reported
# If dates are supplied check their validity
get_COVID_dates <- function(location) {
  
  init_cases_date <- get_init_cases_date(location)
  last_cases_date <- get_last_cases_date(location)
  
  if (location %in% states) {
    
    dates <- jhu_US_data %>%
      filter(State == location) %>%
      filter(between(Date, init_cases_date, last_cases_date)) %>%
      select(Date)   %>%
      distinct(Date) %>%
      arrange(Date)  %>%
      pull(Date)
    
  }
  
  else if (location %in% locales) {
    
    dates <- jhu_US_data %>%
      filter(Locale == location) %>%
      filter(between(Date, init_cases_date, last_cases_date)) %>%
      select(Date)  %>%
      arrange(Date) %>%
      pull(Date)
    
  }
  
  else {
    
    stop("Error: get_COVID_dates() - Invalid location spec")
    
  }
 

  return(dates)
  
}


# Check date validity and return indices.
# arguments 8 digit date in ymd format 
# return indices for date specs based on location
# auto-correct order of dates specified
# Bad programming practice to set a default location for check-function.
# The program should fail if their is a mistake...
# ***ALSO NEED TO CHECK: start_date != stop_date
get_date_indices <- function(location,
                             delay_days = 0,
                             lag_days = 0,
                             start_date = NULL,
                             stop_date = NULL) {
 
  dates <- get_COVID_dates(location = location)
 
  first_date <- dates[1]
   last_date <- dates[length(dates)]

  ########################################
  # If start_date is set and is not NULL #
  ########################################
 
  if (!missing(start_date) & !is.null(start_date)) {

    # Remove punctuation and check format
    if (nchar(str_remove_all(start_date, "[[:punct:]]")) != 8) {

      stop("Error: invalid start_date format")

    }

    # Check range
    else if ((ymd(start_date) < first_date) |
             (ymd(start_date) >= last_date)) {

      stop("Error: start_date outside valid range")

    }

    else {
      
      # Get index
      start_index <- which(dates == ymd(start_date))
      
    }
    
  }
  
  # Default starting index 
  else if (missing(start_date) | is.null(start_date)) {

    start_index <- 1 + delay_days

  }
 
  #######################################
  # If stop_date is set and is not NULL #
  #######################################
  
  if (!missing(stop_date) & !is.null(stop_date)) {

    # Remove punctuation and check format
    if (nchar(str_remove_all(stop_date, "[[:punct:]]")) != 8) {

      stop("Error: invalid stop_date format")

    }

    # Check range
    else if ((ymd(stop_date) <  dates[1]) |
             (ymd(stop_date) >= dates[length(dates)])) {

      stop("Error: stop_date outside valid range")

    }
    
    else {
      
      # Get index
      stop_index <- which(dates == ymd(stop_date))
      
    }
    
  }
  
  # Default stop index 
  else if (missing(stop_date) | is.null(stop_date)) {

    stop_index <- length(dates) - lag_days

  }
   
   
  # Final dates/indices range check: auto-correct order start/stop specs
  indices <- sort(c(start_index, stop_index))
  indices <- setNames(indices, c("start_index", "stop_index"))
 
 
  return(indices)

}
# Tests
get_date_indices("California")
get_date_indices("California", delay_days = 1, lag_days = 1)
get_date_indices("California", delay_days = 2)
get_date_indices("California",   lag_days = 2)
get_date_indices("California", start_date = 20201010)
get_date_indices("California",  stop_date = 20201010)


# Get the raw COVID data
# Set the start and stop dates with a single 8 digit number in year-month-day
# (ymd) format as single number padding single digit months and days with 0's.
get_COVID_data <- function(location,
                           start_date = NULL,
                           stop_date  = NULL) {
 
  # Dates between when 1st and last cases reported
  dates <- get_COVID_dates(location)

  indices <- get_date_indices(location   = location,
                              start_date = start_date,
                              stop_date  = stop_date)

  start_index <- indices["start_index"]
  stop_index  <- indices[ "stop_index"]

  # Cases between when 1st and last cases reported
  if (location %in% states) {

    cases <- jhu_US_data %>%
      filter(State == location) %>%
      filter(between(Date, dates[start_index], dates[stop_index])) %>%
      group_by(Date) %>%
      summarize(state_cases = sum(Cases), .groups = "drop") %>%
      pull(state_cases)
    
    deaths <- jhu_US_data %>%
      filter(State == location) %>%
      filter(between(Date, dates[start_index], dates[stop_index])) %>%
      group_by(Date) %>%
      summarize(state_deaths = sum(Deaths), .groups = "drop") %>%
      pull(state_deaths)

  }

  else if (location %in% locales) {

    cases <- jhu_US_data %>%
      filter(Locale == location) %>%
      filter(between(Date, dates[start_index], dates[stop_index])) %>%
      pull(Cases)
    
    deaths <- jhu_US_data %>%
      filter(Locale == location) %>%
      filter(between(Date, dates[start_index], dates[stop_index])) %>%
      pull(Deaths)
    
  }
  
  else {
    
    stop("Error: get_COVID_data() - Invalid location spec") 
    
  }
  
  
  return(
    data.frame(
      location = location,
      index = seq(1, stop_index - start_index + 1),
      steps = seq(start_index, stop_index),
      dates = dates[start_index:stop_index],
      cases = cases,
      deaths= deaths
    )
  )
  
}
get_COVID_data("Los Angeles, California")


# Get total number of COVID cases for a state or locale
get_COVID_totals <- function(location) {
  
  if ((location %in% states) | (location %in% locales)) {
    
    data <- get_COVID_data(location = location)
    total_cases <- data$cases[length(data$cases)]
    total_deaths<- data$deaths[length(data$deaths)]
    
  }
  
  else {
    
    stop("Error: total_COVID_cases() - Invalid location spec") 

  }
  
  N <- get_pop(location)

 
  return(
    data.frame(
      "Location"  = location,
      Total_cases = total_cases,
      Total_dead  = total_deaths,
      Percent_cases = total_cases/N*100,
      Percent_dead  = total_deaths/N*100,
      Odds_death  = total_deaths/total_cases*100
    )
  )

}
# Test
get_COVID_totals("Los Angeles, California")


# Plot raw data
plot_COVID_cases <- function(location,
                             start_date = NULL,
                              stop_date = NULL) {
 
  raw <- get_COVID_data(location   = location,
                        start_date = start_date,
                         stop_date =  stop_date)
 
  raw %>%
    ggplot(aes(x = dates, y = cases)) +
    geom_line() +
    theme_light() +
    labs(

      title = "Raw Cases Data v. Date",
      subtitle = bquote(.(location)),
      x = "Date",
      y = "Number of Recorded Cases"

    ) +
    theme(

      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 1.0)

    )

}
# Test
plot_COVID_cases("California")


# Map of US States in North America color coded according to variable.
# Variables:
# data - is the data frame
# join_column - this column should contain state names.
# variable - is the observable used to generate color gradient scale.
# title - is a string for the plot title
# legend - is a string for the title of the gradient scale.
# caption - is a string for the plot caption.
# Prerequisites:
# aoi_boundary - obtained with sf package
# stampf - defined with lubridate::stamp() 
plot_map <- function(data,
                     join_column,
                     variable,
                     title = NULL,
                     legend = NULL,
                     caption = NULL) {
  
  data %>%
    rename(NAME = {{join_column}}) %>%
    full_join(aoi_boundary,
              by = "NAME") %>%
    filter(!(NAME == "Alaska"
             | NAME == "Hawaii" 
             | NAME == "Northern Mariana Islands" 
             | NAME == "Virgin Islands" 
             | NAME == "District of Columbia" 
             | NAME == "Puerto Rico"
             | NAME == "United States Virgin Islands"
             | NAME == "Commonwealth of the Northern Mariana Islands"
             | NAME == "Guam"
             | NAME == "American Samoa")) %>%
    mutate(NAME = state.abb[match(NAME, state.name)]) %>%
    ggplot(
      aes(
        geometry = geometry,
        fill  = {{variable}},
      )
    ) +
    scale_fill_gradientn(
      colors = c(
        "red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta"
      )
    ) +
    geom_sf(size = 1, color = "black") +
    geom_label_repel(
      aes(label = NAME, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      color = "black",
      segment.colour = "black"
    ) +    
    coord_sf() +
    theme_void() +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, vjust = -1.0)
    ) +
    labs(
      subtitle = "(U.S. States in North America)",
      caption = bquote(atop(.(caption) ~ "\n", ~ .(stampf(today()))))
    ) +
    guides(
      fill = guide_colourbar(
        title = legend, 
        title.hjust = 0.5, # (not working)
        barheight = 9,
        barwidth = 0.5
      )
    )
  
}
#Common error: cannot use scale_color_gradient, must use scale_fill_gradient.

