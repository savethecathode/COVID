# Purpose: Illustrate COVID-19 data on maps


covid_totals <- map_dfr(states, get_COVID_totals)


# Map of total cases by state 
plot_map(
  data = covid_totals,
  join_column = Location,
  variable = Total_cases,
  title = "Total COVID-19 Cases by State",
  legend = "Cases"
)

# Map of total deaths by state 
plot_map(
  data = covid_totals,
  join_column = Location,
  variable = Total_dead,
  title = "Total COVID-19 Deaths by State",
  legend = "Deaths"
)

# Map of percent of the population that died from COVID-19 by state 
plot_map(
  data = covid_totals,
  join_column = Location,
  variable = Percent_cases,
  title = "Percent of the Population that has Contracted COVID-19 by State",
  legend = "(%)"
)

# Map of percent of the population that died from COVID-19 by state 
plot_map(
  data = covid_totals,
  join_column = Location,
  variable = Percent_dead,
  title = "Percent of the Population that has Died from COVID-19 by State",
  legend = "(%)"
)

# Map of odds of death for infected individuals by state 
# odds = no. deaths/ no. cases
plot_map(
  data = covid_totals,
  join_column = Location,
  variable = Odds_death,
  title = "Odds of Death from COVID-19 by State",
  legend = "Odds"
)


###################################################################
# Grace period: days between the 1st recorded cases in the US and #
# the 1st recorded cases in a given locale.                       #
###################################################################

# The date of the first reported cases in the US
first_cases_date <- jhu_US_data %>%
  filter(Cases != 0) %>%
  arrange(Date) %>%
  head(1) %>%
  pull(Date)


# Grace period for a given locale
get_grace_period <- function(location) {
  
  if (location %in% locales) {
    
    grace_period <- jhu_US_data %>%
      filter(Locale == location, Cases != 0) %>%
      mutate(Grace_period = as.numeric(Date - first_cases_date,
                                       units = "days")) %>%
      arrange(Date) %>%
      select(Locale, Grace_period) %>%
      head(1)
    
  }
  
  else if (location %in% states) {
    
    grace_period <- jhu_US_data %>%
      group_by(State) %>%
      filter(State == location, Cases != 0) %>%
      mutate(Grace_period = as.numeric(Date - first_cases_date,
                                       units = "days")) %>%
      arrange(Date) %>%
      select(State, Grace_period) %>%
      head(1)
    
  }
  
  else {
    
    stop("Error: get_grace_period() - Invalid location spec")
    
  }
  
  
  return(grace_period)
  
}
# Test
get_grace_period("California")

state_grace_periods <- map_dfr(states, get_grace_period)

# Plot Map of Grace Periods
plot_map(
  data = state_grace_periods,
  join_column = State,
  variable = Grace_period,
  title = "Grace Period by State",
  legend = "Days",
  caption = bquote("*Grace period is the number of days since the first" ~
                     " COVID-19 cases were recorded in the USA.")
)
