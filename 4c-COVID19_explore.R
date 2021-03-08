# Purpose: exploratory data analysis

# Find total number of cases for each state
state_COVID_totals <- map_dfr(states, get_COVID_totals)

# The 10 states with highest total number of cases
tibble(state_COVID_totals) %>% arrange(desc(as.numeric(Total_cases)))

# The 10 states with lowest total number of cases
tibble(state_COVID_totals) %>% arrange(as.numeric(Total_cases))

# The 10 states with highest percentage of cases
tibble(state_COVID_totals) %>% arrange(desc(as.numeric(Percent_cases)))

# The 10 states with lowest percentage of cases
tibble(state_COVID_totals) %>% arrange(as.numeric(Percent_cases))


#######################################################################
# Rank states according to the number of cases within a single locale #
#######################################################################

jhu_US_data %>%
  group_by(State) %>%
  summarize(cases = max(Cases)) %>%
  arrange(desc(cases)) %>%
  head(10) %>%
  kable(col.names = c("State", "# of Cases"))


##################################################
# Find the locale with most cases for each state #
##################################################

# Get locale with most cases for a given state.
locale_with_most_cases_by_state <- function(x) {

  jhu_US_data %>%
    filter(State == x) %>%
    group_by(Locale) %>%
    summarize(no_cases = max(Cases), .groups = "drop") %>%
    arrange(desc(no_cases)) %>%
    head(1)
  
}

# Collate data and view the top 10 locales based on the number of cases.
map_dfr(states, locale_with_most_cases_by_state) %>%
  arrange(desc(no_cases)) %>% 
  rename("No. Cases" = no_cases) %>%
  head(10) %>%
  kable()
 

###################################################
# Find the locale with most deaths for each state #
###################################################

# Get locale with most deaths for a given state.
locale_with_most_deaths_by_state <- function(x) {

  jhu_US_data %>%
    filter(State == x) %>%
    group_by(Locale) %>%
    summarize(no_deaths = max(Deaths), .groups = "drop") %>%
    arrange(desc(no_deaths)) %>%
    head(1)
 
}

# Collate data and view the top 10 locales based on the number of deaths.
map_dfr(states, locale_with_most_deaths_by_state) %>%
  arrange(desc(no_deaths)) %>% 
  rename("No. Deaths" = no_deaths) %>%
  head(10) %>%
  kable()

