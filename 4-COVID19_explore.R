## Exploratory data analysis

## List of all states, commonwealths, and territories
states <- levels(factor(jhu_US_data$State))

## List of all cities/towns
locales <- levels(factor(jhu_US_data$Locale))





##################################################
## Rank states according to the number of cases ##
##################################################

#jhu_data %>% group_by(State) %>% summarize(cases = max(Cases)) %>% arrange(desc(cases))





####################################################
## Find the cities with most cases for each state ##
####################################################

## Function to get the city with the most cases for a given state
## pass function the name of a state
## result does not return date...Fix!

city_with_most_cases_by_state <- function(x) {

  jhu_US_data %>%
    filter(State == x) %>%
    group_by(Locale) %>%
    summarize(no_cases = max(Cases)) %>%
    arrange(desc(no_cases)) %>%
    head(1)

}

## Apply the city_with_most_cases_by_state function to every state
## row-bind map output as a data frame, and arrange in descending order by the number of cases

cities_with_most_cases_by_state <- map_dfr(states, city_with_most_cases_by_state) %>% arrange(desc(no_cases))
cities_with_most_cases_by_state


####################################################
## Get the maximum number of cases in each Locale ##
####################################################

jhu_US_data %>%
  group_by(Locale) %>%
  summarize(no_cases = max(Cases)) %>%
  arrange(desc(no_cases))



##############################################################
# plot time series of cases for each locale in a given state #
##############################################################

# jhu_US_data %>%
#   filter(State == "New York") %>%
#   group_by(Locale) %>%
#   ggplot(aes(Date, Cases, color = Locale)) +
#   geom_line()




################################################################################
# Find the date of first documented cases, and the number of cases in a Locale #
################################################################################

# locale_first_cases_date <- function(x) {
# 
#   jhu_data %>%
#     filter(Locale == x, Cases != 0) %>%
#     arrange(Date) %>%
#     head(1)
# 
# }
# 
# 
# ## Arrange by date of first cases
# locales_first_cases <- map_dfr(locales, locale_first_cases_date) %>%
#   arrange(Date) %>%
#   select(Locale, Date)
# 
# 
# ## Rename column
# names(locales_first_cases)[names(locales_first_cases) == "Date"] <- "Locale_first_cases"
# 
# 
# ## Join data to table
# jhu_data <- left_join(jhu_data, locales_first_cases, by = "Locale")



###############################################################################
# Find the date of first documented cases, and the number of cases in a State #
###############################################################################

# state_first_cases_date <- function(x) {
#   
#   jhu_data %>%
#     filter(State == x, Cases != 0) %>%
#     arrange(Date) %>%
#     head(1)
#   
# }
# 
# 
# ## Arrange by date of first cases
# states_first_cases <- map_dfr(states, state_first_cases_date) %>%
#   arrange(Date) %>%
#   select(State, Date)
# 
# 
# ## Rename column
# names(states_first_cases)[names(states_first_cases) == "Date"] <- "State_first_cases"
# 
# 
# ## Join data to table
# jhu_data <- left_join(jhu_data, states_first_cases, by = "State")


