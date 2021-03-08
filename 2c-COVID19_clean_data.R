###############################################################
## Omit places that do not have any cases from consideration ##
###############################################################


# Find Locales that never record any cases
# Side Note: entries with bad Locale/Combined_Key:
# "Weber,Utah,US", "District of Columbia,District of Columbia,US"
locales_with_zero_cases <- jhu_US_data %>%
  group_by(Locale) %>%
  summarize(sum_cases = sum(Cases), .groups = 'drop') %>%
  filter(sum_cases == 0) %>%
  pull(Locale)


# Omit locales with no cases
jhu_US_data <- jhu_US_data %>%
  filter(!Locale %in% locales_with_zero_cases)

population_data_US <- population_data_US %>%
  filter(!Locale %in% locales_with_zero_cases)


##############################################
## Omit places that have no population data ##
##############################################

# Note: some Locale listings have Population equal to zero
locales_without_population <-
  population_data_US$Locale[which(population_data_US$Population == 0)]


# Remove entries for which the population is equal to zero from population data
population_data_US <- population_data_US %>%
  filter(Population > 0)

jhu_US_data <- jhu_US_data %>%
  filter(!Locale %in% locales_without_population)

