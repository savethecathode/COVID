#########################
## Preliminary analysis prior to K-means clustering
## Examine (available) parameters
## 1. Population of a given locale
## 2. The number of days between when cases are 1st reported in the US
##... and when cases are 1st reported in a given locale



###################################################
## Visualize variable for clustering: population ##
###################################################


## Look at 1st variable considered for clustering: population
## Just look at populations over one million
population_data_US %>%
  filter(Population > 100000) %>%
  ggplot(aes(Population)) +
  geom_histogram(binwidth = 1000000, fill = "blue", col = "black")





#####################################################################
## Generate variable for clustering:  days between 1st recorded    ##
##... cases in the US and the 1st recorded cases in a given locale ##
#####################################################################


## The date of the first reported cases in the US
first_cases_date <- jhu_US_data %>%
  filter(Cases != 0) %>%
  arrange(Date) %>%
  head(1) %>%
  pull(Date)


## Get the difference in days between when the 1st cases are recorded in the US and the Date field for each observation
# jhu_US_data %>%
#   mutate(Grace_period = as.numeric(Date - first_cases_date, units = "days")) #%>% filter(Locale == "New York City, New York") %>% view()


## Days between when the 1st cases are recorded in the US and when cases are 1st recorded in a Locale
get_grace_period <- function(x) {
  
  jhu_US_data %>%
    filter(Locale == x, Cases != 0) %>%
    mutate(Grace_period = as.numeric(Date - first_cases_date, units = "days")) %>%
    arrange(Date) %>%
    select(Locale, Grace_period) %>%
    head(1)
  
}


## Test get_grace_period() function on locale: NYC, NY
#get_grace_period("New York City, New York")


## Make vector of locales
locales <- unique(jhu_US_data$Locale)


## Get grace period for each locale
grace_periods <- map_dfr(locales, get_grace_period)



#####################################################
## Visualize variable for clustering: Grace Period ##
#####################################################

grace_periods %>%
  ggplot(aes(Grace_period)) +
  geom_histogram(binwidth = 5, fill = "blue", col = "black")


