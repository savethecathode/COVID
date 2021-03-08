# Web scraping using rvest package
# Scrape wikipedia for lockdown dates in response to the COVID-19 pandemic.
# Wikipedia page: U.S. state and local government response to the COVID-19 pandemic
# url: https://en.wikipedia.org/wiki/U.S._state_and_local_government_response_to_the_COVID-19_pandemic
# date accessed: 5/24/2020


# Download file
url <- "https://en.wikipedia.org/wiki/U.S._state_and_local_government_response_to_the_COVID-19_pandemic"
us_covid_response_webpage <- read_html(url)


# The 1st table is for the state-level regulations
us_covid_response_by_state <- us_covid_response_webpage %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)


# Extract columns for the state and the date of the state of emergency declaration
# Omit the first column, which corresponds to the state flag
# Omit the first column, which repeats to the column headers
states_emergency_declaration <- us_covid_response_by_state[-1,2:3]


#...note: R doesn't re-number the rows, so the first row is numbered 2
#...so manually re-number rows
row.names(states_emergency_declaration) <- 1:nrow(states_emergency_declaration)


# Rename columns: "State/territory", "State of emergency declared"
# The SED date is in the format of: <string>(month) <int>(day)
names(states_emergency_declaration) <- c("State", "SED_date")


# Convert SED_date to date-mode with lubridate
# NOTE: lubridate::parse_date_time(x, orders = c("md")) automatically supplies year even if no year is supplied by the input, which could cause problems in the future.
# lubridate::parse_date_time2() does not automatically append a year to the output even if it is missing from the input.
# ***ALSO***NOTE: the wikipedia data is in US-time zone
#...BUT the CSSE data is in UTC time-zone
#...NEED TO fix time zone...!!!
# The function as_date() is used to convert POSIXct to Date format.
states_emergency_declaration <- states_emergency_declaration %>%
  mutate(SED_date = as_date(parse_date_time2(SED_date, orders = c("md"))))


# Manually include the year of the state of emergency declaration: 2020
# NOTE: Washington state issued its emergency declaration on February 29 (leap day of leap year 2020)
year(states_emergency_declaration$SED_date) <- 2020


# Show results
states_emergency_declaration


# Combine COVID incidence data with lockdown dates according to state.
jhu__US_data <- left_join(jhu_US_data, states_emergency_declaration, by="State")
str(jhu_US_data)