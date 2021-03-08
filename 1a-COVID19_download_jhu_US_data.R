# Download data set(s) for the United States from:
# Johns Hopkins University Center for Systems Science and Engineering (CSSE)


# CSV file of time series of confirmed cases in the United States
url_jhu_ts_confirmed_US <- paste(
  "https://raw.githubusercontent.com/CSSEGISandData/",
  "COVID-19/master/csse_covid_19_data/",
  "csse_covid_19_time_series/",
  "time_series_covid19_confirmed_US.csv", sep="")

confirmed_US <- read_csv(url(url_jhu_ts_confirmed_US))



# CSV file of time series of COVID-19 deaths in the United States
url_jhu_ts_deaths_US <- paste(
  "https://raw.githubusercontent.com/CSSEGISandData/",
  "COVID-19/master/csse_covid_19_data/",
  "csse_covid_19_time_series/",
  "time_series_covid19_deaths_US.csv", sep="")

deaths_US <- read_csv(url(url_jhu_ts_deaths_US))



# No time series data for recovery cases for the US
# Data on recoveries in the US is stored on a day-by-day bases
# (in individual CSV files) by state but not for every state and
# is not broken down further to the city/town level.
# NOTE: estimate the number of recovery cases based on the
# recovery time, which is ~2 weeks (WHO).
# Here we are assuming the infectious period is equal to the 
# recovery time.



# Store data
# make directory to store data if it does not already exist
mainDir <- "~/COVID19/COVID19SIR/"
subDir  <- "COVID19_jhu_ts_data/"

ifelse(!dir.exists(file.path(mainDir, subDir)),
       dir.create(file.path(mainDir, subDir)), FALSE)
# dir.create() does not crash if the directory already exists, it just
# prints out a warning.
# To suppress warnings, set option: showWarnings = FALSE
# returns FALSE if the directory already exists or is uncreatable, and TRUE if
# it didn't exist but was successfully created.
# note: it's not good practice to use ifelse() for non-vectorised branching


# Save raw US confirmed cases data
file_name <- paste0("jhu_ts_confirmed_US_raw_", Sys.Date(), ".csv")
write_csv(confirmed_US,
          file = paste0(mainDir, subDir, file_name),
          append = FALSE,
          col_names = TRUE)
# write_csv(confirmed_US,
#           path = paste0(mainDir, subDir, file_name),
#           append = FALSE,
#           col_names = TRUE)
# Warning message:
#   The `path` argument of `write_csv()` is deprecated as of readr 1.4.0.
# Please use the `file` argument instead.


# Save raw US death cases data
file_name <- paste0("jhu_ts_deaths_US_raw_", Sys.Date(), ".csv")
write_csv(deaths_US,
          file = paste0(mainDir, subDir, file_name),
          append = FALSE,
          col_names = TRUE)
# write_csv(deaths_US,
#           path = paste0(mainDir, subDir, file_name),
#           append = FALSE,
#           col_names = TRUE)

