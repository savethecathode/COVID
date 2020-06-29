## Download Global data set(s) from:
## John's Hopkins University Center for Systems Science and Engineering (CSSE)

## CSV file of time series of confirmed cases globally
url_jhu_ts_confirmed_global <- paste("https://raw.githubusercontent.com/CSSEGISandData/",
                                     "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/",
                                     "time_series_covid19_confirmed_global.csv", sep="")

confirmed_global <- read_csv(url(url_jhu_ts_confirmed_global))



## CSV file of time series of COVID-19 deaths globally
url_jhu_ts_deaths_global <- paste("https://raw.githubusercontent.com/CSSEGISandData/",
                                  "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/",
                                  "time_series_covid19_deaths_global.csv", sep="")

deaths_global <- read_csv(url(url_jhu_ts_deaths_global))



## CSV file of time series of COVID-19 recoveries globally
url_jhu_ts_recovered_global <- paste("https://raw.githubusercontent.com/CSSEGISandData/",
                                     "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/",
                                     "time_series_covid19_recovered_global.csv", sep="")

recovered_global <- read_csv(url(url_jhu_ts_recovered_global))





# Store data
# make directory to store data if it does not already exist
mainDir <- "~/COVID19/COVID19modeling/"
subDir  <- "COVID19_jhu_ts_data/"

ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
# dir.create() does not crash if the directory already exists, it just prints out a warning.
# To supress warnings, set uption: showWarnings = FALSE
# returns FALSE if the directory already exists or is uncreatable, and TRUE if it didn't exist but was succesfully created.
# note: it's not good practice to use ifelse() for non-vectorised branching



## Save raw global confirmed cases data
file_name <- paste0("jhu_ts_confirmed_global_raw_", Sys.Date(), ".csv")
write_csv(confirmed_global,
          path = paste0(mainDir, subDir, file_name),
          append = FALSE,
          col_names = TRUE)


## Save raw global deaths data
file_name <- paste0("jhu_ts_deaths_global_raw_", Sys.Date(), ".csv")
write_csv(deaths_global,
          path = paste0(mainDir, subDir, file_name),
          append = FALSE,
          col_names = TRUE)


## Save raw global recovered cases data
file_name <- paste0("jhu_ts_recovered_global_raw_", Sys.Date(), ".csv")
write_csv(recovered_global,
          path = paste0(mainDir, subDir, file_name),
          append = FALSE,
          col_names = TRUE)


