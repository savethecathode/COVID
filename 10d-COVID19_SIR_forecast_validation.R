# Purpose: validate forecast with updated information.

# Get dates for beginning of modeling and end of forecasting.
valid_start_date  <- Cook_IL_SIR_raw$dates[1]
valid_stop_date <- Cook_IL_SIR_forecast$dates[length(Cook_IL_SIR_forecast$dates)]

# Get raw Infectious SIR data from start modeling through forecast
Cook_IL_SIR_resume <- get_SIR_data(location = test_location,
                                   start_date = valid_start_date,
                                   stop_date  = valid_stop_date) %>%
  rename(raw = raw_I) %>%
  select(dates, raw)
 
# Get forecast and error 
Cook_IL_SIR_forecast_error <- cbind(Cook_IL_SIR_forecast,
                                    Cook_IL_SIR_forecast_CIs) %>%
  select(dates, I, lower, upper) %>%
  rename(forecast = I)

# Join by date
Cook_IL_SIR_validation <- full_join(x = Cook_IL_SIR_resume,
                                    y = Cook_IL_SIR_forecast_error,
                                    by = "dates")
# Tidy
Cook_IL_SIR_validation_tidy <- pivot_longer(data = Cook_IL_SIR_validation,
                                            cols = -c(dates, lower, upper),
                                            names_to  = "code",
                                            values_to = "infectious")
# Plot
plot_SIR_forecastCI(location = test_location,
                    data  = Cook_IL_SIR_validation_tidy,
                    x = dates,
                    y = infectious,
                    code  = code,
                    lower = lower,
                    upper = upper,
                    beta  = Cook_IL_SIR_optim$beta,
                    gamma = Cook_IL_SIR_optim$gamma)

# Compute RSS
RSS(prediction = Cook_IL_SIR_forecast$I,
    raw_data = Cook_IL_SIR_resume$raw[
      which(Cook_IL_SIR_resume$dates == Cook_IL_SIR_forecast$dates[1]):
      which(Cook_IL_SIR_resume$dates == tail(Cook_IL_SIR_forecast$dates, n=1))
    ])
