# Purpose: add confidence intervals to the forecast

# get confidence intervals for forecast
Cook_IL_SIR_forecast_CIs <- get_SIR_CIs(data  = Cook_IL_SIR_forecast$I,
                                        sigma = Cook_IL_SIR_ofit_mle_sigma_hat)

# put raw infectious data and forecast in tidy format separately and row_bind
Cook_IL_SIR_forecast_tidy_CIs <- bind_rows(
  Cook_IL_SIR_raw %>%
    rename(raw = raw_I) %>%
    select(dates, raw),
  
    cbind(Cook_IL_SIR_forecast, Cook_IL_SIR_forecast_CIs) %>%
    select(dates, I, lower, upper)
  ) %>%
  rename(forecast = I) %>%
  pivot_longer(
    cols = -c(dates, lower, upper),
    names_to  = "code",
    values_to = "infectious"
  )
