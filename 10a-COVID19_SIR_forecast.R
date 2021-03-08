# Purpose: Predict the number of future COVID19 cases using the SIR model

# ***Note: SIR model shows some ability to make predictions in the short term,
# that is the model can be used to fit data in a short window of time (2 weeks)
# it is therefore reasonable to use the paramter estimations from this fitted
# model to make short term predictions into the future (1 week) ***


# Compute forecast
forecast_SIR <- function(start_date,
                         day_forecast = 7,
                         N, beta, gamma,
                         S, I, R) {
 
  # Instantiate dataframe
  forecast = tibble("steps" = 0,
                    "dates" = start_date,
                    "S" = S, "I" = I, "R" = R)
 
  for (steps in 1:day_forecast) {

    # stay DRY
    dS  <- - beta * S * I / N
    gI  <-  gamma * I

    # Increment compartments
    S_t <- S + dS
    I_t <- I - dS - gI
    R_t <- R + gI 
 
    # Update values
    S <- S_t
    I <- I_t
    R <- R_t

    dates <- ymd(start_date) + days(steps)
    forecast <- bind_rows(forecast,
                          tibble(steps,
                                 dates,
                                 S, I, R))
  }
 
  return(forecast) 
}


Cook_IL_SIR_forecast <- forecast_SIR(
  start_date   = tail(Cook_IL_SIR_raw$date, n = 1),
  day_forecast = 7,
  N = get_pop("Cook, Illinois"),
  beta  = Cook_IL_SIR_optim$beta,
  gamma = Cook_IL_SIR_optim$gamma,
  S = tail(Cook_IL_SIR_raw$raw_S, n = 1),
  I = tail(Cook_IL_SIR_raw$raw_I, n = 1),
  R = tail(Cook_IL_SIR_raw$raw_R, n = 1)
)

Cook_IL_SIR_forecast


Cook_IL_SIR_tidy_forecast <- Cook_IL_SIR_forecast %>%
  select(dates, I) %>%
  pivot_longer(
    cols = -c(dates),
    names_to  = "code",
    values_to = "infectious") %>%
  mutate(code = "forecast")


plot_SIR_forecast <- function(location,
                              data, x, y, key,
                              delay_days = default_delay,
                              beta, gamma) {
 
  R0 <- beta/gamma
  
  ggplot(data, aes({{x}}, {{y}}, color = {{key}})) +
    geom_line() +
    theme_light() +
    labs(
      title = "Infectious (SIR model) v. Date",
      subtitle = bquote(.(location) ~ ":"
                        ~beta  == .(round(beta,  2))~","
                        ~gamma == .(round(gamma, 2))~"," 
                        ~ R[0] == .(round(R0, 2)) ),
      caption = bquote(
        "*Days since the first cases are detected before modeling starts: "
        ~ .(delay_days)),
      x = "Date",
      y = "Infectious",
      color = "Key:"
    ) +
    scale_color_manual(values=c("#D55E00", "#0072B2", "#000000")) +
    theme(
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 0.5)
    )
}

# # Append raw + fit data to SIR forecast
# plot_data <- bind_rows(Cook_IL_SIR_tidy, Cook_IL_SIR_tidy_forecast)
# 
# # Reorder factor levels in code column for ggplot2 key
# plot_data$code <- factor(plot_data$code, levels = c("raw", "fit", "forecast"))
# 
# plot_SIR_forecast(location = test_location,
#                   data = plot_data,
#                   x = dates,
#                   y = infectious,
#                   key  = code,
#                   delay_days  = default_delay,
#                   beta  = Cook_IL_SIR_optim$beta,
#                   gamma = Cook_IL_SIR_optim$gamma)
