## Use to get CIs
#+ geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)


# Convert wide data to tidy data
tidy_SIR_I_dataCI <- function(raw, x1, fit, x2, join_col, lower, upper) {
 
  raw_and_fit <- inner_join(raw, fit, by = {{join_col}})
  
  raw_and_fit <- cbind(raw_and_fit, lower, upper)

  fig_data <- raw_and_fit %>%
    select(dates, {{x1}}, {{x2}}, lower, upper) %>%
    rename(
      raw = {{x1}},
      fit = {{x2}},
      lower_CI = lower,
      upper_CI = upper 
    ) %>%
    pivot_longer(              # THINK:  Only tidy what you need to color-code
      cols = -c(
        "dates",
        "lower_CI",
        "upper_CI"
      ),                       # OBS: lower_CI and upper_CI excluded from pivot
      names_to  = "code",
      values_to = "infectious"
    )
}

Cook_IL_SIR_tidyCI <- tidy_SIR_I_dataCI(raw = Cook_IL_SIR_raw,  x1 = raw_I,
                                        fit = Cook_IL_SIR_ofit, x2 = fit_I,
                                        join_col = "index",
                                        lower = Cook_IL_SIR_ofit_CIs$lower,
                                        upper = Cook_IL_SIR_ofit_CIs$upper)