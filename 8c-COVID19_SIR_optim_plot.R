# Purpose: plot infectious SIR fit data using optimized parameters
# start_date overrides lag to show the number of days since the 1st cases where
# observed for the specified location before applying the SIR model.

plot_SIR_fit(location = test_location,
             data = Cook_IL_SIR_tidy_ofit,
             x = dates,
             y = infectious,
             key = code,
             delay_days = default_delay,
             start_date = start_date_spec,
             beta  = Cook_IL_SIR_optim$beta,
             gamma = Cook_IL_SIR_optim$gamma)