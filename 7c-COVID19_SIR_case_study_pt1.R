# Purpose: case study of "King, Washington", the first locale to report cases,
# during the 1st phase of its outbreak.
# Obs: a minimum number of infectious people is required prior to SIR fitting.

case_study_locale <- "King, Washington"

# Date 1st cases are recorded
case_study_start_date <- get_init_cases_date(case_study_locale)

# Plot of infectious-SIR data
plot_SIR_I_data(case_study_locale)

# Obs. Peak of 1st wave for "King, WA" is around 2 weeks into April 2020.
case_study_stop_date <- 20200414

# Plot of 1st outbreak
plot_SIR_I_data(case_study_locale, stop_date = case_study_stop_date)

# Scan of beta and gamma parameters for SIR model fit
case_study_SIR_scan <- scan_SIR_RSS(location   = case_study_locale,
                                    start_date = case_study_start_date,
                                    stop_date  = case_study_stop_date)

# Fit data
case_study_SIR_fit <- fit_SIR_model(
  params = unlist(
    case_study_SIR_scan$parameters[which.min(case_study_SIR_scan$values)]),
  location   = case_study_locale,
  start_date = case_study_start_date,
  stop_date  = case_study_stop_date)

# Raw data
case_study_SIR_raw <- get_SIR_data(location   = case_study_locale,
                                   start_date = case_study_start_date,
                                   stop_date  = case_study_stop_date)

# Raw and fit in tidy format
case_study_SIR_tidy <- tidy_SIR_I(raw = case_study_SIR_raw,
                                  fit = case_study_SIR_fit)

# Reorder factors in key for legend construction
case_study_SIR_tidy$code <- factor(case_study_SIR_tidy$code,
                                   levels = c("fit", "raw"))

# Starting values for SIR parameters beta and gamma to perform optimization
case_study_phase1_params <-
  unlist(case_study_SIR_scan$parameters[which.min(case_study_SIR_scan$values)])

# Plot raw and fit for 1st stage of the pandemic
plot_SIR_fit(location = case_study_locale,
             data = case_study_SIR_tidy,
             x = dates,
             y = infectious,
             key = code,
             start_date = case_study_start_date,
             beta  = as.numeric(case_study_phase1_params[1]),
             gamma = as.numeric(case_study_phase1_params[2]))
