# Plot COVID data and SIR fit

Cook_IL_SIR_raw <- get_SIR_data(location = test_location,
                                delay_days = default_delay,
                                start_date = start_date_spec,
                                 stop_date = stop_date_spec)

# Starting values for SIR parameters beta and gamma to perform optimization
starting_optimization_parameters <-
  unlist(Cook_IL_SIR_scan$parameters[which.min(Cook_IL_SIR_scan$values)])

Cook_IL_SIR_fit <- fit_SIR_model(params = starting_optimization_parameters,
                                 location   = test_location,
                                 delay_days = default_delay,
                                 start_date = start_date_spec,
                                  stop_date =  stop_date_spec)


# Tidy infectious data for raw and fit SIR 
# Supply join_column argument as a string in quotes
# THINK/NOTE: the simplest thing to join by is an index, which always starts at
# 1, and the intuitive interpretation should be the correct choice.
tidy_SIR_I <- function(raw, fit, join_column = "index") {
 
  raw_and_fit <- inner_join(raw, fit, by = {{join_column}})
 
  tidy_data <- raw_and_fit %>%
    select(dates, fit_I, raw_I) %>%
    rename(

      raw = raw_I,
      fit = fit_I

    ) %>%
    pivot_longer(
 
      cols      = -c("dates"),
      names_to  = "code",
      values_to = "infectious"
 
    )

  return(tidy_data)
}

Cook_IL_SIR_tidy <- tidy_SIR_I(raw = Cook_IL_SIR_raw,
                               fit = Cook_IL_SIR_fit,
                               join_column = "index")


#####################
## Plot data + fit ##
#####################

plot_SIR_fit <- function(location,
                         data, x, y, key,
                         delay_days = default_delay,
                         start_date = NULL,
                         beta, gamma) {
  
  # If start_date is set and is not NULL
  if (!missing(start_date) & !is.null(start_date)) {
    
    # Check date validity
    indices <- get_date_indices(location = location, start_date = start_date)
    
    delay <- indices["start_index"]
    
  }
  
  # If start_date is not given set start_index to 1
  else if (missing(start_date) | is.null(start_date)) {
    
    delay <- 1

  }
 
  
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
        ~ .(delay)),
      x = "Date",
      y = "Infectious",
      color = "Key:"
 
    ) +
    scale_color_manual(values=c("#0072B2", "#D55E00")) +
    #scale_color_manual(values=c(cbbPalette[1], cbbPalette[2])) +
    theme(
 
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 0.5)

    )
  
}

# Reorder factors in key for legend construction
Cook_IL_SIR_tidy$code <- factor(Cook_IL_SIR_tidy$code, levels = c("fit", "raw"))

plot_SIR_fit(location = test_location,
             data = Cook_IL_SIR_tidy,
             x = dates,
             y = infectious,
             key = code,
             delay_days = default_delay,
             start_date = start_date_spec,
             beta  = as.numeric(starting_optimization_parameters[1]),
             gamma = as.numeric(starting_optimization_parameters[2]))

