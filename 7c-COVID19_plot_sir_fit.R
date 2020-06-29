## Use informed guess for beta and gamma to optimize fit

## Starting values for SIR parameters beta and gamma to perform optimization
starting_optimization_parameters = unlist(rss_tests_results$parameters[which.min(rss_tests_results$values)])

beta  <- as.numeric(starting_optimization_parameters[1])
gamma <- as.numeric(starting_optimization_parameters[2])
R0 <- beta / gamma


fit <- fit_SIR_model(starting_optimization_parameters, the_location, the_lag)
raw <- get_raw_data(the_location, the_lag) %>%
  mutate(time = seq(1, length(date)))

## Combine SIR fit data, raw JHU cases data, and specified range of dates.
fit_and_raw_data <- full_join(fit, raw, by = "time", keep = FALSE)


## Rename variables
fit_and_raw_data <- fit_and_raw_data %>%
  rename(
    
    fit_S = S,
    fit_I = I,
    fit_R = R,
    raw_I = infected,
    raw_R = recovered
    
  )


## Convert wide data to tidy data
fig_data <- fit_and_raw_data %>%
  select(date, fit_I, raw_I) %>%
  rename(
    
    raw = raw_I,
    fit = fit_I
    
  ) %>%
  pivot_longer(
 
    cols      = -c("date"),
    names_to  = "data",
    values_to = "infected"
 
  )


#####################
## Plot data + fit ##
#####################

fig_data %>%
  ggplot(aes(x = date, y = infected, color = data)) +
  geom_line() +
  theme_light() +
  labs(
    
    title = "Infectious (\"I\" of SIR Model) v. Date",
    subtitle = bquote(.(the_location) ~ ":"
                      ~beta  == .(round(beta, 2)) ~ ","
                      ~gamma == .(round(gamma, 2)) ~ ","
                      ~R[0]  == .(round(R0, 2))),
    caption = bquote("*Days since the first cases are detected before modeling starts: " ~ .(the_lag)),
    x = "Date",
    y = "Number of Infectious Individuals",
    color = "Key:"
    
  ) +
  scale_color_manual(values=c("#0072B2", "#D55E00")) +
  theme(
    
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y  = element_text(vjust =  3.0),
    axis.title.x  = element_text(vjust = -1.0),
    plot.caption  = element_text(vjust = -2.0, hjust = 1.0)
    
  )

