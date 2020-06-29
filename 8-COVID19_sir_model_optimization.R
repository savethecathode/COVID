## Use informed guess for beta and gamma to optimize fit

## Starting values for SIR parameters beta and gamma to perform optimization
starting_optimization_parameters = unlist(rss_tests_results$parameters[which.min(rss_tests_results$values)])


time_init <- Sys.time()  # Start timer

optim_model <- optim(

  par = starting_optimization_parameters, # initial values for the parameters to be optimized over.
  location = the_location,  # parameter to get_SIR_fit_RSS() specifying city/location
  lag = the_lag,            # parameter to get_SIR_fit_RSS() specifying model lag 
  fn  = get_SIR_fit_RSS,    # Function to be minimized: RSS of SIR fit
  method = "L-BFGS-B" ,     # Gradient projection method using BFGS matrix to approx Hessian of objective function
  lower  = c(0, 0),         # lower boundary for beta and gamma
  upper  = c(1, 1)          # upper boundary for beta and gamma

)

time_elapsed <- Sys.time() - time_init # Run time
time_elapsed

## Check for convergence
optim_model$message

## Check convergence: 0 for L-BFGS-G method indicates successful completion
optim_model$convergence

min_rss <- optim_model$value

## Check fitted values
params_optim <- setNames(optim_model$par, c("beta", "gamma"))
beta_optim   <- params_optim[["beta"]]
gamma_optim  <- params_optim[["gamma"]]


## Compute R_0 = beta / gamma
R0  <- params_optim[["beta"]] / params_optim[["gamma"]]
R0


fit <- fit_SIR_model(params_optim, the_location, the_lag)
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
    
    title = "Infectious v. Date",
    subtitle = bquote(.(the_location) ~ ":"
                      ~beta  == .(round(beta_optim,  2))~","
                      ~gamma == .(round(gamma_optim, 2))~"," 
                      ~ R[0] == .(round(R0, 2)) ),
    caption = bquote("*Days since the first cases are detected before modeling starts: " ~ .(the_lag)),
    x = "Date",
    y = "Infectious",
    color = "Key:"

  ) +
  scale_color_manual(values=c("#0072B2", "#D55E00")) +
  theme(

    plot.title    = element_text(hjust =  0.5),
    plot.subtitle = element_text(hjust =  0.5),
    axis.title.y  = element_text(vjust =  3.0),
    axis.title.x  = element_text(vjust = -1.0),
    plot.caption  = element_text(vjust = -2.0, hjust = 1.0)

  )

