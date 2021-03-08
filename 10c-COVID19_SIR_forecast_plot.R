# Purpose: plot forecast with 95% confidence intervals 
# Note: how to use geom_ribbon with the ggplot legend is VERY difficult, 
# VERY non-intuitive, and VERY unpredictable.  Maybe there is another way...
# but so far simpler approaches were unsuccessful.


# Plot function for raw + forecast + confidence intervals
plot_SIR_forecastCI <- function(location,
                                data, x, y, code,
                                lower, upper,
                                beta, gamma) {
  
  R0 <- beta/gamma
  
  ggplot(data, aes({{x}}, {{y}}, color = {{code}})) +
    geom_line() +
    geom_ribbon(
      aes(ymin = {{lower}}, ymax = {{upper}}, fill = "95% C.I.s"),
      #fill = "grey",
      alpha = 0.50,
      color = "NA"  # Only show fill, suppress bounding curve lines
    ) +
    scale_color_manual(values = c("#000000", "#D55E00")) +
    scale_fill_manual(values = c("#999999"), name = "Confidence Intervals") +
    theme_light() +
    labs(
      title = "Infectious (SIR model) v. Date",
      subtitle = bquote(.(location) ~ ":"
                        ~beta  == .(round(beta, 2)) ~ ","
                        ~gamma == .(round(gamma, 2)) ~ ","
                        ~R[0]  == .(round(R0, 2))),
      caption = bquote(
        "*Days since the first cases are detected before modeling starts: "
        ~ .(default_delay)),
      x = "Date",
      y = "Number of Infectious Individuals",
      color = "Key:"
    ) +
    theme(
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 1.0)
    )

}

plot_SIR_forecastCI(location = test_location,
                    data  = Cook_IL_SIR_forecast_tidy_CIs,
                    x = dates,
                    y = infectious,
                    code  = code,
                    lower = lower,
                    upper = upper,
                    beta  = Cook_IL_SIR_optim$beta,
                    gamma = Cook_IL_SIR_optim$gamma)
