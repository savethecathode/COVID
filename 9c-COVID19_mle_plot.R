###################
# Plot data + fit #
###################

plot_SIR_fitCI <- function(location,
                           data, x, y, code,
                           lower, upper,
                           beta, gamma) {
  
  R0 <- beta/gamma
  
  ggplot(data, aes({{x}}, {{y}}, color = {{code}})) +
    geom_line() +
    geom_ribbon(
      aes(ymin = {{lower}}, ymax = {{upper}}, fill = "band"),
      fill = "grey",
      alpha = 0.50,
      color = "NA"  # Only show fill, suppress bounding curve lines
      #show.legend = TRUE
    ) + 
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
    scale_color_manual(values = c("#0072B2", "#D55E00")) +
    theme(
      plot.title    = element_text(hjust =  0.5),
      plot.subtitle = element_text(hjust =  0.5),
      axis.title.y  = element_text(vjust =  3.0),
      axis.title.x  = element_text(vjust = -1.0),
      plot.caption  = element_text(vjust = -2.0, hjust = 0.5)
    )
}

# OBS: if you see the Warning message:
# In isFALSE(simplify) : reached elapsed time limit
# running gc() in the console solves this.
 

plot_SIR_fitCI(location = test_location,
               data  = Cook_IL_SIR_tidyCI,
               x = dates,
               y = infectious,
               code  = code,
               lower = lower_CI,
               upper = upper_CI,
               beta  = Cook_IL_SIR_optim$beta,
               gamma = Cook_IL_SIR_optim$gamma)