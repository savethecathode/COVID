#########################################
## Visualize the Model Evaluation Data ##
#########################################


## Convert data from RSS tests into matrix format
## Row values correspond to beta index
## Column values correspond to gamma index

rss_tests_values <- matrix(unlist(rss_tests_results$values), sqrt(length(rss_tests_results$values)))



## 2D contour plot of RSS tests data
## To see available palettes use: hcl.pals()
## https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/

visualize_data <- function(beta_seq, gamma_seq, rss_values_matrix) {

  require(grDevices)
    
  lvls <- seq(min(rss_tests_values), max(rss_tests_values), le = 50)
  
  filled.contour(
    
    x = beta_seq,
    y = gamma_seq,
    z = rss_values_matrix,
    xlim = c(min(betas), max(betas)),
    ylim = c(min(gammas), max(gammas)),
    zlim = c(min(rss_values_matrix), max(rss_values_matrix)),
    col  = hcl.colors(
      n = 100,
      palette = "Viridis",
      alpha = 0.5,
      rev = FALSE,
      fixup = TRUE
    ),
    levels = lvls,
    main = bquote(.(the_location)),
    sub  = list(bquote("*Days since the first cases are detected before modeling starts: " ~ .(the_lag)), cex = 0.75),
    ylab = expression(paste("recovery rate  ", gamma, " (/day)")),
    xlab = "",
    xaxs = "i", yaxs = "i",
    key.title = title(main = bquote("RSS("~beta~","~gamma~")"), cex.main = 0.85),
    plot.axes = {
      
      contour(
        
        x = betas,
        y = gammas,
        z = rss_tests_values,
        add  = TRUE,
        axes = FALSE,
        drawlabels = FALSE
      
        );

      axis(1); axis(2) # must add back axes explicitly

    }

  )
  
  # Adjust x-axis label only: shift up from subtitle
  mtext(
    
    text = expression(paste("infection rate ", beta,  " (/day)")),
    side = 1, # bottom x-axis
    line = 2.5
    
  )
  
  ## Show min and max RSS values for beta and gamma pairs tested
  print(min(rss_values_matrix))
  print(max(rss_values_matrix))
  
}


## Visualize RSS values for beta and gamma pairs using head map and contour lines
visualize_data(betas, gammas, rss_tests_values)


