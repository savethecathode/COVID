# Purpose: Visualize RSS values for SIR model by beta and gamma pairs using heat
# map and contour lines

# 2D contour plot of RSS tests data
# To see available palettes use: hcl.pals()
# https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/

SIR_RSS_contour_plot <- function(location  = test_location,
                           beta_seq  = betas,
                           gamma_seq = gammas,
                           delay_days = default_delay,
                           start_date = NULL,
                            stop_date = NULL,
                           rss_values_matrix) {
 
  require(grDevices)
  
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
 
  #lvls <- seq(min(rss_values_matrix), max(rss_values_matrix), le = 50)
  lvls <- seq(min(rss_values_matrix), max(rss_values_matrix), le = 25)
 
  filled.contour(

    x = beta_seq,
    y = gamma_seq,
    z = rss_values_matrix,
    xlim = c(min(beta_seq), max(beta_seq)),
    ylim = c(min(gamma_seq), max(gamma_seq)),
    zlim = c(min(rss_values_matrix), max(rss_values_matrix)),
    col  = hcl.colors(
      n = 100,
      palette = "Viridis",
      alpha = 0.5,
      rev = FALSE,
      fixup = TRUE
    ),
    levels = lvls,
    main = bquote(.(location)),
    # sub  = list(bquote(
    #   "*Days since the first cases are detected before modeling starts: "
    #   ~ .(delay)), cex = 0.75),
    ylab = expression(paste("recovery rate  ", gamma, " (/day)")),
    xlab = "",
    xaxs = "i", yaxs = "i",
    key.title = title(
      main = bquote("RSS("~beta~","~gamma~")"), cex.main = 0.85),
    plot.axes = {

      contour(
        
        x = beta_seq,
        y = gamma_seq,
        z = rss_values_matrix,
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
 
  # Print min and max RSS values for beta and gamma pairs tested below graph
  #print(min(rss_values_matrix))
  #print(max(rss_values_matrix))
}


# Convert data from RSS tests into matrix format
# Row values correspond to beta index
# Column values correspond to gamma index
rss_scan_values_matrix <-
  matrix(unlist(Cook_IL_SIR_scan$values), sqrt(length(Cook_IL_SIR_scan$values)))

cp <- SIR_RSS_contour_plot(location  = test_location,
                           beta_seq  = betas,
                           gamma_seq = gammas,
                           delay_days = default_delay,
                           start_date = start_date_spec,
                           rss_values_matrix = rss_scan_values_matrix)
cp 


# Note:
# Error in plot.new() : figure margins too large
# Check:
#> par("mar")
#[1] 5.1 4.1 4.1 2.1

# Note: if you're getting the error msg:
# Error in .External.graphics(C_layout, num.rows, num.cols, mat, as.integer(num.figures),  : invalid graphics state
# Entering dev.off() in the Console should solve the issue.