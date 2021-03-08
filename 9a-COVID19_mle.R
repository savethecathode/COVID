# Purpose: maximum likelihood estimation (MLE) obtain confidence intervals
# Note: MLE can be used as an alternative approach to optim( fn = RSS) to
# optimize SIR parameters beta and gamma
# Adapted from Marc Choisy's RStudio Blog post: https://rpubs.com/choisy/sir
# Notes:  The essence of MLE
# The question is, given an observation what is the probability that the true
# mean is given by the fit.
# Assume the residuals are Gaussian distributed and compute sigma.
# Other distributions are valid.  Next consider the Poisson distribution.
# Use the resulting distribution to plot confidence intervals for the fit-curve.

# http://www.ms.uky.edu/~mai/sta321/MLEexample.pdf
# James Holland Jones' blog post:
# http://web.stanford.edu/class/earthsys213/notes/fit.html
# http://web.stanford.edu/class/earthsys214/notes/fit.html


# Load bbmle package: mle2()
#library(bbmle)


# Define a likelihood function.
# Essentially what we are asking is what is the probability that the actual mean
# corresponds to the fitted value given the actual observation by the raw data.
# Assume Guassian distribution of the errors.
# Note: RSS is indifferent wrt order, mLL inherently is NOT!
# Note: skip initial values in fit...(?)
mLL <- function(sigma = 1, observations, predictions) {
  
  # Return minus log-likelihood
  return(
    -sum(
      dnorm(                 # Normal density
        x = observations,
        mean = predictions,
        sd = sigma,
        log = TRUE           # Probability (p) given as log(p)
      )
    )
  )
}



########################################################
# Use MLE to obtain sigma of distribution of residuals #
########################################################

mle_mll <- function(raw, fit) {
  
  estimates <- mle2(
    minuslogl = mLL,
    start = list(sigma = 1),  # Named list of parameters to minuslogl function
    method = "L-BFGS-B",
    data = list(observations = raw,
                predictions  = fit)
  )

  return(estimates)
}


Cook_IL_SIR_ofit_mle <- mle_mll(raw = Cook_IL_SIR_raw$raw_I,
                                fit = Cook_IL_SIR_ofit$fit_I)

# Parameter estimates
Cook_IL_SIR_ofit_mle_sigma_hat <- coef(Cook_IL_SIR_ofit_mle)["sigma"]
Cook_IL_SIR_ofit_mle_sigma_hat


#################################################
# Get confidence intervals for the fitted curve #
#################################################

get_SIR_CIs <- function(data, sigma) {
  
  # confidence level
  cl <- 0.95
  cl <- (1 - cl)/2
  
  low_CI <- qnorm(p = cl,     mean = data, sd = sigma)
  upp_CI <- qnorm(p = 1 - cl, mean = data, sd = sigma)

  return(data.frame(lower = low_CI, upper = upp_CI))
}

Cook_IL_SIR_ofit_CIs <- get_SIR_CIs(data  = Cook_IL_SIR_ofit$fit_I,
                                    sigma = Cook_IL_SIR_ofit_mle_sigma_hat)