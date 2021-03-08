# Setup environment for COVID-19 modeling


# Move to working directory
setwd("~/COVID19/COVID19SIR")


# Clear work space
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() # free up memory and report the memory usage.


# Load packages
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
# Parallelization
if(!require(furrr)) install.packages("furrr",
                                     repos = "http://cran.us.r-project.org")
# Work with dates
if(!require(lubridate)) install.packages("lubridate",
                                         repos = "http://cran.us.r-project.org")
# For web scraping
if(!require(rvest)) install.packages("rvest",
                                     repos = "http://cran.us.r-project.org")
# Has unscale(), the opposite of scale()
if(!require(DMwR)) install.packages("DMwR",
                                    repos = "http://cran.us.r-project.org")
# Get kmeans()
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
# Perform join on imperfect match
if(!require(fuzzyjoin)) install.packages("fuzzyjoin",
                                         repos = "http://cran.us.r-project.org")
# Solve differential equations
if(!require(deSolve)) install.packages("deSolve",
                                       repos = "http://cran.us.r-project.org")
# Maximum likelihood estimation 
if(!require(bbmle)) install.packages("bbmle",
                                     repos = "http://cran.us.r-project.org")
# Get := operator
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr",
                                     repos = "http://cran.us.r-project.org")
# To append to .RData file use resave()
if(!require(cgwtools)) install.packages("cgwtools",
                                        repos = "http://cran.us.r-project.org")
# Viridis color gradient
if(!require(viridis)) install.packages("viridis",
                                        repos = "http://cran.us.r-project.org")
# Read shapefile using Simple Features (sf) package (automatically loads rgdal)
if(!require(sf)) install.packages("sf",
                                  repos = "http://cran.us.r-project.org")
# Repel labels in ggplot2
if(!require(ggrepel)) install.packages("ggrepel",
                                  repos = "http://cran.us.r-project.org")


# update R packages, or use RStudio Packages tab
#update.packages(ask=FALSE, checkBuilt=TRUE)

## Parallelization setup
#future::plan(multiprocess) # One-time start up cost
#future::plan(sequential) # restore serial setup

# View environment
(.packages()) # show list of loaded packages
getwd() # show path to working directory
ls()    # show empty work space

