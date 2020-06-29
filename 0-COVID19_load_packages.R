## Setup environment for COVID-19 modeling

# Move to working directory
setwd("~/COVID19/COVID19modeling")


## Clear work space
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() # free up memory and report the memory usage.


## Load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") # Work with dates
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")         # For web scraping
if(!require(DMwR)) install.packages("DMwR", repos = "http://cran.us.r-project.org") # Has unscale(), the opposite of scale()
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") # Get kmeans()
if(!require(fuzzyjoin)) install.packages("fuzzyjoin", repos = "http://cran.us.r-project.org") # Perform join on imperfect match
if(!require(deSolve)) install.packages("deSolve", repos = "http://cran.us.r-project.org")   # Solve differential equations
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(cgwtools)) install.packages("cgwtools", repos = "http://cran.us.r-project.org") # To append to .RData file use resave()
#if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")


## update packages
#update.packages(ask=FALSE, checkBuilt=TRUE) # update R packages, or use RStudio Packages tab


## View environment
(.packages()) # show list of loaded packages
getwd() # show path to working directory
ls()    # show empty work space