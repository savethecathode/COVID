# Purpose:  Get shapefile for US states from US Census Bureau.
# Read shapefile using Simple Features (sf) package
# (automatically loads rgdal package).
#library(sf)


# In browser, right-click the file link, select Copy Link Location
# URL: https://www2.census.gov/geo/tiger/TIGER2019/STATE/
# Accessed on 10/28/2020
URL <- "https://www2.census.gov/geo/tiger/TIGER2019/STATE/tl_2019_us_state.zip"


# Setup directory for shapefile
mainDir <- "~/COVID19/COVID19SIR/"
subDir  <- "Shapefiles/"

ifelse(!dir.exists(file.path(mainDir, subDir)),
       dir.create(file.path(mainDir, subDir)), FALSE)

# (file size should be 3.2 MB)
shape_zip <- paste0(mainDir, subDir, "tl_2019_us_state.zip")
download.file(url = URL, destfile = shape_zip)

# Unpack zip file
shape_dir <- paste0(mainDir, subDir, "tl_2019_us_state")
unzip(zipfile = shape_zip, exdir = shape_dir)

# Define area of interest
# Read shapefile
shape_file <- paste0(mainDir, subDir,
                     "tl_2019_us_state/", "tl_2019_us_state.shp")
aoi_boundary <- st_read(shape_file)
