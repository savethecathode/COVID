# Join JHU cases and US Census dataframes
# Use fuzzy join because city names don't always match between the two data sets.

# Remove redundant State information from Locale
# Motivation is to remove everything after the comma because commas mess up fuzzy matching
dat <- dat %>% mutate(Locale = gsub(",.*", "", Locale))



# Need to define a vectorized-function for evaluating fuzzy logic
fuzzy <- Vectorize( function(x, y) {
  
  return(isTRUE(as.logical(agrep(x, y))))
  
})


# Start timer
start_time <- Sys.time()

# Fuzzy join JHU cases data with US Census population data on city/town name
testing_df2 <- fuzzy_inner_join(dat, census_data, by=c("State"="STNAME", "Locale"="NAME"), match_fun=fuzzy) #%>% select(State, Locale, Date, Cases, SED_date, POPESTIMATE2019)

stop_time <- Sys.time()
stop_time - start_time  # Show elapsed time

# Check results of fuzzy join
head(testing_df2)


# OBS:
# there are a lot of cities with similar names: Los Angeles County, Los Angeles city, Balance of Los Angeles County