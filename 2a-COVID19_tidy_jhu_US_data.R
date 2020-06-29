## Extract population data from deaths_US data set
population_data_US <- deaths_US[c("Province_State", "Combined_Key", "Population")]
population_data_US <- population_data_US %>%
  rename(State = "Province_State", Locale = "Combined_Key") %>% # Rename columns
  mutate(Locale = str_remove(Locale, ", US"))                   # Remove country spec



## Remove data from cruise ships
## Presumably these people were quarantined upon arrival and did not contribute to spread of COVID19
confirmed_US <- confirmed_US %>%
  filter(!(Province_State == "Grand Princess" | Province_State == "Diamond Princess"))
deaths_US <- deaths_US %>%
  filter(!(Province_State == "Grand Princess" | Province_State == "Diamond Princess"))


## Remove columns: UID, iso2, iso3, code3, FIPS, Admin2, Country_Region, Lat, Long_
## Keep columns: Province_State, Combined_Key, and columns corresponding to dates
confirmed_US <- confirmed_US[,-c(seq(1,6), seq(8,10))]

## Keep columns: Province_State, Combined_Key, and columns corresponding to dates
## Omit column 12: Population
deaths_US <- deaths_US[,-c(seq(1,6), seq(8,10), 12)]


## Rename field containing detailed location data
confirmed_US <- confirmed_US %>%
  rename(State = "Province_State", Locale = "Combined_Key")
deaths_US    <- deaths_US    %>%
  rename(State = "Province_State", Locale = "Combined_Key")


## Put data in tidy format
confirmed_US <- confirmed_US %>%
  pivot_longer(
    
    cols = -c(State, Locale),
    names_to  = "Date",
    values_to = "Cases"
    
  )

deaths_US <- deaths_US %>%
  pivot_longer(
    
    cols = -c(State, Locale),
    names_to  = "Date",
    values_to = "Deaths"
    
  )

## Remove ", US" from Locale
confirmed_US <- confirmed_US %>% mutate(Locale = str_remove(Locale, ", US"))
deaths_US    <- deaths_US    %>% mutate(Locale = str_remove(Locale, ", US"))


## Convert dates data to mode date
confirmed_US <- confirmed_US %>% mutate(Date = mdy(Date))
deaths_US    <- deaths_US    %>% mutate(Date = mdy(Date))


## Join tables by State, Locale, and Date
jhu_US_data <- inner_join(
  
  x = confirmed_US,
  y = deaths_US,
  by=c(
    
    "State" = "State",
    "Locale" = "Locale",
    "Date" = "Date"
    
    ),
  keep=FALSE
  
  )

