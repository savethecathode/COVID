


## Omit latitude and longitude columns
confirmed_global <- confirmed_global[-c(3,4)]


## Renaim fields
confirmed_global <- confirmed_global %>% rename(State = "Country/Region", Locale = "Province/State")


## Put data in tidy format
confirmed_global <- confirmed_global %>%
  pivot_longer(
    cols = -c(State, Locale),
    names_to  = "Date",
    values_to = "Cases"
  )



## Omit latitude and longitude columns
deaths_global <- deaths_global[-c(3,4)]


## Renaim fields
deaths_global <- deaths_global %>% rename(State = "Country/Region", Locale = "Province/State")


## Put data in tidy format
deaths_global <- deaths_global %>%
  pivot_longer(
    cols = -c(State, Locale),
    names_to  = "Date",
    values_to = "Deaths"
  )



## Omit latitude and longitude columns
recovered_global <- recovered_global[-c(3,4)]


## Renaim fields
recovered_global <- recovered_global %>% rename(State = "Country/Region", Locale = "Province/State")


## Put data in tidy format
recovered_global <- recovered_global %>%
  pivot_longer(
    cols = -c(State, Locale),
    names_to  = "Date",
    values_to = "Recovered"
  )


# Join tables by State, Locale, and Date
jhu_global_data <- inner_join(confirmed_global, deaths_global, by=c("State"="State", "Locale"="Locale", "Date"="Date"), keep=FALSE)
jhu_global_data <- inner_join(jhu_global_data, recovered_global, by=c("State"="State", "Locale"="Locale", "Date"="Date"), keep=FALSE)
