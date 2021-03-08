# Purpose: plot representation of COVID-19 cases by state over time.

# Collect COVID-19 time-series data for each state.
states_COVID_data <- map_dfr(states, get_COVID_data)

# Heatmap of cases v. time.
states_COVID_data %>%
  filter(!(location == "Alaska"
           | location == "Hawaii" 
           | location == "Northern Mariana Islands" 
           | location == "Virgin Islands" 
           | location == "District of Columbia" 
           | location == "Puerto Rico"
           | location == "United States Virgin Islands"
           | location == "Commonwealth of the Northern Mariana Islands"
           | location == "Guam"
           | location == "American Samoa")) %>%
  group_by(location) %>%
  select(location, dates, cases) %>%
  ggplot(aes(dates, location, fill = cases)) +
  geom_tile() +
  #scale_fill_viridis() +
  scale_fill_gradientn(
    colors = c(
      "midnightblue", "seagreen3","yellow", "lightsalmon"
    )
  ) +
  theme_light() +
  ggtitle("Heatmap of COVID-19 Cases by State v. Time") +
  labs(
    x = "Date",
    y = NULL,
    subtitle = "(U.S. States in North America in Alphabetical Order)",
    caption = stampf(today())
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(vjust = -.175),
    axis.text.y = element_text(size = 7.5),
    axis.title.y = element_text(vjust = 3),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, vjust = -1.0)
  ) +
  guides(
    fill = guide_colourbar(
      title = "Cases",
      barheight = 9,
      barwidth = 0.5
    )
  )
