install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

options(scipen=999)

#data
unicef_indicator_1_2_ <- read_csv("unicef_indicator_1 (2).csv")
unicef_metadata_2_ <- read_csv("unicef_metadata (2).csv")

myjoin <- full_join(unicef_metadata_2_, unicef_indicator_1_2_, by = c("year"= "time_period", "country" = "country"))
myjoin_2022 <- filter(myjoin, year == 2022)
myjoin_countries <- filter(myjoin, 
                           !country %in% c("China", "France", "Ireland", "India", "Chad", 
                                           "Mexico", "Papua New Guinea", "South Africa", "Ukraine", 
                                           "United Kingdom", "United States", "Central African Republic", 
                                           "Brazil"))
myjoin_2022_countries <- filter(myjoin_2022,
                              country %in% c("China", "France", "Ireland", "India", "Chad", 
                                                                                        "Mexico", "Papua New Guinea", "South Africa", "Ukraine", 
                                                                                        "United Kingdom", "United States", "Central African Republic", 
                                                                                        "Brazil"))

#Visualisation-1
map_world <- map_data("world")
map_join_2022 <- full_join(myjoin_2022, map_world, by = c("country" = "region"))


ggplot(data = map_join_2022) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "orange", high ="red", na.value ="black") +
  labs(
    title = "World map showing surviving infants who received the 3rd dose of Hep-B in 2022",
    subtitle = "Countries in black have no data for 2022",
    x = "Longitude",
    y = "Latitude",
    fill = "%"
  ) +
  theme_minimal()


#Time Series of indicator
filtered_data_2 <- myjoin %>% 
  filter(country %in% c("China", "France", "Ireland", "India", "Chad", 
                        "Mexico", "Papua New Guinea", "South Africa", "Ukraine", 
                        "United Kingdom", "United States", "Central African Republic", 
                        "Brazil") & year >= 2000)

ggplot(data = filtered_data_2) +
  aes(x = year, y = obs_value, group = country, colour = country) +
  geom_line() +
  labs(
    title = "Time series of evolution of Hep-B dosage after 2000",
    subtitle = "Legend shows selected countries",
    x = "% of HEP-B 3rd dosage",
    y = "Year",
    fill = "%"
  ) +
  theme_minimal()


# Scatterplot showing relationship between GDP and life exp (regression line)
filtered_data_3 <- myjoin %>% 
  filter(country %in% c("China", "France", "Ireland", "India", "Chad", 
                        "Mexico", "Papua New Guinea", "South Africa", "Ukraine", 
                        "United Kingdom", "United States", "Central African Republic", 
                        "Brazil") & year >= 2018)
ggplot(filtered_data_3) +
  aes(`Life expectancy at birth, total (years)`, `GDP per capita (constant 2015 US$)`, color = country) +
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm", se=FALSE, aes(group=1), color="black", linetype="dashed") +
  labs(
    title = "Scatterplot of relationship between life expectancy and GDP per capita",
    subtitle = "Legend shows selected countries",
    x = "Life expectancy",
    y = "GDP per capita"
  ) +
  theme_grey()



# Barchart 
ggplot(myjoin_2022_countries) +
  aes(country, obs_value, fill = country) +
  geom_col() +
  labs(
    title = "Bar chart showing Hep-B coverage in 2022",
    subtitle = "Legend shows selected countries",
    x = "Country",
    y = "Indicator %"
  ) +
  theme_grey()
  
