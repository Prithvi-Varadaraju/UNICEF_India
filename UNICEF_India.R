library(tidyverse)
library(maps)

unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")
unicef_indicator_1_1_ <- read_csv("unicef_indicator_1 (1).csv")
unicef_indicator_2_1_ <- read_csv("unicef_indicator_2 (1).csv")


india_metadata <- unicef_metadata_1_ %>% 
  filter(country == "India")

india_population <- india_metadata %>%
  select(year, `Population, total`)

india_economic <- india_metadata %>%
  select(year, `GDP per capita (constant 2015 US$)`, `GNI (current US$)`)

india_health_economic <- india_metadata %>%
  select(`GNI (current US$)`, `Life expectancy at birth, total (years)`)

world_map <- map_data("world")

#WORLD MAP Visulaizaton

unicef_indicator_1_1_$obs_value <- as.numeric(as.character(unicef_indicator_1_1_$obs_value))
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "white", color = "black", size = 0.25) +
  geom_map(data = unicef_indicator_1_1_, map = world_map,
           aes(fill = obs_value, map_id = country),
           color = "black", size = 0.25) +
  scale_fill_gradient(low = "yellow", high = "orange", na.value = "grey", name = "Thinness %") +
  coord_fixed(1.3) +
  labs(title = "Thinness Observation Values by Country") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_line(color = "gray", size = 0.1))

unicef_indicator_2_1_$obs_value <- as.numeric(as.character(unicef_indicator_2_1_$obs_value))
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "white", color = "black", size = 0.25) +
  geom_map(data = unicef_indicator_2_1_, map = world_map,
           aes(fill = obs_value, map_id = country),
           color = "black", size = 0.25) +
  scale_fill_gradient(low = "tomato", high = "tomato4", na.value = "grey",  name = "SIGI %") +
  coord_fixed(1.3) +
  labs(title = "SIGI Observation Values by Country") +
  theme_void() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_line(color = "gray", size = 0.1)) 


#Bar chart Visualization

ggplot(india_population, aes(x = year, y = `Population, total`)) +
  geom_bar(stat = "identity", fill = "yellowgreen") +
  labs(title = "Population Growth in India (1960-2022)", x = "Year", y = "Population Total") +
  theme_minimal()

#Time-Series Chart Visualization

ggplot() +
  geom_line(data = india_economic, aes(x = year, y = `GDP per capita (constant 2015 US$)`, color = "GDP per capita"), size = 1) +
  geom_line(data = india_economic, aes(x = year, y = `GNI (current US$)`, color = "GNI"), size = 1) +
  scale_color_manual(values = c("GDP per capita" = "turquoise3", "GNI" = "violetred2")) +
  labs(title = "Economic Indicators for India (1960-2022)", x = "Year", y = "Value (Current US $)") +
  theme_minimal()

#Scatterplot with Linear Regression Line Visualization

ggplot(india_health_economic, aes(x = `GNI (current US$)`, y = `Life expectancy at birth, total (years)`)) +
  geom_point(color = "tan2", alpha = 0.6) +
  geom_smooth(method = "lm", color = "salmon2") +
  labs(title = "Relationship Between GNI and Life Expectancy in India", x = "GNI (current US$)", y = "Life Expectancy at Birth (Years)") +
  theme_minimal()
