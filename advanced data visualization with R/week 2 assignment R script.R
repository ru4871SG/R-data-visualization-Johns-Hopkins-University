## Problem 1

library(tidyverse)
library(maps)

####make some data for painting the map

my_world_map <- map_data("world")

countries <- unique(my_world_map$region)

set.seed(987)
some_data_values <- data.frame("region"=countries,"Score"=runif(252,0,100))

merged_data <- left_join(my_world_map, some_data_values, by = c("region"="region"))

ggplot(data = merged_data, mapping = aes(x = long, y = lat, group = group, fill = Score)) +
  geom_polygon() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "colorbar") +
  labs(title = "Problem 1")

## Problem 2

set.seed(15)
Measurement<-rnorm(32,50,1)

central_america <- merged_data %>%
  filter(region %in% c("Costa Rica", "Panama", "Nicaragua"))

my_cities <-maps::world.cities

central_america_cities <- my_cities %>%
  filter(country.etc %in% c("Costa Rica", "Panama", "Nicaragua") & pop > 40000)

central_america_cities <- central_america_cities %>%
  add_column(Measurement = Measurement)

ggplot(data = central_america, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white") +
  geom_point(data = central_america_cities, aes(x = long, y = lat, group = NULL, color = Measurement), size = 5) +
  scale_color_distiller(palette=7, direction=-1, breaks=49:51, limits = c(49, 51)) + labs(title = "Problem 2")

## Problem 3

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

s_america<-ne_countries(scale="medium",continent='south america',returnclass="sf")

ggplot() +
  geom_sf(data = s_america, aes(fill = pop_est)) +
  scale_fill_distiller(palette=10, n.breaks = 5, direction=-1, guide=guide_colorbar()) +
  labs(title = "Problem 3") +
  scale_x_continuous(
    labels = function(x) sprintf("%.1f°W", -1 * x)
  ) +
  scale_y_continuous(
    labels = function(y) {
      ifelse(y < 0, sprintf("%.1f°S", -1 * y), ifelse(y == 0, "0°", sprintf("%.1f°N", y)))
    }
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 45, hjust = 1)
  )