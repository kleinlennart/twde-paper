### Plot Data on Maps ###

## Visuals
# - https://userpage.fu-berlin.de/soga/100/10900_plotting/10960_Plotting_maps.html
# - https://de.wikipedia.org/wiki/Mittelpunkte_Deutschlands#Mittelpunkte_der_Bundesländer

library(tidyverse)
library(rgeos)
library(googlesheets4)

# Load Data ---------------------------------------------------------------

## Get poly data of federal states
# raster::getData(country = "Germany", level = 1)
germany <- readRDS("data/external/gadm36_DEU_1_sp.rds")

# Simplify the geometry of the polygon shapes (faster runtime)
germany <- germany %>% gSimplify(tol = 0.01, topologyPreserve = TRUE)


## Get the coords
bundesland_coords <- read_sheet("https://docs.google.com/spreadsheets/d/1fBqwbwJARpPYbkhZ63c3aTg3L4U3WbWlk5uxylxLhKM/edit#gid=1618395913", sheet = "Plot Points")
state_data <- read_sheet("https://docs.google.com/spreadsheets/d/1oywT3MdTXOkZfm1sURnWu49I35dUKJKsnsvoCUx-TS0/edit#gid=1541039396", "Bundesland Report")

states <- left_join(bundesland_coords, state_data)


# Mapping -----------------------------------------------------------------

extrafont::loadfonts() # Register the fonts with R

theme_lk <-
  # ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title.position = "plot",
    plot.caption = element_text(size = 7),
    # axis.title = element_text(size = 10),
    text = element_text(family = "Helvetica Neue")
  )


## Graphic Device
# quartz(title = "Plot1", width = 5, height = 5, pointsize = 10)

#### Plot 1 - Tweets ####

ggplot() +
  theme_void() +
  geom_polygon(
    data = germany,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc", alpha = 1.0
  ) +
  geom_point(
    data = states,
    aes(x = LONG, y = LAT, size = N_tweets), col = "red", alpha = 0.9
  ) +
  # range = size of circle, breaks
  scale_size(name = "", range = c(1, 10)) +
  coord_map() +
  labs(x = "", y = "", title = "Original Tweets per Federal State") +
  theme(legend.position = "left") + # or bottom
  theme_lk


ggsave("plots/geo_map1.png", width = 5, height = 5)


#### Plot 2 - Users ####

# quartz(title = "Plot2", width = 5, height = 5, pointsize = 10)

ggplot() +
  theme_void() +
  geom_polygon(
    data = germany,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc", alpha = 1.0
  ) +
  geom_point(
    data = states,
    aes(x = LONG, y = LAT, size = N_users), col = "blue", alpha = 0.9
  ) +
  # range = size of circle, breaks
  scale_size(name = "", range = c(1, 10)) +
  coord_map() +
  labs(x = "", y = "", title = "Users per Federal State") +
  theme(legend.position = "right") +
  theme_lk

ggsave("plots/geo_map2.png", width = 5, height = 5)


# plot1 + plot2 +  patchwork::plot_layout(ncol = 2)
