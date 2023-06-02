##### Geo Maps ######
### Prep Data

## Get poly data of germany + federal states (level 1)
# raster::getData(country = "Germany", level = 1)
germany <- readRDS(here::here("data", "external", "gadm36_DEU_1_sp.rds"))

# Simplify the geometry of the polygon shapes (for faster plotting runtime)
germany <- germany %>% rgeos::gSimplify(tol = 0.01, topologyPreserve = TRUE)

# Get coords of points
bundesland_coords <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1fBqwbwJARpPYbkhZ63c3aTg3L4U3WbWlk5uxylxLhKM/edit#gid=1618395913",
  sheet = "Plot Points"
)

# Get Geo Stats
bundesland_data <- read.csv(here::here("data", "geo_stats.csv"))
states <- cbind(bundesland_coords, bundesland_data) %>% select(-geo_bundesland)


quartz(title = "Plot1", width = 6, height = 4)
library(ggthemes)

### Tweets
# ca. 49.41% aller tweets aufgeklärt 
ggplot() +
  geom_polygon(
    data = germany,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = states,
    aes(x = LONG, y = LAT, size = N_tweets), col = "red",
    alpha = 1.0
  ) +
  scale_size(
    name = "Anzahl Tweets",
    range = c(0.5, 12),
    breaks = c(25000, 50000, 100000)
  ) +
  coord_map() +
  theme_map() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.title = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 10, face = "plain")
  )

ggsave("tweet_map.png", width = 6, height = 4)


### User
# derzeit 38.27% aller User erklärt (nicht alle aus Deutschland)
ggplot() +
  geom_polygon(
    data = germany,
    aes(x = long, y = lat, group = group),
    colour = "grey10", fill = "#fff7bc"
  ) +
  geom_point(
    data = states,
    aes(x = LONG, y = LAT, size = N_users), col = "blue",
    alpha = 1.0
  ) +
  scale_size(
    name = "Anzahl User",
    range = c(0.5, 12)
  ) +
  coord_map() +
  theme_map() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.title = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 10, face = "plain")
  )

ggsave("user_map.png", width = 6, height = 4)
