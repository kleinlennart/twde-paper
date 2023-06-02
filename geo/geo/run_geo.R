### Script to run OSM Geocoding API on dataset ###


library(tidyverse)
library(tidygeocoder)

callGeoAPI <- function(dat) { # needs to be a dataframe
  cat("Started geocoding...")
  dat %>% geocode(
    address = location_clean, # Switch here
    method = "osm",
    full_results = TRUE,
    verbose = FALSE # Show logs
  )
}

dat <- readRDS("data", "TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")

dat <- dat %>% mutate(
  location_clean = case_when(
    location == NA ~ as.character(NA),
    # also NA if nothing but space
    !str_detect(location, "[^[:space:]]") ~ as.character(NA), 
    TRUE ~ location %>%
      str_replace_all("[^[:ascii:][äÄöÖüÜß][°]]", replacement = "")
  )
)

# Run Geo API
geo_results <- callGeoAPI(dat)

# Remove useless variables
geo_results <- geo_results %>% 
  select(-icon, -importance, -licence, -place_id, -boundingbox, -osm_id)

saveRDS(geo_results, here::here("data", "output", "geo_results.rds"))
