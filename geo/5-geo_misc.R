
# Postcodes ---------------------------------------------------------------

# postal_codes data from cities

# users <- users %>%
#   mutate(geo_postal_code = str_extract(
#     user_location_clean,
#     "\\b[[:digit:]]{5}\\b"
#   ))
postal_codes <- postal_codes %>% distinct(postal_code, .keep_all = TRUE)

geo_tokens_tag <- geo_tokens_tag %>%
  left_join(postal_codes, by = c("user_location_tokens" = "postal_code"))


# Coordinates -------------------------------------------------------------

# # Detect and clean coordinates in descrip
# get_coordinates <- function(dat) {
#   return(
#     dat %>%
#       # detect decimal coordinates
#       filter(str_detect(location, "\\b\\d{1,2}[.,]\\d{4,}")) %>%
#       mutate(
#         # remove all characters besides coordinates
#         location_clean = str_replace_all(location, "[^[:digit:][[:blank:]].,-]", "")
#       )
#   )
# }

# FIXME: add tidygeocoder
