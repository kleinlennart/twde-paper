### Country Tagging ###

# Flag tagging
# users <- users %>%
#   mutate(geo_country_flag = if_else(
#     str_detect(user_location, "🇩🇪"), true = "Germany", false = NA_character_
#   ))

geo_tokens_tag <- geo_tokens

# on same tokens data structure (for later merging) but not on token var
geo_tokens_tag <- geo_tokens_tag %>%
  mutate(geo_country_flag = if_else(
    str_detect(user_location, "🇩🇪"),
    true = "Germany", false = NA_character_
  ))


# Keyword Matching --------------------------------------------------------

country_key <-
  tibble(
    geo_country_match = "Germany",
    country_keyword =
      c("BRD", "DEU", "GER", "Deutsch", "Deutschland", "German", "Germany", "Alemania", "Germania") %>%
        str_to_lower()
  )
# TODO: add more countries as joined tibbles here

geo_tokens_tag <- geo_tokens_tag %>%
  left_join(country_key, by = c("user_location_tokens" = "country_keyword"))

# geo_tokens_tag <- geo_tokens %>%
#   fuzzyjoin::stringdist_left_join(country_key,
#     by = c("user_location_tokens" = "country_keyword"),
#     method = "lv", # Levenshtein distance (see stringdist-metrics)
#     max_dist = 0 # 1 or 2, 3 is too much!
#   )


# Old ---------------------------------------------------------------------

# Pattern matching
# germany_match <- c("BRD", "DEU", "GER", "Deutsch", "Deutschland", "German", "Germany", "Alemania", "Germania")
# germany_match_regex <- paste0("(", paste(germany_match, collapse = "|"), ")")
#
# users <- users %>%
#   mutate(geo_country_match = if_else(
#     str_detect(user_location, stringr::regex(germany_match_regex, ignore_case = TRUE)),
#     true = "Germany", false = NA_character_
#   ))
