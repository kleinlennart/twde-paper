# Load libraries
library(tidyverse)
library(tidytext)

## Import data
cat("\nReading data...")
# saveRDS(final, here::here("4-Data_Exports", "output", "final_data.rds"))
# dat <- readRDS("6-Analyses/Geotagging/output/test.rds")
# FIXME: final data

## select user data only (smaller payload)
users_raw <- all_split$dataset %>%
  filter(!is_bot) %>% # IMPORTANT no bots
  select(user_id, source, user_description, user_location) %>%
  distinct(user_id, .keep_all = TRUE)

# Clean data ------------------------------------------------------

## Words + Umlaute Replacement
# TODO: add words!!
replace_words <- tribble(
  ~pattern, ~replacement,
  "\\&", "and",
  # "\\@", "at"
)
# make named vector (name = regex match, element = replacement) for "str_replace_all"
replace_words_vec <- replace_words$replacement %>%
  stats::setNames(replace_words$pattern)

replace_umlaute <- tribble(
  ~pattern, ~replacement,
  "ä", "ae",
  "ö", "oe",
  "ü", "ue",
  "ß", "ss"
)
replace_umlaute_vec <- replace_umlaute$replacement %>%
  stats::setNames(replace_umlaute$pattern)

rm(list = c("replace_words", "replace_umlaute"))

clean_strings <- function(string) {
  string %>%
    # TODO: remove URLs?
    str_to_lower() %>%
    str_replace_all(replace_umlaute_vec) %>%
    str_replace_all(replace_words_vec) %>%
    stringi::stri_trans_general(str = ., id = "latin-ascii") %>% # NOTE: replace Umlaute before!
    str_replace_all("[^[a-zA-Z0-9][äÄöÖüÜß][-.#@ ][°\"`´]]", " ") %>% # keep some symbols
    str_replace_all("[:space:]", " ") %>%
    str_squish() %>% # remove trailing & repeated whitespace
    na_if("")
}
### Tests:
# clean_strings("é")

# Process vars
users <- users_raw %>%
  mutate(
    .after = "user_description",
    user_description_clean = user_description %>%
      clean_strings()
  ) %>%
  mutate(
    .after = "user_location",
    user_location_clean = user_location %>%
      clean_strings()
  )

## Check
# users %>%
#   distinct(user_description, .keep_all = TRUE) %>%
#   View()


## Tag potential bots

# users <- users %>%
#   mutate(
#     matched_bot = str_detect(user_description)
#     # any(str_detect(source))
#   )


### tokenize + prep
# -> how to get back to full user data?
geo_tokens <- users %>%
  drop_na("user_location_clean") %>% # What to do with removed users? -> set NA to empty string?!
  tidytext::unnest_tokens(
    input = "user_location_clean",
    output = "user_location_tokens",
    token = "words", # FIXME
    drop = FALSE,
  )

# remove stopwords
stopwords_de <- stopwords::stopwords(language = "de") %>%
  str_replace_all(replace_umlaute_vec)

stopwords_en <- stopwords::stopwords(language = "en")
# stopwords_en <- stopwords_en[!stopwords_en %in% c("at")]
# relevant to match Austria? (not a good idea!)
# TODO: add other exeptions

geo_tokens <- geo_tokens %>%
  filter(!user_location_tokens %in% stopwords_de) %>%
  filter(!user_location_tokens %in% stopwords_en)


# Export user data
cat("\nSaving cleaned geo data...")
saveRDS(geo_tokens, "data/geo_tokens.rds")
