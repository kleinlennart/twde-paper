library(tidyverse)
library(fuzzyjoin)

#
# users <- users %>%
#   mutate(geo_state_match = if_else(
#     str_detect(
#       user_location,
#       stringr::regex("\\bNRW\\b",
#         ignore_case = TRUE
#       )
#     ),
#     true = "NRW", false = NA_character_
#   ))
#
# users <- users %>%
#   mutate(geo_state_match = if_else(
#     str_detect(
#       user_location,
#       stringr::regex("\\bNI\\b",
#         ignore_case = TRUE
#       )
#     ),
#     true = "NI", false = NA_character_
#   ))
#
# users <- users %>%
#   mutate(
#     geo_state_match = user_location %>%
#       str_extract_all() %>%
#       str_replace("regex_vector to get clean normalized names for each state")
#   )


# TODO: add \\b to everything ?!
# -> probably not neccessary!

# but: add different spellings (or fuzzy matching?!)
# "Baden-Wuerttemberg" vs. "Baden Wuerttemberg" or just "baden"

match_helper <- function(input_string, rgx) {
  str_detect(
    input_string,
    stringr::regex(rgx, ignore_case = TRUE)
  )
}

# NOTE: users can be in multiple states, this data structure represents this problem!
geo_tokens_tag <- geo_tokens_tag %>%
  mutate(
    geo_bw = match_helper(user_location_clean, "Baden-Wuerttemberg|BadenWuerttemberg|\\bBW\\b"), # schwarzwald
    geo_by = match_helper(user_location_clean, "Bayern|\\bBY\\b"), # bavaria
    geo_be = match_helper(user_location_clean, "Berlin|\\bBE\\b"),
    geo_bb = match_helper(user_location_clean, "Brandenburg|\\bBB\\b"),
    geo_hb = match_helper(user_location_clean, "Bremen|\\bHB\\b"),
    geo_hh = match_helper(user_location_clean, "Hamburg|\\bHH\\b"),
    geo_he = match_helper(user_location_clean, "Hessen|\\bHE\\b"),
    geo_mv = match_helper(user_location_clean, "Mecklenburg-Vorpommern|MecklenburgVorpommern|Mecklenburg|\\bMV\\b"),
    geo_nds = match_helper(user_location_clean, "Niedersachsen|\\bNI\\b"),
    geo_nrw = match_helper(user_location_clean, "Nordrhein-Westfalen|NordrheinWestfalen|Nordrhein|\\bNW\\b|\\bNRW\\b"),
    geo_rlp = match_helper(user_location_clean, "Rheinland-Pfalz|RheinlandPfalz|\\bPfalz\\b|\\bRP\\b|\\bRLP\\b"),
    geo_sl = match_helper(user_location_clean, "Saarland|\\bSL\\b"),
    geo_sn = match_helper(user_location_clean, "Sachsen[^-]|\\bSN\\b"), ## FIXME: CHECK! (works but see Issue #1 below)
    geo_st = match_helper(user_location_clean, "Sachsen-Anhalt|SachsenAnhalt|\\bST\\b"),
    geo_sh = match_helper(user_location_clean, "Schleswig-Holstein|Schleswig-Holstein|\\bSH\\b"),
    geo_th = match_helper(user_location_clean, "Thueringen|\\bTH\\b"),
  )


## Regionen?
# - Schwarzwald?


## NOTE: should be able to be in multiple states!!
## -> average?!

# With Tokens and fuzzy matching ------------------------------------------

# states_key <-
#   tribble(
#     ~geo_state_match, ~geo_state_keyword,
#     "BW", "baden-wuerttemberg",
#     "NRW", "nordrhein-westfalen",
#     "BE", "berlin", # baerlin, balin # -> fuzzy string matching is great!
#     "ST", "sachsen-anhalt",
#     "SN", "sachsen",
#     "NDS", "niedersachsen"
#   )
#
# geo_tokens_tag <- geo_tokens_tag %>%
#   fuzzyjoin::stringdist_left_join(states_key,
#     by = c("user_location_tokens" = "geo_state_keyword"),
#     method = "lv", # Levenshtein distance (see stringdist-metrics)
#     max_dist = 1 # 1 or 2, 3 is too much!
#   )
