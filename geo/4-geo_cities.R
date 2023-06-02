#### Cities ####
cat("\nCities coding...")

## Get cities data
# from https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/05-staedte.html
# "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/05-staedte.xlsx?__blob=publicationFile"

cities_raw <- readxl::read_xlsx("geo/data/05-staedte.xlsx",
  sheet = "Staedte",
  skip = 3
) %>% janitor::clean_names()


### Cleaning

cities <- cities_raw %>%
  select(lfd_nr,
    state = schlusselnummer,
    city = stadt,
    postal_code = post_leitzahl_1
  ) %>%
  slice(-(1:3)) %>%
  drop_na()

# Factor
cities <- cities %>%
  mutate(state = state %>%
    factor(labels = c(
      "Schleswig-Holstein", "Hamburg", "Niedersachsen",
      "Bremen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz",
      "Baden-Wuerttemberg", "Bayern", "Saarland", "Berlin", "Brandenburg",
      "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thueringen"
    )))

replace_umlaute <- tribble(
  ~pattern, ~replacement,
  "ä", "ae",
  "ö", "oe",
  "ü", "ue",
  "ß", "ss"
)

replace_umlaute_vec <- replace_umlaute$replacement %>%
  stats::setNames(replace_umlaute$pattern)

cities <- cities %>%
  mutate(city_clean = city %>%
    str_to_lower() %>%
    str_replace_all(replace_umlaute_vec) %>%
    str_remove(",.*$") %>% # remove everything after comma
    str_remove("/.*$"))

# manual recodings
cities <- cities %>%
  mutate(city_clean = city_clean %>%
    str_replace("frankfurt am main", "frankfurt"))

postal_codes <- cities %>%
  distinct(postal_code, .keep_all = TRUE) %>%
  select(postal_code, geo_state_postal = state)


city_key <- cities %>%
  select(city_clean, geo_state_city = state) %>%
  add_row(city_clean = "munich", geo_state_city = "Bayern")


city_key <- city_key %>% distinct(city_clean, .keep_all = TRUE)

# translations of major cities?
# - munich?

# Tag cities --------------------------------------------------------------

# same as fuzzy
geo_tokens_tag <- geo_tokens_tag %>%
  left_join(city_key, by = c("user_location_tokens" = "city_clean"))
# keep = TRUE) # FIXME!!

# -> remove stopwords to avoid random fuzzy matching?
# geo_tokens_tag <- geo_tokens_tag %>%
#   fuzzyjoin::stringdist_left_join(city_key,
#     by = c("user_location_tokens" = "city_clean"),
#     method = "lv", # Levenshtein distance (see stringdist-metrics)
#     max_dist = 0 # aach mach dach (change?)
#   )
