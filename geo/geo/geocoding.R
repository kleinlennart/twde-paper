#### Geo Code Users / Tweets ####

library(tidyverse)

# TODO:
# - implement tied variables (with string pastings)
# - improve Regex
# - Streamline the process

# data <- readRDS("data/TWDE-FINAL-TWEETS-2021-05-01.rds")

# test data
dat <- readRDS("data/testdata.rds") %>% sample_n(1000)

setwd(here::here())
dat <- targets::tar_read(clean_senti)
d <- dat
dat <- dat %>% sample_n(1000)
dat <-d 

## Add Clean Variables
# FIXME: Possible speedup with group_by user_id and group mutate + ungrouping afterwards
dat <- dat %>%
  mutate(
    user_description_clean = user_description %>%
      tolower() %>% # useful for tokenization (not needed for regex -> ignore_case)
      str_replace_all("[^[:ascii:][äÄöÖüÜß]]", replacement = "") %>% # remove all non German-Ascii Unicode characters
      trimws(whitespace = "[ \t\r\n]") %>% # Remove Leading/Trailing whitespace (also punctuation with [:punct:]?) -> can remove hashtags!
      na_if("") %>% # empty strings as NA
      str_replace_all("ä", "ae") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ß", "ss"),
    
    user_location_clean = user_location %>%
      tolower() %>%
      str_replace_all("[^[:ascii:][äÄöÖüÜß]]", replacement = "") %>% # remove all non German-Ascii Unicode characters (also emoji)
      str_replace_all("[#&+,/@:;|()]", "") %>%  # keep hyphens
      trimws(whitespace = "[ \t\r\n]") %>% # Remove Leading/Trailing whitespace + (also punctuation? [:punct:])
      na_if("") %>% # empty strings as NA
      str_replace_all("ä", "ae") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ß", "ss")
  )

## Errors:
# México -> Mxico

## This function is like mutate but only acts on the rows satisfying the condition
# Needed to tag specific tweets in dataset
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

## Add tagging variables
dat$geo_country <- ""
dat$geo_city <- ""
dat$geo_name_state <- ""

## Review Data
# dat %>%
#   # distinct(user_id, .keep_all = TRUE) %>%
#   select(
#     text, user_description, user_description_clean, user_location, user_location_clean,
#     contains("geo")
#   ) %>% View()

#### Tag Countries -----------------------------------------------------------------

### Germany ###

## Regex String for non-greedy matching of keywords
germany_short_regex <- c("BRD", "DEU", "GER") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

## Need to check for missing values otherwise throws error!
dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(germany_short_regex, ignore_case = TRUE)),
              geo_country = paste0(geo_country, ", ", "Germany")
  ) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(germany_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany"))

# Ignore_case FALSE for "DE" because of Spanish cities like "Abc *de* Xyz"
dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex("(\\W|^)DE(\\W|$)", ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex("(\\W|^)DE(\\W|$)", ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Germany"))


## For longer Keywords
germany_regex <- c("Deutsch", "Deutschland", "German", "Germany", "Schland") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(germany_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(germany_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany"))


### Switzerland ###
swiss_regex <- c("Swiss, Schweiz, Switzerland", "Switzer", "Suisse", "Svizzera", "Svizra") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(swiss_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(swiss_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland"))

swiss_short_regex <- c("CH", "CHE") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(swiss_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(swiss_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland"))

# Austria
austria_regex <- c("Austria", "Oesterreich")
dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(austria_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(austria_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria"))

austria_short_regex <- c("AT", "AUT") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)") # non-greedy matching

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(austria_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(austria_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria"))


### By Flags ###
## Read in Flags
flags <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Wez9YJq8fYJRa-LUNilUAY_hXRhUEB1jkTs48XJKPhQ/edit#gid=763046312",
  "Flags"
)


## Flags of all Countries but Austria, Germany and Switzerland
other_flags_regex <- flags %>%
  filter(Category == "Other Country") %>%
  pull(Emoji) %>%
  paste(collapse = "|")

# does not work on clean variables!
dat <- dat %>%
  mutate_cond(!is.na(user_location) &
                str_detect(user_location, other_flags_regex), geo_country = paste0(geo_country, ", ", "Other")) %>%
  mutate_cond(!is.na(user_description) &
                str_detect(user_description, other_flags_regex), geo_country = paste0(geo_country, ", ", "Other"))

## Germany
dat <- dat %>%
  mutate_cond(!is.na(user_location) &
                str_detect(user_location, "🇩🇪"), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(user_description) &
                str_detect(user_description, "🇩🇪"), geo_country = paste0(geo_country, ", ", "Germany"))

## Switzerland
dat <- dat %>%
  mutate_cond(!is.na(user_location) &
                str_detect(user_location, "🇨🇭"), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(user_description) &
                str_detect(user_description, "🇨🇭"), geo_country = paste0(geo_country, ", ", "Switzerland"))

## Austria
dat <- dat %>%
  mutate_cond(!is.na(user_location) &
                str_detect(user_location, "🇦🇹"), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(user_description) &
                str_detect(user_description, "🇦🇹"), geo_country = paste0(geo_country, ", ", "Austria"))


# Tag Other countries by (German) country name ----------------------------

# Country names are in German though!
states <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Wez9YJq8fYJRa-LUNilUAY_hXRhUEB1jkTs48XJKPhQ/edit#gid=194692324", "Staaten")

states_regex <- states %>%
  unlist() %>%
  tolower() %>%
  unique() %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean) &
                str_detect(user_location_clean, regex(states_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Other")) %>%
  mutate_cond(!is.na(user_description_clean) &
                str_detect(user_description_clean, regex(states_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Other"))


### Tag Cities --------------------------------------------------------------------
dat$geo_city <- ""

# Get a list of nearly all Towns in Germany
# FIXME: what if two states have a town with the same name?
german_cities <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Wez9YJq8fYJRa-LUNilUAY_hXRhUEB1jkTs48XJKPhQ/edit#gid=111326953", "German Cities") 
german_cities <- german_cities %>%
  mutate(
    Stadt = Stadt %>%
      tolower() %>%
      str_replace_all("ä", "ae") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ß", "ss"),
    Bundesland = Bundesland %>%
      str_replace_all("ü", "ue")
  )


# FIXME: Make non Greedy?? (Hessen = Essen)
# FIXME: What about commas before city names?

# FIXME: way too slow! (maybe change \\W to simple whitespace)
german_cities_regex <- german_cities %>%
  pull("Stadt") %>%
  sample() %>% # random permutation
  paste(collapse = "|")
  

  # FIXME: too slow? @CB might try on your machine
  # paste(collapse = "(\\W|$)|(\\W|^)") %>%
  # paste0("(\\W|^)", ., "(\\W|$)") # non-greedy matching

  
dat <- dat %>% 
  mutate(
    # FIXME: Add multiple cities?! (extract_all) -> for overlap report! (slow!)
    # -> Could just scramble the regex pattern to randomly break ties (what gets extracted first)
    geo_city = str_extract(user_location_clean, regex(german_cities_regex, ignore_case = TRUE))
    # na_if("character(0)")
  )

stringi::stri_extract_regex()

# TODO: separately calculate an overlap report here! (just to legitimise random tie breaking, as we have done with the previous dataset)

#### Tag States ------------------------------------------------------

### Tag states by city
# TODO: map function to map every single city to a bundesland if matched more than one
dat["geo_city_state"] <- german_cities[match(dat$geo_city, german_cities$Stadt), "Bundesland"]


### Tag states by name / keyword

# TODO: Check if greedy overmatching occurs
bund_names_regex <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
  "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
  "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
  "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
) %>%
  sample() %>% 
  str_replace_all("ü", "ue") %>%
  paste(collapse = "|")

## Match state by name
dat$geo_name_state <- ""

dat <- dat %>%
  mutate_cond(!is.na(user_location_clean),
              geo_name_state = str_extract(user_location_clean, regex(bund_names_regex, ignore_case = TRUE))
              # %>% na_if("character(0)")
  )

# TODO: Calculate overlap!

### By Keyword
bund_iso_regex <- c(
  "BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW",
  "NRW", "RP", "RLP", "SL", "SN", "ST", "SH", "TH"
) %>%
  sample() %>% 
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

## Match State by State ISO
dat$geo_iso_state <- ""
dat <- dat %>%
  mutate_cond(!is.na(user_location_clean),
              geo_iso_state = str_extract(user_location_clean, regex(bund_iso_regex, ignore_case = TRUE))
  )


# dat <- dat %>%
#   rowwise() %>%
#   mutate(
#     geo_iso_state = paste(geo_iso_state, collapse = ", "),
#     geo_name_state = paste(geo_name_state, collapse = ", ")
#   )


# Unite measures
dat <- dat %>%
  unite("geo_state", geo_name_state, geo_city_state, geo_iso_state,
        remove = FALSE, na.rm = TRUE, sep = " "
  ) %>%
  mutate(
    state = geo_state %>% str_replace_all("NA", "") %>% trimws()
  )


#### Tag States dichotom
match.helper <- function(chr_data, pattern) {
  return(
    str_detect(
      chr_data,
      regex(pattern, ignore_case = TRUE)
    ) %>%
      as.numeric() # make numeric categorical for checksum (0 / 1)
  )
}

dat <- dat %>%
  mutate(
    geo_bw = match.helper(state, "Baden-Wuerttemberg|BW"),
    geo_bayern = match.helper(state, "Bayern|BY"),
    geo_berlin = match.helper(state, "Berlin|BE"),
    geo_brandenburg = match.helper(state, "Brandenburg|BB"),
    geo_bremen = match.helper(state, "Bremen|HB"),
    geo_hamburg = match.helper(state, "Hamburg|HH"),
    geo_hessen = match.helper(state, "Hessen|HE"),
    geo_mv = match.helper(state, "Mecklenburg-Vorpommern|MV"),
    geo_nds = match.helper(state, "Niedersachsen|NI"),
    geo_nrw = match.helper(state, "Nordrhein-Westfalen|NW|NRW"),
    geo_rlp = match.helper(state, "Rheinland-Pfalz|RP|RLP"),
    geo_saarland = match.helper(state, "Saarland|SL"),
    geo_sachsen = match.helper(state, "Sachsen[^-]|SN"), # prevet from prematching Sachsen-Anhalt
    geo_sachsenan = match.helper(state, "Sachsen-Anhalt|ST"),
    geo_sh = match.helper(state, "Schleswig-Holstein|SH"),
    geo_thueringen = match.helper(state, "Thueringen|TH"),
  )





# dat <- dat %>%
#   mutate(
#     n_states = map_int(geo_iso_state, ~ length(.x))
#   )
# 
# dat %>%
#   filter(n_states > 1) %>%
#   View()

#### Edge Cases ####

# dat <- dat %>%
#   mutate(
#     geo_coord = str_extract_all(user_location_clean, "\\b\\d{1,2}[.,]\\d{4,}") %>% na_if("character(0)")
#   )
# 
# geo_coordinate ## Coords
# dat$location_cat[str_detect(dat$location, "\\b\\d{1,2}[.,]\\d{4,}")] <- "coords"
# 
# dat <- dat %>%
#   mutate(
#     user_location_clean = case_when(
#       location_cat == "coords" ~ str_replace_all(location, "[^[:digit:][[:blank:]].,-]", ""), # Replace everything but relevant chars for coordinates
#       TRUE ~ user_location_clean
#     )
#   )
