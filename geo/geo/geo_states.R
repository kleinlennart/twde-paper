#### Geocode German Federal States ####

library(tidyverse)
library(tidygeocoder)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")
# User Level Aggregation
dat <- dat %>%
  distinct(user_id, .keep_all = TRUE) %>%
  select(user_id, description, location, country)


## Add Clean Variables
dat <- dat %>% mutate(
  description_clean = description %>%
    str_replace_all("[^[:ascii:][äÄöÖüÜß]]", replacement = "") %>%
    trimws(whitespace = "[ \t\r\n[:punct:]]") %>%
    na_if("") %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ß", "ss"),
  location_clean = location %>%
    str_replace_all("[^[:ascii:][äÄöÖüÜß]]", replacement = "") %>%
    trimws(whitespace = "[ \t\r\n[:punct:]]") %>%
    na_if("") %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ß", "ss")
)

## This function is like mutate but only acts on the rows satisfying the condition
# Needed to tag specific tweets in dataset
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

### Tag Cities --------------------------------------------------------------------

# Load German Cities and clean
german_cities <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Wez9YJq8fYJRa-LUNilUAY_hXRhUEB1jkTs48XJKPhQ/edit#gid=111326953",
  "German Cities"
) %>%
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
## Create City Regex
german_cities_regex <- german_cities %>%
  pull("Stadt") %>%
  paste(collapse = "|")

# FIXME: Deal with multiple cities
## Extract Cities from location
dat$geo_city <- ""
dat <- dat %>%
  mutate(
    geo_city = str_extract(location_clean, regex(german_cities_regex, ignore_case = TRUE)) %>%
      tolower()
    # na_if("character(0)")
  )


#### Tag States ------------------------------------------------------

# TODO: a map function to map every single city to a bundesland if extracted more than one above

## Tag states by city (full state name)
dat["geo_city_state"] <- german_cities[match(dat$geo_city, german_cities$Stadt), "Bundesland"]


### Tag states by name / keyword ###


# TODO: Check if greedy overmatching occurs
## Create State Name Regex
state_names_regex <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
  "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
  "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen[^-]",
  "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
) %>%
  str_replace_all("ü", "ue") %>%
  paste(collapse = "|")


### Match state by name (extracts multiple)
dat$geo_name_state <- ""
dat <- dat %>%
  mutate_cond(!is.na(location_clean),
    geo_name_state = str_extract_all(location_clean, regex(state_names_regex, ignore_case = TRUE)) %>% 
      na_if("character(0)")
  )

### Match state by ISO (extracts multiple)
state_iso_regex <- c(
  "BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW",
  "NRW", "RP", "RLP", "SL", "SN", "ST", "SH", "TH"
) %>%
  paste(collapse = "(?=\\W|$)|(?<=\\W|^)") %>%
  paste0("(?<=\\W|^)", ., "(?=\\W|$)")

## Match State by State ISO
dat$geo_iso_state <- ""
dat <- dat %>%
  mutate_cond(!is.na(location_clean),
    geo_iso_state = str_extract_all(location_clean, regex(state_iso_regex, ignore_case = FALSE))
    %>% na_if("character(0)")
  )


## Collapse the extracted vectors (if multiple states were extracted before)
# e.g. c("Thueringen", "Berlin") -> "Thueringen, Berlin"
# important for 'unite'
dat <- dat %>%
  rowwise() %>%
  mutate(
    geo_name_state = paste(geo_name_state, collapse = ", "),
    geo_iso_state = paste(geo_iso_state, collapse = ", ")
  ) %>% ungroup()


## Unite the 3 state measures
dat <- dat %>%
  unite("geo_state", geo_city_state, geo_name_state, geo_iso_state,
    remove = FALSE, na.rm = TRUE, sep = " "
  ) %>%
  # TODO: Remove NAs beforehand?!
  mutate(
    # Remove NA and trim
    geo_state = geo_state %>% str_replace_all("NA", "") %>% trimws() %>% na_if("")
  ) %>%
  relocate(geo_state, .after = last_col())


## Report Data Status in `geo_state` (if no location data was available)
dat <- dat %>% mutate_cond(is.na(geo_state),
  geo_state = case_when(
    is.na(location_clean) ~ "No location_clean",
    TRUE ~ NA_character_
  )
)

### Dichotomize State Tags for analysis (numeric)
match.helper <- function(chr_data, pattern) {
  return(
    str_detect(
      chr_data,
      regex(pattern, ignore_case = TRUE)
    ) %>%
      as.numeric() # make numeric categorical for checksum (0 / 1)
  )
}

## Dichtomisation over geo_state variable
# TODO: Check for errors
dat <- dat %>%
  mutate(
    geo_bw = match.helper(geo_state, "Baden-Wuerttemberg|( |^)BW( |$)"),
    geo_bayern = match.helper(geo_state, "Bayern|( |^)BY( |$)"),
    geo_berlin = match.helper(geo_state, "Berlin|( |^)BE( |$)"),
    geo_brandenburg = match.helper(geo_state, "Brandenburg|( |^)BB( |$)"),
    geo_bremen = match.helper(geo_state, "Bremen|( |^)HB( |$)"),
    geo_hamburg = match.helper(geo_state, "Hamburg|( |^)HH( |$)"),
    geo_hessen = match.helper(geo_state, "Hessen|( |^)HE( |$)"),
    geo_mv = match.helper(geo_state, "Mecklenburg-Vorpommern|( |^)MV( |$)"),
    geo_nds = match.helper(geo_state, "Niedersachsen|( |^)NI( |$)"),
    geo_nrw = match.helper(geo_state, "Nordrhein-Westfalen|( |^)NW( |$)|( |^)NRW( |$)"),
    geo_rlp = match.helper(geo_state, "Rheinland-Pfalz|( |^)RP( |$)|( |^)RLP( |$)"),
    geo_saarland = match.helper(geo_state, "Saarland|( |^)SL( |$)"),
    geo_sachsen = match.helper(geo_state, "Sachsen[^-]|( |^)SN( |$)"), ## FIXME: CHECK! (works but see Issue #1 below)
    geo_sachsenan = match.helper(geo_state, "Sachsen-Anhalt|( |^)ST( |$)"),
    geo_sh = match.helper(geo_state, "Schleswig-Holstein|( |^)SH( |$)"),
    geo_thueringen = match.helper(geo_state, "Thueringen|( |^)TH( |$)"),
  )


### Report Geodata ---------------------------------------------------------------

## Add rowsum
dat <- dat %>% 
  rowwise() %>% 
  mutate(
    geo_sum = sum(geo_bw,geo_bayern,geo_berlin,geo_brandenburg,geo_bremen,
                  geo_hamburg,geo_hessen,geo_mv,geo_nds,geo_nrw,geo_rlp,geo_saarland,
                  geo_sachsen,geo_sachsenan,geo_sh,geo_thueringen)
  ) %>% ungroup()


## Report state overlap
dat %>% count(geo_sum)


### Bug Reports
## FIXME Issue #1: "Sachsen[^-]" in geo_name_state overmatches for punctuation (for some reason)
# -> but does not match "Sachsen-Anhalt" anymore

# dat %>% filter(str_detect(location_clean, "Sachsen")) %>% View()
## Possible solution: negative lookahead?!


## SOLVED Issue #2: ignore case for geo_state but not for dichotomisation (otherwise the ISO Codes would overmatch)
# dat %>% filter(str_detect(location_clean, "SAARLAND")) %>% View()
## Solution: Added brackets around the ISO Codes to prevent them from overmatching -> turn on ignore_case
