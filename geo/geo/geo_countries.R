#### Geocode Countries ####

library(tidyverse)
library(tidygeocoder)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")
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

## Errors:
# México -> Mxico

## This function is like mutate but only acts on the rows satisfying the condition
# Needed to tag specific tweets in dataset
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#### Tag Countries -----------------------------------------------------------------
## Add tagging variable
dat$geo_country <- ""

#### Germany ####

## Regex String for non-greedy matching of keywords
germany_short_regex <- c("BRD", "DEU", "GER") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

## Need to check for missing values otherwise throws error!
dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(germany_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(germany_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany"))

# Ignore_case FALSE for "DE" because of Spanish cities like "Abc de Xyz"
dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex("(\\W|^)DE(\\W|$)", ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex("(\\W|^)DE(\\W|$)", ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Germany"))


## For longer Keywords
germany_regex <- c("Deutsch", "Deutschland", "German", "Germany", "Schland", "Alemania") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(germany_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(germany_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany"))


## Tag by German cities
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


dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
                str_detect(location_clean, regex(german_cities_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Germany"))
 



#### Austria ####

## Regex String for non-greedy matching of keywords
austria_short_regex <- c("AUT", "AT") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

## ignore_case False here because "at" in English
dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(austria_short_regex, ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(austria_short_regex, ignore_case = FALSE)), geo_country = paste0(geo_country, ", ", "Austria"))

## For longer Keywords
austria_regex <- c("Austria", "Österreich", "oesterreich", "Austrian") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(austria_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(austria_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria"))

## By Austrian Regions
austria_region_regex <- c(
  "Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich",
  "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien"
) %>%
  str_replace_all("ä", "ae") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ß", "ss") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(austria_region_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(austria_region_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria"))


## By Austrian Main Cities
austria_city_regex <- c(
  "Eisenstadt", "Klagenfurt", "Wörthersee", "St. Pölten", "Linz",
  "Salzburg", "Graz", "Innsbruck", "Bregenz", "Wien"
) %>%
  str_replace_all("ä", "ae") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ß", "ss") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(austria_city_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(austria_city_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Austria"))



#### Switzerland ####

## Regex String for non-greedy matching of keywords
swiss_short_regex <- c("CHE", "CH") %>%
  paste(collapse = "(\\W|$)|(\\W|^)") %>%
  paste0("(\\W|^)", ., "(\\W|$)")

## TODO: Check for overmatching (e.g. CHE-Ranking?!)
dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(swiss_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(swiss_short_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland"))

## For longer Keywords
siwss_regex <- c("Schweiz", "Swiss", "Switzerland", "Suisse", "Svizzera") %>%
  paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(siwss_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(siwss_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland"))


## By Swiss Kantone
swiss_region_regex <- c(
  "Zürich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden", "Glarus",
  "Zug", "Freiburg", "Solothurn", "Basel-Stadt", "Basel-Landschaft", "Schaffhausen",
  "Appenzell Ausserrhoden", "Appenzell Innerrhoden", "St. Gallen", "Graubünden", "Aargau",
  "Thurgau", "Tessin", "Waadt", "Wallis", "Neuenburg", "Genf", "Jura"
) %>%
  str_replace_all("ä", "ae") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ß", "ss") %>%
  paste(collapse = "|")

# Only on location to prevent overmatchuing (e.g. "Jura")
dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(swiss_region_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Switzerland"))


## Clean Regex Strings
# rm(list = ls(pattern = "_regex"))


### By Flags ###
## Read in Flags
flags <- read_sheet(
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
  mutate_cond(!is.na(location) &
    str_detect(location, other_flags_regex), geo_country = paste0(geo_country, ", ", "Other")) %>%
  mutate_cond(!is.na(description) &
    str_detect(description, other_flags_regex), geo_country = paste0(geo_country, ", ", "Other"))

## Germany
dat <- dat %>%
  mutate_cond(!is.na(location) &
    str_detect(location, "🇩🇪"), geo_country = paste0(geo_country, ", ", "Germany")) %>%
  mutate_cond(!is.na(description) &
    str_detect(description, "🇩🇪"), geo_country = paste0(geo_country, ", ", "Germany"))

## Switzerland
dat <- dat %>%
  mutate_cond(!is.na(location) &
    str_detect(location, "🇨🇭"), geo_country = paste0(geo_country, ", ", "Switzerland")) %>%
  mutate_cond(!is.na(description) &
    str_detect(description, "🇨🇭"), geo_country = paste0(geo_country, ", ", "Switzerland"))

## Austria
dat <- dat %>%
  mutate_cond(!is.na(location) &
    str_detect(location, "🇦🇹"), geo_country = paste0(geo_country, ", ", "Austria")) %>%
  mutate_cond(!is.na(description) &
    str_detect(description, "🇦🇹"), geo_country = paste0(geo_country, ", ", "Austria"))



### Tag Other countries by country name

states <- read_sheet("https://docs.google.com/spreadsheets/d/1fBqwbwJARpPYbkhZ63c3aTg3L4U3WbWlk5uxylxLhKM/edit#gid=1687424970", "Staaten") %>%
  pull("Countries_DE")

states_regex <- states %>% paste(collapse = "|")

dat <- dat %>%
  mutate_cond(!is.na(location_clean) &
    str_detect(location_clean, regex(states_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Other")) %>%
  mutate_cond(!is.na(description_clean) &
    str_detect(description_clean, regex(states_regex, ignore_case = TRUE)), geo_country = paste0(geo_country, ", ", "Other"))


#### Export States --------------------------------------------------------

# Clean geo_country
dat <- dat %>% 
  mutate(
    geo_country = geo_country %>% na_if("") %>% trimws(whitespace = "[ ,]")
  )


dat %>% saveRDS("data/output/user_countries.rds")


