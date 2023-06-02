#### Geo Report Tables ###

library(tidyverse)
library(googlesheets4)

## Read in Data
dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")
geo <- readRDS("data/output/user_geotagged.rds")

dat <- left_join(dat, geo, by = "user_id")

d <- list()
d$BW <- dat %>% filter(geo_bw == 1)
d$BY <- dat %>% filter(geo_bayern == 1)
d$BE <- dat %>% filter(geo_berlin == 1)
d$BB <- dat %>% filter(geo_brandenburg == 1)
d$HB <- dat %>% filter(geo_bremen == 1)
d$HH <- dat %>% filter(geo_hamburg == 1)
d$HE <- dat %>% filter(geo_hessen == 1)
d$MV <- dat %>% filter(geo_mv == 1)
d$NI <- dat %>% filter(geo_nds == 1)
d$NW <- dat %>% filter(geo_nrw == 1)
d$RP <- dat %>% filter(geo_rlp == 1)
d$SL <- dat %>% filter(geo_saarland == 1)
d$SN <- dat %>% filter(geo_sachsen == 1)
d$ST <- dat %>% filter(geo_sachsenan == 1)
d$SH <- dat %>% filter(geo_sh == 1)
d$TH <- dat %>% filter(geo_thueringen == 1)


#### Table Generation ####
table <- data.frame(
  N_tweets = map_dbl(d, function(x) {
    x %>%
      nrow()
  }),
  N_users = map_dbl(d, function(x) {
    x %>%
      pull(user_id) %>%
      unique() %>%
      length()
  })
) %>%
  rownames_to_column("Bundesland")


write_sheet(table, "https://docs.google.com/spreadsheets/d/1oywT3MdTXOkZfm1sURnWu49I35dUKJKsnsvoCUx-TS0/edit#gid=0", "Bundesland Report")



##### Report States -----------------------------------------------------------------


dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")
geo <- readRDS("data/output/user_countries.rds")

dat <- left_join(dat, geo, by = "user_id")


d <- list()
d$Germany <- dat %>% filter(str_detect(geo_country, "Germany"))
d$Austria <- dat %>% filter(str_detect(geo_country, "Austria"))
d$Switzerland <- dat %>% filter(str_detect(geo_country, "Switzerland"))
d$Other <- dat %>% filter(is.na(geo_country))


#### Table Generation ####
table <- data.frame(
  N_tweets = map_dbl(d, function(x) {
    x %>%
      nrow()
  }),
  N_users = map_dbl(d, function(x) {
    x %>%
      pull(user_id) %>%
      unique() %>%
      length()
  })
) %>%
  rownames_to_column("Country")


write_sheet(table, "https://docs.google.com/spreadsheets/d/1oywT3MdTXOkZfm1sURnWu49I35dUKJKsnsvoCUx-TS0/edit#gid=0", "Country Report")



geo %>% filter(is.na(location_clean)) %>% nrow()


