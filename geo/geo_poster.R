
## 1. Preprocessing
source("6-Analyses/Geotagging/1-geo_cleaning.R")

geo_tokens_tag <- geo_tokens

# FIXME: runs on tokens, but only needs the full var!!
# -> change processing order? or ignore because fast enough?!
source("6-Analyses/Geotagging/3-geo_states.R")
# - first write to geo_state vars


# 4. Tag Cities + postal codes!
source("6-Analyses/Geotagging/4-geo_cities.R")

# Combine
# nest tokens
geo_tokens_nested <- geo_tokens_tag %>%
  nest(geo_data = c(user_location_tokens, geo_state_city))

geo_prep <- geo_tokens_nested %>%
  hoist(geo_data, geo_state_city = "geo_state_city")

geo_final <- geo_prep %>%
  rowwise() %>% # IMPORTANT!
  mutate(
    geo_bw = geo_bw + ("Baden-Wuerttemberg" %in% geo_state_city),
    geo_by = geo_by + ("Bayern" %in% geo_state_city),
    geo_be = geo_be + ("Berlin" %in% geo_state_city),
    geo_bb = geo_bb + ("Brandenburg" %in% geo_state_city),
    geo_hb = geo_hb + ("Bremen" %in% geo_state_city),
    geo_hh = geo_hh + ("Hamburg" %in% geo_state_city),
    geo_he = geo_he + ("Hessen" %in% geo_state_city),
    geo_mv = geo_mv + ("Mecklenburg-Vorpommern" %in% geo_state_city),
    geo_nds = geo_nds + ("Niedersachsen" %in% geo_state_city),
    geo_nrw = geo_nrw + ("Nordrhein-Westfalen" %in% geo_state_city),
    geo_rlp = geo_rlp + ("Rheinland-Pfalz" %in% geo_state_city),
    geo_sl = geo_sl + ("Saarland" %in% geo_state_city),
    geo_sn = geo_sn + ("Sachsen" %in% geo_state_city),
    geo_st = geo_st + ("Sachsen-Anhalt" %in% geo_state_city),
    geo_sh = geo_sh + ("Schleswig-Holstein" %in% geo_state_city),
    geo_th = geo_th + ("Thueringen" %in% geo_state_city),
  )


# make booleans again!

geo_final <- geo_final %>%
  mutate(
    across(
      .cols = c(
        geo_bw, geo_by, geo_be, geo_bb, geo_hb, geo_hh, geo_he,
        geo_mv, geo_nds, geo_nrw, geo_rlp, geo_sl, geo_sn, geo_st, geo_sh, geo_th
      ),
      function (x) {
        if (x > 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    )
  )
