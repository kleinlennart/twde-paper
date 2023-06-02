## Re-nest tokens after tagging!
geo_tokens_nested <- geo_tokens_tag %>%
  nest(geo_data = c(
    user_location_tokens,
    geo_country_flag,
    geo_country_match,
    # geo_state_match,
    # geo_state_keyword,
    geo_state_city,
    geo_state_postal
  ))

# process!

#   mutate(
#     # Don't need this! just merge the other vars!
#     text = map(location_tokens, unlist),
#     text = map_chr(text, paste, collapse = " ")
#   )




# Combine methods ---------------------------------------------------------

geo_tokens_tag %>% count(geo_state_postal)


# geo_final <- geo_tokens_nested %>% hoist("geo_data", c("geo_country_match" = "geo_country_match", "geo_country_flag" =  "geo_country_flag"))


geo_raw <- geo_tokens_nested %>%
  hoist(geo_data, geo_state_city = "geo_state_city") %>%
  hoist(geo_data, geo_state_postal = "geo_state_postal")

# FIXME: this will override the old tags!
# -> counter?! then convert to boolean again?!
# geo <- geo_raw %>%
#   rowwise() %>% # IMPORTANT!
#   mutate(
#     geo_bw = "Baden-Wuerttemberg" %in% geo_state_city,
#     geo_by = "Bayern" %in% geo_state_city,
#     geo_be = "Berlin" %in% geo_state_city,
#     geo_bb = "Brandenburg" %in% geo_state_city,
#     geo_hb = "Bremen" %in% geo_state_city,
#     geo_hh = "Hamburg" %in% geo_state_city,
#     geo_he = "Hessen" %in% geo_state_city,
#     geo_mv = "Mecklenburg-Vorpommern" %in% geo_state_city,
#     geo_nds = "Niedersachsen" %in% geo_state_city,
#     geo_nrw = "Nordrhein-Westfalen" %in% geo_state_city,
#     geo_rlp = "Rheinland-Pfalz" %in% geo_state_city,
#     geo_sl = "Saarland" %in% geo_state_city,
#     geo_sn = "Sachsen" %in% geo_state_city,
#     geo_st = "Sachsen-Anhalt" %in% geo_state_city,
#     geo_sh = "Schleswig-Holstein" %in% geo_state_city,
#     geo_th = "Thueringen" %in% geo_state_city
#   )

geo <- geo_raw %>%
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
  ) %>%
  mutate(
    geo_bw = geo_bw + ("Baden-Wuerttemberg" %in% geo_state_postal),
    geo_by = geo_by + ("Bayern" %in% geo_state_postal),
    geo_be = geo_be + ("Berlin" %in% geo_state_postal),
    geo_bb = geo_bb + ("Brandenburg" %in% geo_state_postal),
    geo_hb = geo_hb + ("Bremen" %in% geo_state_postal),
    geo_hh = geo_hh + ("Hamburg" %in% geo_state_postal),
    geo_he = geo_he + ("Hessen" %in% geo_state_postal),
    geo_mv = geo_mv + ("Mecklenburg-Vorpommern" %in% geo_state_postal),
    geo_nds = geo_nds + ("Niedersachsen" %in% geo_state_postal),
    geo_nrw = geo_nrw + ("Nordrhein-Westfalen" %in% geo_state_postal),
    geo_rlp = geo_rlp + ("Rheinland-Pfalz" %in% geo_state_postal),
    geo_sl = geo_sl + ("Saarland" %in% geo_state_postal),
    geo_sn = geo_sn + ("Sachsen" %in% geo_state_postal),
    geo_st = geo_st + ("Sachsen-Anhalt" %in% geo_state_postal),
    geo_sh = geo_sh + ("Schleswig-Holstein" %in% geo_state_postal),
    geo_th = geo_th + ("Thueringen" %in% geo_state_postal),
  ) %>%
  ungroup()


# make booleans again!

geo_final <- geo %>%
  mutate(across(.cols = c(
    geo_bw, geo_by, geo_be, geo_bb, geo_hb, geo_hh, geo_he,
    geo_mv, geo_nds, geo_nrw, geo_rlp, geo_sl, geo_sn, geo_st, geo_sh, geo_th),
    ~ if_else(.x > 0, TRUE, FALSE)
  ))

geo_final %>% glimpse()


# geo_final <- geo_tokens_nested %>% unnest_wider(geo_data)


# Edge cases to look at:
# - user 8760266 (some cities match in multiple states!!)
