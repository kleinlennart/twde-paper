geo %>% names()

geo$user_id %>% duplicated() %>% sum()

check <- geo %>%
  select(matches("geo_"), -geo_data, -geo_state_city, -geo_state_postal) %>%
  mutate(
    any_state = rowSums(select(., everything()))
  )

check %>%
  count(any_state)


saveRDS(geo_final, "geo/data/geo_final.rds")

geo %>% nrow()


47308/ 91369

# Without bots?
