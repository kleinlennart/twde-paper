
library(tidyverse)

# TWLZ sampling frame
TWLZ_hashtags <- c(
  "#lehrerzimmer",
  "#tlz",
  "#twitterkollegium",
  "#twitterlehrerzimmer",
  "#twitterlz",
  "#twlz"
)

chat_hashtags <- c(
  "#edchatde"
)

# FIXME: requires dat to have a hashtag variable pregenerated! (only works on cleaned data)
create_twlz_desc_split <- function(dat, reference) {
  hashtags <- reference

  results <- list() # FIXME: dont grow objects?
  for (hash in hashtags) {
    cat("\n", "Reading in:", hash)
    selector <- map_lgl(dat$hashtags, ~ .x %>%
      unlist() %>%
      is.element(hash) %>%
      base::any())
    results[[hash]] <- dat %>% filter(selector)
  }

  return(results)
}

create_twlz_desc <- function(results) {
  table <- data.frame(
    N_tweets = map_dbl(results, function(x) {
      x %>%
        filter(is_original) %>%
        nrow()
    }),
    N_tweets_teachers = map_dbl(results, function(x) {
      x %>%
        filter(is_original) %>%
        filter(is_teacher == 1) %>%
        nrow()
    }),
    N_unique_users = map_dbl(results, function(x) {
      x %>%
        filter(is_original) %>%
        pull(user_id) %>%
        unique() %>%
        length()
    }),
    N_unique_teachers = map_dbl(results, function(x) {
      x %>%
        filter(is_original) %>%
        filter(is_teacher == 1) %>%
        pull(user_id) %>%
        unique() %>%
        length()
    })
  ) %>%
    rownames_to_column("Hashtag")

  all_twlz <- bind_rows(results) %>%
    distinct(status_id, .keep_all = TRUE) %>%
    filter(is_original)

  table <- bind_rows(tibble(
    Hashtag = "TWLZ",
    N_tweets = all_twlz %>% nrow(),
    N_tweets_teachers = all_twlz %>% filter(is_teacher == 1) %>% nrow(),
    N_unique_users = all_twlz %>% pull(user_id) %>% unique() %>% length(),
    N_unique_teachers = all_twlz %>% filter(is_teacher == 1) %>% pull(user_id) %>% unique() %>% length()
  ), table)

  table$tweets_per_user <- round(table$N_tweets / table$N_unique_users, 2)
  table$tweets_per_teacher <- round(table$N_tweets_teachers / table$N_unique_teachers, 2)

  return(table)
}


# Run ---------------------------------------------------------------------

cat("\nReading dataset...")
dat <- readRDS(here::here("4-Data_Exports", "output" , "rehm_final.rds"))

# test <- dat %>% sample_n(10000)

#### TWLZ ####
twlz_split <- create_twlz_desc_split(dat = dat, reference = TWLZ_hashtags)
saveRDS(twlz_split, here::here("5-Descriptives", "output", "twlz_split.rds"))

twlz_table <- create_twlz_desc(twlz_split)
saveRDS(twlz_table, here::here("5-Descriptives", "output", "twlz_table.rds"))

#### Chats ####
chats_split <- create_twlz_desc_split(dat = dat, reference = chat_hashtags)
saveRDS(chats_split, here::here("5-Descriptives", "output", "chats_split.rds"))

chats_table <- create_twlz_desc(twlz_split)
saveRDS(chats_table, here::here("5-Descriptives", "output", "chats_table.rds"))

