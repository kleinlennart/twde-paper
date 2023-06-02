library(tidyverse)

# split_data <- readRDS("~/Documents/HiWi Server/twde-dataset/data/test_split_data.rds")

cat("\nGenerating Table...")
all_hashtags <- map_dfr(split_data, ~
  tibble(
    N_all_tweets = .x %>% nrow(),
    N_original_tweets = .x %>%
      filter(is_original) %>%
      nrow(),
    N_all_users = .x %>%
      distinct(user_id) %>%
      nrow(),
    N_original_users = .x %>%
      filter(is_original) %>%
      distinct(user_id) %>%
      nrow()
  )) %>%
  mutate(
    .before = everything(),
    Hashtag = names(split_data) %>% str_c("#", .)
  )

source("5-Descriptives/tables/table_utils.R")

all_hashtags <- all_hashtags %>% format_table()


cat("\nExporting table...")

all_hashtags %>% clipr::write_clip()
