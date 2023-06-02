
#
# old_report <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1n_9ZDM8sAQwECsvzZ-LYzqBN9xlx_QOLNmqqYXUnZfg/edit#gid=877796910",
#   sheet = "REPORT-DONE"
# ) %>% rename(hashtag_old = hashtag_new)
#
# # from lang removal report csv
# new_report <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1q-kQA7c8lfJt9ITvfKO1LM4sAVfOhY6lZkS0Lir8Lr4/edit#gid=0",
#   sheet = "lang"
# )
#
# new_hashtags <- new_report %>%
#   filter(in_dataset & passed_lang) %>%
#   select(hashtag_new = hashtag)
#
# # add old info to new hashtags in dataset
# merge_report <- left_join(new_hashtags, old_report,
#   by = c("hashtag_new" = "hashtag_old"), keep = TRUE)
#
#
# googlesheets4::write_sheet(merge_report, "https://docs.google.com/spreadsheets/d/1q-kQA7c8lfJt9ITvfKO1LM4sAVfOhY6lZkS0Lir8Lr4/edit#gid=0",
#                                         sheet = "REPORT-TODO")






# Reimport after coding ---------------------------------------------------

### @Conrad Start here

hashtag_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1q-kQA7c8lfJt9ITvfKO1LM4sAVfOhY6lZkS0Lir8Lr4/edit#gid=0",
                           sheet = "REPORT-DONE")

hashtag_sample <- hashtag_list %>%
  select(hashtag = hashtag_new, not_k12, is_chat, is_twlz, is_subject, is_state, state, subject, community) %>%
  filter(!not_k12)



subjects <- hashtag_sample %>%
  mutate(is_subject = is_subject %>% unlist() %>% as.logical()) %>%
  filter(is_subject) %>%
  pull(hashtag)


states <- hashtag_sample %>%
  mutate(is_state = is_state %>% unlist() %>% as.logical()) %>%
  filter(is_state) %>%
  pull(hashtag)


chats <- hashtag_sample %>%
  mutate(is_chat = is_chat %>% unlist() %>% as.logical()) %>%
  filter(is_chat) %>%
  pull(hashtag)

TWLZ <- hashtag_sample %>%
  mutate(is_twlz = is_twlz %>% unlist() %>% as.logical()) %>%
  filter(is_twlz) %>%
  pull(hashtag)




# Old ---------------------------------------------------------------------



# Old ---------------------------------------------------------------------

cat("\nGenerating Table...")
all_hashtags <- map_dfr(split_data, function(x) {
  tibble(
    N_all_tweets = x %>% nrow(),
    N_original_tweets = x %>%
      filter(is_original) %>%
      nrow(),
    # replies
    N_replies = x %>%
      filter(is_reply) %>%
      nrow(),
    # retweets + quotes
    N_reposts = x %>% filter(is_retweet | is_quote) %>% nrow(),
    N_all_users = x %>%
      distinct(user_id) %>%
      nrow(),
    # number of users of original posts
    N_original_users = x %>%
      filter(is_original) %>%
      distinct(user_id) %>%
      nrow(),
    # number of users of replies
    N_reply_users = x %>%
      filter(is_reply) %>%
      distinct(user_id) %>%
      nrow(),
    # number of users of retweets + quotes
    N_repost_users =
      x %>%
      filter(is_retweet | is_quote) %>%
      distinct(user_id) %>%
      nrow()
  )
}) %>%
  mutate(
    .after = "N_reposts",
    ratio_post_repost = N_original_tweets / N_reposts
  ) %>%
  mutate(
    .after = "N_repost_users",
    post_per_user = N_all_tweets / N_all_users
  ) %>%
  mutate(
    .before = everything(),
    rowname = names(split_data)
  )

cat("\nExporting table...")
