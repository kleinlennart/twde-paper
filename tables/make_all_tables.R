library(tidyverse)

state_split <- readRDS("data/test_state_split_names.rds")

### Script for hashtag overview all tables ###

# split_all_dat <- list(
#   dataset = all,
#   twlz = all_TWLZ,
#   subjects = all_subjects,
#   states = all_states,
#   chats = all_chats
# )
#
# saveRDS(split_all_dat, "data/split_all_dat.rds")

### OVERVIEW TABLES ----------------------------------------------------------

generate_table_complete <- function(tweet_split_dat) {
  table <- data.frame()
  for (category in names(tweet_split_dat)) {
    # FIXME: add empty categories manually later on?!
    # if category is null append empty row to table
    if (is.null(tweet_split_dat[[category]][[1]])) {
      table <- bind_rows(table, data.frame(
        Hashtag = category
      ))
      next
    }

    descr <- data.frame(
      # all kinds of tweets
      N_all_tweets = map_int(tweet_split_dat[[category]], function(x) {
        x %>% nrow()
      }),

      # original posts
      N_original_tweets = map_int(tweet_split_dat[[category]], function(x) {
        x %>%
          filter(is_original) %>%
          nrow()
      }),

      # replies
      N_replies = map_int(tweet_split_dat[[category]], function(x) {
        x %>%
          filter(is_reply) %>%
          nrow()
      }),


      # retweets + quotes
      N_reposts = map_int(tweet_split_dat[[category]], function(x) {
        x %>%
          filter(is_retweet | is_quote) %>%
          nrow()
      }),
      N_all_users = map_int(tweet_split_dat[[category]], function(x) {
        x %>%
          distinct(user_id) %>%
          nrow()
      }),

      # number of users of original posts
      N_original_users = map_int(tweet_split_dat[[category]], function(x) {
        x %>%
          filter(is_original) %>%
          distinct(user_id) %>%
          nrow()
      })

      # # number of users of replies
      # N_reply_users = map_int(tweet_split_dat[[category]], function(x) {
      #   x %>%
      #     filter(is_reply) %>%
      #     distinct(user_id) %>%
      #     nrow()
      # }),

      # # number of users of retweets + quotes
      # N_repost_users = map_int(tweet_split_dat[[category]], function(x) {
      #   x %>%
      #     filter(is_retweet | is_quote) %>%
      #     distinct(user_id) %>%
      #     nrow()
      # })
    ) %>%
      rownames_to_column("Hashtag") %>%
      mutate(Hashtag = str_c("#", Hashtag))

    # add hashtag signs again



    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    descr <- bind_rows(data.frame(
      Hashtag = category,
      N_all_tweets = all %>% nrow(),
      N_original_tweets = all %>% filter(is_original) %>% nrow(),
      N_replies = all %>% filter(is_reply) %>% nrow(),
      N_reposts = all %>% filter(is_retweet | is_quote) %>% nrow(),
      N_all_users = all %>% distinct(user_id) %>% nrow(),
      N_original_users = all %>% filter(is_original) %>% distinct(user_id) %>% nrow()
      # N_reply_users = all %>% filter(is_reply) %>% distinct(user_id) %>% nrow(),
      # N_repost_users = all %>% filter(is_retweet | is_quote) %>% distinct(user_id) %>% nrow()
    ), descr)

    table <- bind_rows(table, descr)
  }

  table <- table %>%
    # mutate(
    #   .after = "N_reposts",
    #   post_repost_ratio = N_original_tweets / N_reposts
    # ) %>%
    mutate(
      .after = "N_original_users",
      post_pro_user = N_all_tweets / N_all_users
      # originals_pro_user = N_original_tweets / N_original_users,
      # replies_pro_user = N_replies / N_reply_users,
      # reposts_pro_user = N_reposts / N_repost_users
    )

  return(table)
}

generate_table_main <- function(tweet_split_dat) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    # FIXME: add empty categories manually later on?!
    # if category is null append empty row to table
    if (is.null(tweet_split_dat[[category]][[1]])) {
      table <- bind_rows(table, tibble(
        Hashtag = category
      ))
      next
    }

    # descr <- tibble(
    #   # all kinds of tweets
    #   N_all_tweets = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>% nrow()
    #   }),
    #
    #   # original posts
    #   N_original_tweets = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_original) %>%
    #       nrow()
    #   }),
    #
    #   # replies
    #   N_replies = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_reply) %>%
    #       nrow()
    #   }),
    #
    #
    #   # retweets + quotes
    #   N_reposts = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_retweet | is_quote) %>%
    #       nrow()
    #   }),
    #
    #
    #   N_all_users = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       distinct(user_id) %>%
    #       nrow()
    #   }),
    #
    #   # number of users of original posts
    #   N_original_users = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_original) %>%
    #       distinct(user_id) %>%
    #       nrow()
    #   })
    #
    #   # # number of users of replies
    #   # N_reply_users = map_int(tweet_split_dat[[category]], function(x) {
    #   #   x %>%
    #   #     filter(is_reply) %>%
    #   #     distinct(user_id) %>%
    #   #     nrow()
    #   # }),
    #
    #   # # number of users of retweets + quotes
    #   # N_repost_users = map_int(tweet_split_dat[[category]], function(x) {
    #   #   x %>%
    #   #     filter(is_retweet | is_quote) %>%
    #   #     distinct(user_id) %>%
    #   #     nrow()
    #   # })
    # ) %>% rownames_to_column("Hashtag") %>%
    #   mutate(Hashtag = str_c("#", Hashtag))


    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    descr <- bind_rows(tibble(
      Hashtag = category,
      N_all_tweets = all %>% nrow(),
      N_original_tweets = all %>% filter(is_original) %>% nrow(),
      N_replies = all %>% filter(is_reply) %>% nrow(),
      N_reposts = all %>% filter(is_retweet | is_quote) %>% nrow(),
      N_all_users = all %>% distinct(user_id) %>% nrow(),
      N_original_users = all %>% filter(is_original) %>% distinct(user_id) %>% nrow()
      # N_reply_users = all %>% filter(is_reply) %>% distinct(user_id) %>% nrow(),
      # N_repost_users = all %>% filter(is_retweet | is_quote) %>% distinct(user_id) %>% nrow()
    ))

    table <- bind_rows(table, descr)
  }

  table <- table %>%
    # mutate(
    #   .after = "N_reposts",
    #   post_repost_ratio = N_original_tweets / N_reposts
    # ) %>%
    mutate(
      .after = "N_original_users",
      post_pro_user = N_all_tweets / N_all_users
      # originals_pro_user = N_original_tweets / N_original_users,
      # replies_pro_user = N_replies / N_reply_users,
      # reposts_pro_user = N_reposts / N_repost_users
    )

  return(table)
}

##### States
state_table_complete <- generate_table_complete(state_split)
state_table_complete <- state_table_complete %>% format_table()
state_table_complete %>% clipr::write_clip()


state_table <- generate_table_main(state_split)
state_table <- state_table %>%
  drop_na(everything()) %>%
  format_table()
state_table %>% clipr::write_clip()


##### Subjects
subjects_table <- generate_table_complete(subject_split) %>% format_table()
subjects_table %>% clipr::write_clip()

subjects_table <- generate_table_main(subject_split) %>% format_table()
subjects_table %>% clipr::write_clip()

###### TWLZ
TWLZ_table <- generate_table_complete(TWLZ_split) %>% format_table()
TWLZ_table <- TWLZ_table %>% arrange(desc(N_all_tweets))

TWLZ_table %>% clipr::write_clip()

# TWLZ_table <- generate_table_main(TWLZ_split)
# TWLZ_table <- TWLZ_table %>% format_table()
# TWLZ_table %>% clipr::write_clip()


#### SOCIAL STATS ------------------------------------------------------------

generate_social_table_main <- function(tweet_split_dat) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    # FIXME: add empty categories manually later on?!
    # if category is null append empty row to table
    if (is.null(tweet_split_dat[[category]][[1]])) {
      table <- bind_rows(table, tibble(
        Hashtag = category
      ))
      next
    }

    # descr <- tibble(
    #   # all kinds of tweets
    #   N_all_tweets = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>% nrow()
    #   }),
    #
    #   # original posts
    #   N_original_tweets = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_original) %>%
    #       nrow()
    #   }),
    #
    #   # replies
    #   N_replies = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_reply) %>%
    #       nrow()
    #   }),
    #
    #
    #   # retweets + quotes
    #   N_reposts = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_retweet | is_quote) %>%
    #       nrow()
    #   }),
    #
    #
    #   N_all_users = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       distinct(user_id) %>%
    #       nrow()
    #   }),
    #
    #   # number of users of original posts
    #   N_original_users = map_int(tweet_split_dat[[category]], function(x) {
    #     x %>%
    #       filter(is_original) %>%
    #       distinct(user_id) %>%
    #       nrow()
    #   })
    #
    #   # # number of users of replies
    #   # N_reply_users = map_int(tweet_split_dat[[category]], function(x) {
    #   #   x %>%
    #   #     filter(is_reply) %>%
    #   #     distinct(user_id) %>%
    #   #     nrow()
    #   # }),
    #
    #   # # number of users of retweets + quotes
    #   # N_repost_users = map_int(tweet_split_dat[[category]], function(x) {
    #   #   x %>%
    #   #     filter(is_retweet | is_quote) %>%
    #   #     distinct(user_id) %>%
    #   #     nrow()
    #   # })
    # ) %>% rownames_to_column("Hashtag") %>%
    #   mutate(Hashtag = str_c("#", Hashtag))

    ## reposts per tweet stats

    # M_reposts_per_tweet = map_dbl(tweet_split_dat[[category]], function(x) {
    #   x %>% pull()
    # }),
    #
    # M_q95_reposts_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>%
    #     filter(is_original) %>%
    #     nrow()
    # }),
    #
    # Median_reposts_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>%
    #     filter(is_reply) %>%
    #     nrow()
    # }),
    #
    #
    # SD_reposts_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>%
    #     filter(is_retweet | is_quote) %>%
    #     nrow()
    # }),

    ## likes

    # M_likes_per_tweet = map_dbl(tweet_split_dat[[category]], function(x) {
    #   x %>% pull(like_count) %>% mean(na.rm = TRUE)
    # }),
    #
    # M_q95_likes_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>% pull(like_count) %>% mean(na.rm = TRUE, trim = 0.025)
    # }),
    #
    # Median_likes_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>% pull(like_count) %>% median(na.rm = TRUE)
    # }),
    # SD_likes_per_tweet = map_int(tweet_split_dat[[category]], function(x) {
    #   x %>% pull(like_count) %>% sd(na.rm = TRUE)
    # })
    #
    # ## conversations

    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    descr <- bind_rows(tibble(
      Hashtag = category,
      M_reposts_per_tweet = all$repost_count %>% mean(na.rm = TRUE),
      M_q95_reposts_per_tweet = all$repost_count %>% mean(na.rm = TRUE, trim = 0.025),
      SD_reposts_per_tweet = all$repost_count %>% sd(na.rm = TRUE),
      M_likes_per_tweet = all$like_count %>% mean(na.rm = TRUE),
      M_q95_likes_per_tweet = all$like_count %>% mean(na.rm = TRUE, trim = 0.025),
      SD_likes_per_tweet = all$like_count %>% sd(na.rm = TRUE)
      # (N_conversations  –)= all %>%
      # M_conversation_length  –= all %>%

      # -> average senti score!!

      ####

      # N_all_tweets = all %>% nrow(),
      # N_original_tweets = all %>% filter(is_original) %>% nrow(),
      # N_replies = all %>% filter(is_reply) %>% nrow(),
      # N_reposts = all %>% filter(is_retweet | is_quote) %>% nrow(),
      #
      # N_all_users = all %>% distinct(user_id) %>% nrow(),
      # N_original_users = all %>% filter(is_original) %>% distinct(user_id) %>% nrow()
      # # N_reply_users = all %>% filter(is_reply) %>% distinct(user_id) %>% nrow(),
      # # N_repost_users = all %>% filter(is_retweet | is_quote) %>% distinct(user_id) %>% nrow()
    ))

    table <- bind_rows(table, descr)
  }

  # table <- table %>%
  #   # mutate(
  #   #   .after = "N_reposts",
  #   #   post_repost_ratio = N_original_tweets / N_reposts
  #   # ) %>%
  #   mutate(
  #     .after = "N_original_users",
  #     post_pro_user = N_all_tweets / N_all_users
  #     # originals_pro_user = N_original_tweets / N_original_users,
  #     # replies_pro_user = N_replies / N_reply_users,
  #     # reposts_pro_user = N_reposts / N_repost_users
  #   )

  return(table)
}

##### States
# state_table_complete <- generate_table_complete(state_split)
# state_table_complete <- state_table_complete %>% format_table()
# state_table_complete %>% clipr::write_clip()


state_table <- generate_social_table_main(state_split)
state_table <- state_table %>%
  drop_na(everything()) %>%
  format_table()
state_table %>% clipr::write_clip()


##### Subjects
# subjects_table <- generate_table_complete(subject_split) %>% format_table()
# subjects_table %>% clipr::write_clip()

subjects_table <- generate_social_table_main(subject_split) %>% format_table()
subjects_table %>% clipr::write_clip()

###### TWLZ
# TWLZ_table <- generate_table_complete(TWLZ_split) %>% format_table()
# TWLZ_table <- TWLZ_table %>% arrange(desc(N_all_tweets))
#
# TWLZ_table %>% clipr::write_clip()

TWLZ_table <- generate_social_table_main(TWLZ_split[["TWLZ"]])
TWLZ_table <- TWLZ_table %>% format_table()
TWLZ_table %>% clipr::write_clip()
