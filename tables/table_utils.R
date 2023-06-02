# Formatting --------------------------------------------------------------


format_table <- function(tab) {
  cat("\nFormatting table...")
  tab <- tab %>%
    mutate_if(is_integer, ~ format(.x, nsmall = 0, big.mark = ",")) %>%
    mutate_if(is_double, ~ format(round(as.numeric(.x), 2), nsmall = 2, big.mark = ","))

  return(tab)
}

# all_tables_format <- all_tables %>% map(function(tab) {
#   tab %>%
#     mutate_if(is_integer, ~ format(.x, nsmall = 0, big.mark = ",")) %>%
#     mutate_if(is_double, ~ format(round(as.numeric(.x), 2), nsmall = 2, big.mark = ","))
# })

# tab2.3 <- tab2.3 %>%
#   rename(hashtag = Category) %>%
#   mutate(
#     hashtag = str_c("#", hashtag)) %>%
#   arrange(-N_all_tweets)


# Full Overview -----------------------------------------------------------
full_overview_table <- function(dat) {
  tab <- tibble(
    Category = dat %>% names(),

    # Tweet Stats
    N_all_tweets = dat %>% map_int(nrow),
    # N_original_tweets = dat %>% map_int(~ sum(.x$is_original)),
    N_reposts = dat %>% map_int(~ sum(.x$is_repost)),
    N_replies = dat %>% map_int(~ sum(.x$is_reply)),

    # User Stats
    N_all_users = dat %>% map_int(~ n_distinct(.x$user_id)),
    # N_original_users = dat %>% map_int(~ .x %>%
    #   filter(is_original) %>%
    #   pull(user_id) %>%
    #   n_distinct()),
    N_true_conversations = dat %>% map_int(~ .x %>%
      distinct(conversation_id, .keep_all = TRUE) %>%
      pull(conv_is_real) %>%
      sum())
  )

  tab <- tab %>% mutate(
    post_per_user = N_all_tweets / N_all_users,
    .after = N_all_users,
  )

  return(tab)
}

### OVERVIEW TABLES ----------------------------------------------------------
## With all subhashtag included (Appendix) ########
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
      ### Tweets
      N_all_tweets = tweet_split_dat[[category]] %>% map_int(nrow),
      # N_original_tweets = tweet_split_dat[[category]] %>% map_int(~ sum(.x$is_original)),
      N_reposts = tweet_split_dat[[category]] %>% map_int(~ sum(.x$is_repost)),
      N_replies = tweet_split_dat[[category]] %>% map_int(~ sum(.x$is_reply)),

      ### Users
      N_all_users = tweet_split_dat[[category]] %>% map_int(~ n_distinct(.x$user_id)),
      # N_original_users = tweet_split_dat[[category]] %>% map_int(
      #   ~ .x %>%
      #     filter(is_original) %>%
      #     pull(user_id) %>%
      #     n_distinct()
      # ),

      ### on conversation level!
      N_true_conversations = tweet_split_dat[[category]] %>% map_int(~ .x %>%
        distinct(conversation_id, .keep_all = TRUE) %>%
        pull(conv_is_real) %>%
        sum())
    ) %>%
      rownames_to_column("Hashtag") %>%
      mutate(Hashtag = str_c("#", Hashtag)) # add hashtag signs again

    ## Calc True sums for Categories
    all <- tweet_split_dat[[category]] %>%
      plyr::rbind.fill() %>%
      distinct(status_id, .keep_all = TRUE)

    descr <- bind_rows(data.frame(
      Hashtag = category,
      N_all_tweets = all %>% nrow(),
      # N_original_tweets = all %>% filter(is_original) %>% nrow(),
      N_reposts = all %>% filter(is_repost) %>% nrow(),
      N_replies = all %>% filter(is_reply) %>% nrow(),
      N_all_users = all$user_id %>% n_distinct(),
      # N_original_users = all %>% filter(is_original) %>% distinct(user_id) %>% nrow(),
      N_true_conversations = all %>% distinct(conversation_id, .keep_all = TRUE) %>%
        pull(conv_is_real) %>%
        sum()
    ), descr)

    table <- bind_rows(table, descr)
  }

  table <- table %>%
    mutate(
      post_per_user = N_all_tweets / N_all_users,
      .after = N_all_users,
    )

  return(table)
}

## Only Communities, not hashtags (Report) ###########
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

    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    descr <- bind_rows(tibble(
      Hashtag = category,
      N_all_tweets = all %>% nrow(),
      # N_original_tweets = all %>% filter(is_original) %>% nrow(),
      N_reposts = all %>% filter(is_repost) %>% nrow(),
      N_replies = all %>% filter(is_reply) %>% nrow(),
      N_all_users = all %>% distinct(user_id) %>% nrow(),
      # N_original_users = all %>% filter(is_original) %>% distinct(user_id) %>% nrow()
      # N_reply_users = all %>% filter(is_reply) %>% distinct(user_id) %>% nrow(),
      # N_repost_users = all %>% filter(is_retweet | is_quote) %>% distinct(user_id) %>% nrow()
      N_true_conversations = all %>%
        distinct(conversation_id, .keep_all = TRUE) %>%
        pull(conv_is_real) %>%
        sum()
    ))

    table <- bind_rows(table, descr)
  }

  table <- table %>%
    # mutate(
    #   .after = "N_reposts",
    #   post_repost_ratio = N_original_tweets / N_reposts
    # ) %>%
    mutate(
      .after = "N_all_users",
      post_pro_user = N_all_tweets / N_all_users
      # originals_pro_user = N_original_tweets / N_original_users,
      # replies_pro_user = N_replies / N_reply_users,
      # reposts_pro_user = N_reposts / N_repost_users
    )

  return(table)
}

#### SOCIAL STATS ------------------------------------------------------------

generate_social_table_main <- function(tweet_split_dat, teachers_only = FALSE, drop_bots = FALSE, drop_retweets = FALSE) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    # if category is null append empty row to table
    if (is.null(tweet_split_dat[[category]][[1]])) {
      table <- bind_rows(table, tibble(
        Hashtag = category
      ))
      next
    }

    # merge category
    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    if (drop_bots) {
      cat("Removing bots...")
      all <- all %>% filter(!is_bot)
    }

    if (drop_retweets) {
      cat("Removing retweets...")
      all <- all %>% filter(!is_retweet)
    }

    if (teachers_only) {
      cat("Filtering for teachers...")
      all <- all %>% filter(is_teacher)
    }

    descr <- bind_rows(tibble(
      Hashtag = category,
      M_reposts_per_tweet = all$repost_count %>% mean(na.rm = TRUE), # FIXME: exclude retweets?!
      M_q95_reposts_per_tweet = all$repost_count %>% mean(na.rm = TRUE, trim = 0.025),
      SD_reposts_per_tweet = all$repost_count %>% sd(na.rm = TRUE),
      M_likes_per_tweet = all$like_count %>% mean(na.rm = TRUE),
      M_q95_likes_per_tweet = all$like_count %>% mean(na.rm = TRUE, trim = 0.025),
      SD_likes_per_tweet = all$like_count %>% sd(na.rm = TRUE),
      M_conv_length = all %>% distinct(conversation_id, .keep_all = TRUE) %>%
        filter(conv_is_real) %>% # NOTE: important here!!
        pull(conv_tweet_count) %>%
        mean(na.rm = TRUE),
      sentiment_ratio = all %>% pull(senti_final) %>%
        table() %>%
        (function(tab) {
          tab["-1"] / tab["0"] # neg / pos/neutral
        })
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

full_overview_social_table <- function(dat) {
  tab <- tibble(
    Category = dat %>% names(),


    # M_reposts_per_tweet –
    # M_q95_reposts_per_tweet –
    # SD_reposts_per_tweet –
    # M_likes_per_tweet –
    # M_q95_likes_per_tweet –
    # SD_likes_per_tweet –
    # M_conversation_length –
    # -> average senti score!!



    # Tweet Stats
    N_all_tweets = dat %>% map_int(nrow),
    # N_original_tweets = dat %>% map_int(~ sum(.x$is_original)),
    N_reposts = dat %>% map_int(~ sum(.x$is_repost)),
    N_replies = dat %>% map_int(~ sum(.x$is_reply)),

    # User Stats
    N_all_users = dat %>% map_int(~ n_distinct(.x$user_id)),
    # N_original_users = dat %>% map_int(~ .x %>%
    #   filter(is_original) %>%
    #   pull(user_id) %>%
    #   n_distinct()),
    N_true_conversations = dat %>% map_int(~ .x %>%
      distinct(conversation_id, .keep_all = TRUE) %>%
      pull(conv_is_real) %>%
      sum())
  )

  tab <- tab %>% mutate(
    post_per_user = N_all_tweets / N_all_users,
    .after = N_all_users,
  )

  return(tab)
}


# Conversation Stats ----------------------------------------------------------

generate_conv_table_main <- function(tweet_split_dat, teachers_only = FALSE, drop_bots = FALSE, non_teachers = FALSE) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    # if category is null append empty row to table
    if (is.null(tweet_split_dat[[category]][[1]])) {
      table <- bind_rows(table, tibble(
        Hashtag = category
      ))
      next
    }

    # merge category
    all <- plyr::rbind.fill(tweet_split_dat[[category]]) %>%
      distinct(status_id, .keep_all = TRUE)

    if (drop_bots) {
      cat("Removing bots...")
      all <- all %>% filter(!is_bot)
    }

    if (teachers_only) {
      cat("Filtering for teachers...")
      all <- all %>% filter(is_teacher)
    }

    if (non_teachers) {
      cat("Filtering out teachers...")
      all <- all %>% filter(!is_teacher)
    }

    # aggregate up to conv
    conv <- all %>%
      dplyr::distinct(conversation_id, .keep_all = TRUE) %>%
      filter(conv_is_real) # NOTE

    descr <- bind_rows(tibble(
      Hashtag = category,
      # M_conv_like_count = conv %>% pull(conv_like_count) %>% mean(na.rm = TRUE),
      # M_conv_repost_count = conv %>% pull(conv_repost_count) %>% mean(na.rm = TRUE),
      # M_conv_responsivity = conv %>% pull(conv_mean_responsivity) %>% mean(na.rm = TRUE),

      ## Means
      M_conv_length = conv %>% pull(conv_tweet_count) %>% mean(na.rm = TRUE),
      M_conv_users = conv %>% pull(conv_user_count) %>% mean(na.rm = TRUE),
      M_conv_mentions_count = conv %>% pull(conv_mentions_count) %>% mean(na.rm = TRUE),
      M_conv_halflife = conv %>% pull(conv_time_halflife) %>% mean(na.rm = TRUE),

      ## SDs
      SD_conv_length = conv %>% pull(conv_tweet_count) %>% sd(na.rm = TRUE),
      SD_conv_users = conv %>% pull(conv_user_count) %>% sd(na.rm = TRUE),
      SD_conv_mentions_count = conv %>% pull(conv_mentions_count) %>% sd(na.rm = TRUE),
      SD_conv_halflife = conv %>% pull(conv_time_halflife) %>% sd(na.rm = TRUE)
    ))

    table <- bind_rows(table, descr)
  }

  return(table)
}


##########

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


# Comparison Tables -------------------------------------------------------


# embrace variable?
comparison_table <- function(tweet_split_dat) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    cat("\nProcessing", category)

    sub <- tweet_split_dat[[category]] %>%
      filter(!is_retweet) %>%
      filter(!is_bot)

    descr <- tibble(
      Category = category,
      N_tweet_teacher = sub %>% filter(is_teacher) %>% nrow(),
      M_reposts_per_tweet_teacher = sub %>% filter(is_teacher) %>% pull(repost_count) %>% mean(na.rm = TRUE),
      SD_reposts_per_tweet_teacher = sub %>% filter(is_teacher) %>% pull(repost_count) %>% sd(na.rm = TRUE),

      N_non_teacher = sub %>% filter(!is_teacher) %>% nrow(),
      M_reposts_per_tweet_non_teacher = sub %>% filter(!is_teacher) %>% pull(repost_count) %>% mean(na.rm = TRUE),
      SD_reposts_per_tweet_non_teacher = sub %>% filter(!is_teacher) %>% pull(repost_count) %>% sd(na.rm = TRUE),
    )
    table <- bind_rows(table, descr) # grow table
  }
  return(table)
}

# comparison_table_clean <- function(tweet_split_dat) {
#   table <- tibble()
#   for (category in names(tweet_split_dat)) {
#     cat("\nProcessing", category)
#
#     sub <- tweet_split_dat[[category]] %>%
#       filter(!is_retweet) %>%
#       filter(!is_bot)
#
#     descr <- tibble(
#       Category = category,
#       n1 = sub %>% filter(is_teacher) %>% nrow(),
#       mean1 = sub %>% filter(is_teacher) %>% pull(repost_count) %>% mean(na.rm = TRUE),
#       sd1 = sub %>% filter(is_teacher) %>% pull(repost_count) %>% sd(na.rm = TRUE),
#
#       n2 = sub %>% filter(!is_teacher) %>% nrow(),
#       mean2 = sub %>% filter(!is_teacher) %>% pull(repost_count) %>% mean(na.rm = TRUE),
#       sd2 = sub %>% filter(!is_teacher) %>% pull(repost_count) %>% sd(na.rm = TRUE),
#     )
#     table <- bind_rows(table, descr) # grow table
#   }
#   return(table)
# }

comparison_table_clean <- function(tweet_split_dat, var) {
  table <- tibble()
  for (category in names(tweet_split_dat)) {
    cat("\nProcessing", category)

    sub <- tweet_split_dat[[category]] %>%
      filter(!is_retweet) %>%
      filter(!is_bot)

    descr <- tibble(
      Category = category,
      n1 = sub %>% filter(is_teacher) %>% nrow(),
      mean1 = sub %>% filter(is_teacher) %>% pull({{ var }}) %>% mean(na.rm = TRUE),
      sd1 = sub %>% filter(is_teacher) %>% pull({{ var }}) %>% sd(na.rm = TRUE),

      n2 = sub %>% filter(!is_teacher) %>% nrow(),
      mean2 = sub %>% filter(!is_teacher) %>% pull({{ var }}) %>% mean(na.rm = TRUE),
      sd2 = sub %>% filter(!is_teacher) %>% pull({{ var }}) %>% sd(na.rm = TRUE),
    )
    table <- bind_rows(table, descr) # grow table
  }
  return(table)
}


# sentiment_ratios <- function(sub) {
#   sub <- sub %>% filter(!is_retweet)
#   tibble(
#     SR_total = sub %>% pull(senti_final) %>%
#       as.logical() %>% # -1 wird zu TRUE, 0 zu FALSE
#       (function(x) {
#         sum(!x) / sum(x) # pos / neg
#       }),
#     SR_teacher = sub %>% filter(is_teacher) %>%
#       pull(senti_final) %>%
#       as.logical() %>% # -1 wird zu TRUE, 0 zu FALSE
#       (function(x) {
#         sum(!x) / sum(x) # pos / neg
#       }),
#     SR_non_teacher = sub %>% filter(!is_teacher) %>% pull(senti_final) %>%
#       as.logical() %>% # -1 wird zu TRUE, 0 zu FALSE
#       (function(x) {
#         sum(!x) / sum(x) # pos / neg
#       })
#   )
# }

sentiment_ratios <- function(sub) {
  sub <- sub %>% filter(!is_retweet)
  tibble(
    SR_total = sub %>% pull(senti_final) %>%
      table() %>%
      (function(tab) {
        tab["-1"] / tab["0"] # neg / pos/neutral
      }),
    SR_teacher = sub %>% filter(is_teacher) %>%
      pull(senti_final) %>%
      table() %>%
      (function(tab) {
        tab["-1"] / tab["0"] # neg / pos/neutral
      }),
    SR_non_teacher = sub %>% filter(!is_teacher) %>%
      pull(senti_final) %>%
      table() %>%
      (function(tab) {
        tab["-1"] / tab["0"] # neg / pos/neutral
      })
  )
}


