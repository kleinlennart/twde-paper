library(tidyverse)

## Fixed the counts!

# conv <- raw %>% select(
#   status_id, user_id, repost_count, like_count, reply_count, mentions_count,
#   retweet_count, quote_count, conversation_id, is_head,
#   created_at, contains("conv_")
# )

# update <- readRDS("data/added_vars_final_UPDATE.rds")
#
# update <- update %>% select(conversation_id, contains("mean"))
#
# new_raw <- raw %>%
#   select(-conv_mean_likes, -conv_mean_reposts, -conv_mean_mentions) %>%
#   left_join(update, by = "conversation_id")

group <- test %>%
  group_by(conversation_id) %>%
  summarise(
    # conv_tweet_count = n(),
    # conv_user_count = user_id %>% unique() %>% length(),
    #
    # # SOCIAL
    # conv_like_count = like_count %>% sum(),
    # conv_repost_count = repost_count %>% sum(),
    # conv_mentions_count = mentions_count %>% sum(),

    # STATS
    conv_mean_likes = like_count %>% mean(na.rm = TRUE) %>% round(2),
    conv_mean_reposts = repost_count %>% mean(na.rm = TRUE) %>% round(2),
    conv_mean_mentions = mentions_count %>% mean(na.rm = TRUE) %>% round(2)

    # ## TIME
    # conv_time_duration = difftime(max(created_at), min(created_at), tz = "UTC", units = "secs"),
    # conv_time_halflife = conv_time_duration / 2,
    # conv_time_first_response = sort(created_at)[1:2] %>% diff(),
    # conv_mean_responsivity = sort(created_at) %>% diff() %>% mean(na.rm = TRUE)
  )


# from more_variables.R ---------------------------------------------------------------------

## FIXED halflife!

# join on all tweets with conversation ID, not just the heads
# need to run distinct before

dat <- all_split$dataset

all_conv <- dat %>% # heads should be included!
  group_by(conversation_id)

# is_thread detection
# ⇒ same user as sparking tweet for the first tweets / reply to sparking tweet (not subreplies!)

conv_stats <- all_conv %>%
  summarise(
    conv_tweet_count = n(),
    conv_user_count = user_id %>% unique() %>% length(),

    # SOCIAL
    conv_like_count = like_count %>% sum(),
    conv_repost_count = repost_count %>% sum(),
    conv_mentions_count = mentions_count %>% sum(),

    # STATS
    conv_mean_likes = like_count %>% mean(na.rm = TRUE) %>% round(2),
    conv_mean_reposts = repost_count %>% mean(na.rm = TRUE) %>% round(2),
    conv_mean_mentions = mentions_count %>% mean(na.rm = TRUE) %>% round(2),

    ## TIME
    conv_time_duration = difftime(max(created_at), min(created_at), tz = "UTC", units = "secs"),
    conv_time_halflife = difftime(median(created_at, na.rm = TRUE), min(created_at), tz = "UTC", units = "secs"),
    conv_time_first_response = sort(created_at)[1:2] %>% diff(), # FIXME: only works if first is_head!!
    conv_mean_responsivity = sort(created_at) %>% diff() %>% mean(na.rm = TRUE)

    # TODO: conv_head_in_dataset
  )


conv_stats <- all_conv %>%
  summarise(
      conv_time_halflife = difftime(median(created_at, na.rm = TRUE), min(created_at), tz = "UTC", units = "secs")
  )

# conv_time_first_response set NA for conv where is_head is not available!!

# conversation criterion
conv_stats <- conv_stats %>%
  mutate(
    # this includes the sparking tweet!! is_real_conversation
    conv_is_real = if_else(conv_tweet_count > 1 & conv_user_count > 1, TRUE, FALSE)
  )

# Join conv stats
dat <- dat %>% left_join(conv_stats, by = "conversation_id")

# Test Conv Stats ---------------------------------------------------------

test_stats <- conv %>% filter(conversation_id == "10001110")

test <- all_split$dataset %>% filter(conversation_id == "10001110")

test %>%
  select(status_id, conversation_id, is_head, starts_with("conv_")) %>%
  View()

test %>% summarise(
  conv_time_duration = difftime(max(created_at), min(created_at), tz = "UTC", units = "secs"),
  # conv_time_median = median(created_at, na.rm = TRUE),
  conv_time_halflife = difftime(median(created_at, na.rm = TRUE), min(created_at), tz = "UTC", units = "secs")
)
