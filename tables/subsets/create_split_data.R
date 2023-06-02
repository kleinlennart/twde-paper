library(tidyverse)

# Split all hashtags -------------------------------------------------------------------------

raw <- readRDS("data/final_2023_FEB.rds") # recoded bots!

dat <- dat %>% mutate(
  is_repost = is_retweet | is_quote
)

dat <- dat %>%
  mutate(
    user_class = case_when(
      is_bot ~ "Bot",
      is_teacher ~ "Teacher",
      !is_bot & !is_teacher ~ "Other",
      TRUE ~ NA_character_
    )
  )


####
hashtags_screened <- read_csv("reports/screened_hashtags.csv")
hashtags_sample <- hashtags_screened %>% pull(hashtag)

hashtags_screened <- hashtags_screened %>%
  mutate(hashtag = hashtag %>% str_remove_all("#"))

# Reduce payload
dat <- raw %>%
  select(
    text, lang, created_at, conversation_id, status_id, in_reply_to_user_id, source, user_id, retweet_count, reply_count, like_count, quote_count, user_description, user_created_at, user_location, user_followers_count, user_following_count, user_total_tweet_count, user_listed_count, replied_conversation_id, quoted_conversation_id, replied_status_id, quoted_status_id, replied_user_id, quoted_user_id, is_conversation, retweeted_user_id, retweeted_conversation_id, retweeted_status_id, is_retweet, is_quote, is_reply, is_original, is_head, hashtags, n_hashtags, mentions, mentions_count, urls, n_urls, year, year_month, date_created, user_tweet_count, user_lifespan_days, user_tweets_per_day, user_tweet_activity_skew, hashtags_sampled, has_sampled_hashtag, spark_hashtags, all_hashtags, in_sample, is_bot, is_teacher, is_twlz, is_chat, is_state, is_subject, sws_binary, sws_trinary, senti_final, pass_screening, all_hashtags_str, sample_hashtags, sample_hashtags_str, repost_count, created_at_DE, hour, month, weekday, conv_tweet_count, conv_user_count, conv_like_count, conv_repost_count, conv_mentions_count, conv_time_duration, conv_time_halflife, conv_time_first_response, conv_mean_responsivity, is_real_conversation, tweet_type, conv_mean_likes, conv_mean_mentions, conv_mean_reposts,
    contains("conv_"),
    conv_is_real = is_real_conversation, senti_final, contains("sws_"), is_repost
  )

# saveRDS(dat, "data/final_sample_FEB_small.rds")
# dat <- readRDS("data/final_sample_FEB_small.rds")

# generate new detection vars!!
# dat <- dat %>% mutate(
#   all_hashtags_str = all_hashtags %>% map_chr(paste, collapse = " "),
#   sample_hashtags = all_hashtags %>% map(~ .x[.x %in% hashtags_sample]),
#   sample_hashtags_str = sample_hashtags %>% map_chr(paste, collapse = " ")
# )

### Generate Hashtag Tokens ###

# important to take care of cooccurences, tweet is added to every community (token)
tokens <- dat %>% tidytext::unnest_tokens(
  input = "sample_hashtags_str",
  output = "sample_hashtag_token",
  drop = FALSE,
  token = "tweets",
  strip_punct = FALSE
)

grouped <- tokens %>%
  group_by(sample_hashtag_token)

keys <- grouped %>%
  group_keys() %>%
  pull("sample_hashtag_token") %>% # get first col as vector
  str_remove_all("#") # hashtags are not allowed in list names!

# split into hashtag slices
split_data <- grouped %>% group_split()

# add hashtag names to each list element
split_data <- split_data %>% set_names(keys)

saveRDS(split_data, "data/split_data.rds")
# saveRDS(split_dat, "data/test_split_data.rds")

# States Split ------------------------------------------------------------------

# split_data <- readRDS("data/split_data.rds")

### Subfilter ###

states <- hashtags_screened %>%
  filter(is_state) %>%
  select(hashtag, state)

missing_states <- tibble(state = setdiff(
  c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH"),
  states$state
))

states <- bind_rows(states, missing_states)

states_grouped <- states %>%
  arrange(state) %>%
  group_by(state)

state_keys <- states_grouped %>%
  group_keys() %>%
  pull(1)

state_hashs <- states_grouped %>%
  group_map(~ .x$hashtag) %>%
  set_names(state_keys)

# FIXME: all split data!
state_split <- state_hashs %>% map(~ split_data[.x])

saveRDS(state_split, "data/state_split.rds")


# Combine all states ------------------------------------------------------

# state_sample <- split_data[states$hashtag]
# all_states <- state_sample %>%
#   plyr::rbind.fill() %>%
#   distinct(status_id, .keep_all = TRUE)

# is_state = map_lgl(all_hashtags, ~ .x %in% states %>% any()),
all_states <- dat %>% filter(is_state)
saveRDS(all_states, "data/all_states.rds")

# Subjects ----------------------------------------------------------------

### Subfilter ###

subjects <- hashtags_screened %>%
  filter(is_subject) %>%
  select(hashtag, subject)

# add missing?!
# pre_fach = [ "bio", "biologie", "chemie", "chinesisch", "deutsch", "englisch",
#              "erdkunde", "erziehungswissenschaft", "ethik", "evangelisch",
#              "franzoesisch", "gemeinschaftskunde", "geo", "geographie", "geologie",
#              "geschichte", "geschichts", "griechisch", "hebraeisch", "informatik",
#              "islamisch", "italienisch", "japanisch", "juedisch", "katholisch",
#              "kunst", "latein", "lateinisch", "literatur", "mathe", "mathematik",
#              "medienbildung", "musik", "neugriechisch", "niederlaendisch",
#              "paeda", "paedagogik", "philo", "philosophie", "physik", "politik",
#              "polnisch", "portugiesisch", "psychologie", "recht", "reli",
#              "religion", "religions", "religionslehre", "russisch", "sowi",
#              "sozialkunde", "sozialwissenschaften", "spanisch", "sport", "technik",
#              "tschechisch", "tuerkisch", "wirtschaft", "wirtschafts"
# ]
# missing_states <- tibble(state = setdiff(c("BW","BY","BE","BB","HB","HH","HE","MV","NI","NW","RP","SL","SN","ST","SH","TH"),
# states$state))

# states <- bind_rows(states, missing_states)

subjects_grouped <- subjects %>%
  arrange(subject) %>%
  group_by(subject)

subject_keys <- subjects_grouped %>%
  group_keys() %>%
  pull(1)

subject_hashs <- subjects_grouped %>%
  group_map(~ .x$hashtag) %>%
  set_names(subject_keys)

# FIXME: all split data!
subject_split <- subject_hashs %>% map(~ split_data[.x])

saveRDS(subject_split, "data/subject_split.rds")

### All Subjects combined ###

# all_subjects <- split_data[subjects$hashtag] %>%
#   plyr::rbind.fill() %>%
#   distinct(status_id, .keep_all = TRUE)

all_subjects <- dat %>% filter(is_subject)
saveRDS(all_subjects, "data/all_subjects.rds")

# Chats -------------------------------------------------------------------

### Subfilter ###
#
# chats <- hashtags_screened %>%
#   filter(is_chat) %>%
#   select(hashtag, community)
#
# subjects_grouped <- subjects %>%
#   arrange(subject) %>%
#   group_by(subject)
#
# subject_keys <- subjects_grouped %>%
#   group_keys() %>%
#   pull(1)
#
# subject_hashs <- subjects_grouped %>%
#   group_map(~ .x$hashtag) %>%
#   set_names(subject_keys)
#
# # FIXME: all split data!
# subject_split <- subject_hashs %>% map(~ split_data[.x])
#
#
# saveRDS(subject_split, "data/subject_split.rds")

### All Subjects combined ###

all_chats <- dat %>% filter(is_chat)
saveRDS(all_chats, "data/all_chats.rds")

# TWLZ --------------------------------------------------------------------

### Subfilter ###

TWLZ <- hashtags_screened %>%
  filter(is_twlz) %>%
  arrange(hashtag) %>%
  select(hashtag, community)

# states <- bind_rows(states, missing_states)

# TWLZ_grouped <- TWLZ %>%
#   group_by(community)

TWLZ_hashs <- TWLZ %>%
  group_map(~ .x$hashtag) %>%
  set_names("TWLZ")

# FIXME: all split data!
TWLZ_split <- TWLZ_hashs %>% map(~ split_data[.x])

saveRDS(TWLZ_split, "data/TWLZ_split.rds")

### All TWLZ ###

all_TWLZ <- dat %>% filter(is_twlz)
saveRDS(all_TWLZ, "data/all_TWLZ.rds")


# Combine -----------------------------------------------------------------

all_split <- list(
  TWLZ = all_TWLZ,
  Chats = all_chats,
  Subjects = all_subjects,
  States = all_states,
  Dataset = dat
)

saveRDS(all_split, "data/split_all_dat.rds")
