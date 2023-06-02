# Hashtags ----------------------------------------------------------------

subjects <- c(
  "#spanischunterricht", "#sportunterricht", "#englischunterricht",
  "#matheunterricht", "#biologieunterricht", "#chemieunterricht",
  "#deutschunterricht", "#erdkundeunterricht", "#ethikunterricht",
  "#geographieunterricht", "#geounterricht", "#geschichtsunterricht",
  "#informatikunterricht", "#kunstunterricht", "#mathematikunterricht",
  "#musikunterricht", "#physikunterricht", "#politikunterricht",
  "#religionsunterricht", "#reliunterricht", "#wirtschaftsunterricht",
  "#englischlehrerin", "#englischlehrer", "#deutschlehrer", "#deutschlehrerin",
  "#deutschdidaktik", "#geschichtsdidaktik", "#geschichtslehrer",
  "#informatikschule", "#lateinunterricht", "#literaturdidaktik",
  "#mathelehrer", "#musiklehrer", "#pflichtfachinformatik", "#philoedu",
  "#physiklehrer", "#relichat", "#religionslehrer", "#schulewirtschaft",
  "#schulfachwirtschaft", "#sportlehrer", "#wirtschaftsschule",
  "#bioabi", "#biounterricht", "#chemielehrer", "#chinesischlernen",
  "#deutsch_lernen", "#deutschabi", "#deutschlernen", "#englisch_lernen",
  "#englischabi", "#englischlernen", "#franzoesischlernen", "#franzoesischunterricht",
  "#geographiedidaktik", "#geographielehrer", "#geschichtswettbewerb",
  "#hauptfachinformatik", "#japanischlernen", "#kunstlehrer", "#kunstschule",
  "#kunstwettbewerb", "#lateinchat", "#leitfachinformatik", "#lernendeutsch",
  "#literaturwettbewerb", "#matheabi", "#matheabitur", "#mathelernen",
  "#musikschule", "#musikwettbewerb", "#philosophieunterricht",
  "#physikedu", "#portugiesischlernen", "#religionslehrerin", "#russischlernen",
  "#spanischlehrer", "#sportdigital", "#sportschule"
)

states <- c(
  "#digitalbw", "#bayernedu", "#bildungrlp", "#bildungsh", "#bildungslandnrw",
  "#bwedu", "#edubayern", "#edubw", "#edunrw", "#edush", "#hbedu",
  "#hessenedu", "#mvedu", "#ndsedu", "#nrwedu", "#rlpbildung",
  "#rlpedu", "#schuledigitalbw", "#schuledigitalnrw", "#schulenrw",
  "#bayerndigital", "#bwdigital", "#digitalbayern", "#digitalbb",
  "#digitalberlin", "#nrw_digital", "#nrwdigital", "#sachsendigital",
  "#thueringendigital"
)


chats <- c("#edchatde")


TWLZ <- c(
  "#lehrerzimmer", "#twitterkollegium", "#twitterlehrerzimmer",
  "#twitterlz", "#twlz"
)


# Subsets -----------------------------------------------------------------

dat_raw <- readRDS("data/FINAL_23-07-2022.rds")
dat <- dat_raw

get_tweets_by_hashtag <- function(dat, hashtag_selection) {
  return(
    dat %>%
      filter(map_lgl(hashtags, ~ any(.x %in% hashtag_selection)))
  )
}

cat("\nGenerating Subsets...")

TWLZ_subset <- dat %>%
  get_tweets_by_hashtag(TWLZ)

chats_subset <- dat %>%
  get_tweets_by_hashtag(chats)

states_subset <- dat %>%
  get_tweets_by_hashtag(states)

subjects_subset <- dat %>%
  get_tweets_by_hashtag(subjects)

cat("\nRemerging Subsets...")
all_sampled <- bind_rows(TWLZ_subset, chats_subset, states_subset, subjects_subset) %>%
  distinct(status_id, .keep_all = TRUE)

# FIXME: what about all the conversations?!
# -> we didn't tag them with hashtags this time!!


# TWLZ_table <-
# TWLZ_subset %>%
#   mutate(is_repost = map2_lgl(is_retweet, is_quote, any)) %>%
#   summarise(
#     N_all_tweets = n(),
#     N_original_tweets = sum(is_original),
#     N_replies = sum(is_reply),
#     # N_reposts = sum(is_repost)
#     N_reposts = sum(is_repost),
#     N_all_users = unique(user_id) %>% length(),
#     N_original_users = filter(is_original) %>% distinct(user_id) %>% nrow()
#   )


# Table Generation --------------------------------------------------------

split_dat <- list(
  "Full Dataset" = all_sampled,
  "TWLZ" = TWLZ_subset,
  "Chats" = chats_subset,
  "States" = states_subset,
  "Subjects" = subjects_subset
)

cat("\nGenerating Table...")
all_tables <- map_dfr(split_dat, function(x) {
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
    rowname = names(split_dat)
  )


cat("\nExporting table...")
