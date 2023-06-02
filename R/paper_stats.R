library(tidyverse)

## Note: includes recoded bot assignments
# dat <- readRDS("data/final_2023_FEB.rds")
dat <- all_split$dataset
# User Classfiication

## Bots
dat %>% distinct(user_id, .keep_all = TRUE) %>%
  count(is_bot)

dat %>% filter(is_bot) %>% nrow()
dat %>% filter(is_bot) %>% pull(user_tweet_count) %>% sd()
dat %>% filter(is_bot) %>% count(tweet_type)

# users who posted an average of 19.XX tweets (SD = XX.XX).


dat$user_id[1]

dat %>% filter(user_id == "33407584") %>% select(is_bot, is_teacher, user_description) %>% View()

user_tweet_count <- dat %>% distinct(user_id, .keep_all = TRUE) %>% pull(user_tweet_count)
user_tweet_count %>% mean()
user_tweet_count %>% median()
user_tweet_count %>% max()
user_tweet_count %>% summary()

user_tweet_count %>% sd() %>% round(2)

# Examine largest user
dat %>% distinct(user_id, .keep_all = TRUE) %>% arrange(-user_tweet_count) %>% View()


# Results -----------------------------------------------------------------

percent_increase <- function(start, final) {
  round(((final - start) / start) * 100, 2)
}

all_TWLZ <- readRDS("data/all_TWLZ.rds")
clean <- dat %>% filter(!is_bot)
all_TWLZ_clean <- all_TWLZ %>% filter(!is_bot)

# the TWLZ community increased its user numbers from 2010 to 2022 by XX% (2010: XXX users, 2020: XXX tweets)


all_split$twlz %>%
  filter(!is_bot) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(year)

# year      n
# 1 2008      3
# 2 2009     12
# 3 2010     32
# 4 2011     71
# 5 2012     96
# 6 2013    144
# 7 2014    145
# 8 2015    151
# 9 2016    176
# 10 2017    193
# 11 2018   1196
# 12 2019   5839
# 13 2020  22524
# 14 2021  67579
percent_increase(32, 22524)


# Alongside an XX% increase in tweets (2010: XXX tweets, 2020: XXX tweets).

all_split$twlz %>%
  filter(!is_bot) %>%
  count(year)
# year       n
# 1 2008    5
# 2 2009      36
# 3 2010     111
# 4 2011     170
# 5 2012     342
# 6 2013     375
# 7 2014     572
# 8 2015     476
# 9 2016     635
# 10 2017    3373
# 11 2018   38338
# 12 2019  194047
# 13 2020  607855
# 14 2021  938134

percent_increase(111, 607855)

# Interestingly, the number of users and tweets in state-specific communities plateaued from 2011-2016 at around XX users and XX tweets
distinct(user_id, .keep)
# 1 2008       5
# 2 2009      36
# 3 2010     111
# 4 2011     170
# 5 2012     342
# 6 2013     375
# 7 2014     572
# 8 2015     476
# 9 2016     635
# 10 2017    3373
# 11 2018   38338
# 12 2019  194047
# 13 2020  607855
# 14 2021  938134

938134 / 111

# before showing a XX% increase in users and a XX% increase in tweets compared to 2017. (in 2017!!)
all_states <- readRDS("data/all_states.rds")

all_states %>%
  filter(!is_bot) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(year) %>%
  filter(as.numeric(year) < 2016) %>%
  summarise(
    mean = mean(n)
  )

all_split$states %>%
  filter(!is_bot) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(year)
## Users in States
# 1 2011     32
# 2 2012     36
# 3 2013     29
# 4 2014     22
# 5 2015     34
# 6 2016     24
# 7 2017    582
# 8 2018   1853
# 9 2019   2101
# 10 2020   6231
# 11 2021  10865
percent_increase(24, 582)


all_states %>%
  filter(!is_bot) %>%
  count(year)
# 1 2011    215
# 2 2012    149
# 3 2013    162
# 4 2014     86
# 5 2015    151
# 6 2016     76
# 7 2017  24755
# 8 2018  49352
# 9 2019  57203
# 10 2020  79473
# 11 2021  89132
percent_increase(76, 24755)
#> [1] 32472.37


# Notably, the EdChatDE community was the only community that showed a decline in the
# number of users and tweets after a peak of XX users and XX tweets in 201X.

all_edchat <- readRDS("data/all_chats.rds")

all_edchat %>%
  filter(!is_bot) %>%
  count(year) %>%
  filter(n == max(n))
# 2015  60503

all_edchat %>%
  filter(!is_bot) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(year) %>%
  filter(n == max(n))
# 1 2018   2174

