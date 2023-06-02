library(tidyverse)

raw <- readRDS("data/added_vars_final_FIX.rds") # with new fixed conversation stats
dat <- raw

# Check User Classifications ----------------------------------------------

dat %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(is_bot, is_teacher)
# is_bot is_teacher      n
# <lgl>  <lgl>       <int>
# 1 FALSE  FALSE      132288
# 2 FALSE  TRUE        10513

# 3 TRUE   FALSE         188 # CHECK
# 4 TRUE   TRUE           15 # CHECK
## Problem: non-exclusive categories! (bot retweets as teachers?)


# is_bot & is_teacher -----------------------------------------------------------
# -> Human screening confirmed that these are all bots!

dat %>% distinct(user_id, .keep_all = TRUE) %>%
  filter(is_bot) %>%
  filter(is_teacher) %>%
  pull(user_id) %>% dput()

teacher_bots <- c("33407584", "38098692", "18693807", "34772822", "39432955",
  "26466324", "22271791", "5183461", "19770912", "35247558", "43999013",
  "35366536", "41902780", "43579693", "36611253")


dat[dat$user_id %in% teacher_bots, "is_teacher"] <- FALSE
dat[dat$user_id %in% teacher_bots, "is_bot"] <- TRUE

# is_bot & non-teacher  -------------------------------------------------------
# N = 188
dat %>% distinct(user_id, .keep_all = TRUE) %>%
  filter(is_bot) %>%
  filter(!is_teacher) %>%
  pull(user_id) %>% dput()

# Human Coding: mostly bots, some recodings:

not_teacher_not_bot <- c("12079142", "12544636", "14986623", "1740049", "20613311", "2398925", "36057972",
                         "6551325", "40829431", "42258827", "29943400", "32551147")

dat[dat$user_id %in% not_teacher_not_bot, "is_teacher"] <- FALSE
dat[dat$user_id %in% not_teacher_not_bot, "is_bot"] <- FALSE

teacher_not_bot <- c("1226293", "1487337", "43441849", "44743819")

dat[dat$user_id %in% teacher_not_bot, "is_teacher"] <- TRUE
dat[dat$user_id %in% teacher_not_bot, "is_bot"] <- FALSE


# Final Check -------------------------------------------------------------------

dat %>%
  distinct(user_id, .keep_all = TRUE) %>%
  count(is_bot, is_teacher)
# is_bot is_teacher      n
# <lgl>  <lgl>       <int>
# 1 FALSE  FALSE      132300
# 2 FALSE  TRUE        10517
# 3 TRUE   FALSE         187


# Save --------------------------------------------------------------------

saveRDS(dat, "data/final_2023_FEB.rds")

