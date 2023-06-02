library(tidyverse)
library(tidygraph)

source('6-Analyses/COP/utils.R')

d <- readRDS('final_sample_NOV.rds') %>%
  filter(is_twlz)

# Run LIWC and topics once

# LIWC export
# sink('6-Analyses/COP/liwc-in.txt')
# for (text in d$text)
#   cat(text, '\n')
# sink()

# Combining tweets into conversations for topic modeling
# Sample convs with sparking tweet by non-bot and at least 50 words total.
# sparked_by <- d %>%
#   filter(is_head) %>%
#   mutate(sparked_by_teacher = is_teacher) %>%
#   select(conversation_id, sparked_by_teacher)
#
# d <- d %>%
#   left_join(sparked_by, by = 'conversation_id')

# # Teacher converations
# tmp <- d %>%
#   filter(is_teacher) %>%
#   filter(!is_bot) %>%
#   group_by(conversation_id) %>%
#   summarize(text = paste0(text, collapse = " ")) %>%
#   ungroup() %>%
#   mutate(n_words = str_count(text, "\\S+")) %>%
#   filter(n_words >= 50)
#
# sink('6-Analyses/COP/non-teacher.txt')
# for (text in tmp$text)
#   cat(text, '\n')
# sink()
#
# # non-Teacher-sparked converations
# tmp <- d %>%
#   filter(!is_teacher) %>%
#   filter(!is_bot) %>%
#   group_by(conversation_id) %>%
#   summarize(text = paste0(text, collapse = " ")) %>%
#   ungroup() %>%
#   mutate(n_words = str_count(text, "\\S+")) %>%
#   filter(n_words >= 50)
#
# sink('6-Analyses/COP/teacher.txt')
# for (text in tmp$text)
#   cat(text, '\n')
# sink()

# $ python3 6-Analyses/COP/COP.py
# $ python3 6-Analyses/COP/TOPIC.py

# Debug

DEBUG <- FALSE
if (DEBUG) {d <- d %>% sample_n(10000)}

# Add membership and transaction variables
d <- d %>%
  add_transaction_variables()

d <- d %>%
  add_member_group(n_interactions_for_membership = 2)

d <- d %>%
  add_membership_exit_variables(exit_quantile = 0.9)

d <- d %>%
  add_centrality_measures()


saveRDS(d, 'd_main_cop.rds')

d <- readRDS('d_main_cop.rds')

# Get transactions

transactions <- d %>%
  get_user_class_transaction()

saveRDS(transactions, 'transactions.rds')

transactions <- readRDS('transactions.rds')

get_chisq <- function(v1, v2) {
  # Calculate the chi-square
  Xsq <- chisq.test(v1, v2)
  cv <- round(as.numeric(sqrt(Xsq$statistic/(min(c(length(v1), length(v2))) - 1))), 4)
  return(list(chi = Xsq, cv = cv))
}

# Community: User-type transactions
xtabs(~is_teacher_from+is_teacher_to, transactions)
get_chisq(transactions$is_teacher_from, transactions$is_teacher_to)

xtabs(~twlz_old_member_from+twlz_old_member_to, transactions)
get_chisq(transactions$twlz_old_member_from, transactions$twlz_old_member_to)

xtabs(~top5perc_betweenness_from+top5perc_betweenness_to, transactions)
get_chisq(transactions$top5perc_betweenness_from, transactions$top5perc_betweenness_to)

# Community, are top Betweenness more teachers?
d_user <- d %>% distinct(user_id, .keep_all=TRUE)

xtabs(~top5perc_betweenness+is_teacher, d_user)
chisq.test(d_user$top5perc_betweenness, d_user$is_teacher)

# Community: Are older users more teachers?
xtabs(~twlz_old_member+is_teacher, d_user)
chisq.test(d_user$twlz_old_member, d_user$is_teacher)

# ------------------------------------------------------------------------------

# Practice

liwc <- read_csv('6-Analyses/COP/liwc-res.csv')

d <- cbind(d, liwc)

my_se <- function(v) sd(v, na.rm = TRUE)/length(v)

d %>%
  group_by(is_teacher) %>%
  summarize(
    m_tentat = mean(Tentat),
    se_tentat = my_se(Tentat),
    m_certain = mean(Certain),
    se_certain = my_se(Certain),
    m_social = mean(Social),
    se_social = my_se(Social)
  ) %>%
  ungroup()

d_plot <- d %>%
  group_by(is_teacher, year) %>%
  summarize(
    tentat_mean = mean(Tentat),
    tentat_se = my_se(Tentat),
    certain_mean = mean(Certain),
    certain_se = my_se(Certain),
    social_mean = mean(Social)/10,
    social_se = my_se(Social)/10
  ) %>%
  ungroup() %>%
  pivot_longer(matches('_mean|_se')) %>%
  separate(name, sep='_', into = c('var', 'metric')) %>%
  pivot_wider(names_from = metric, values_from = value)

d_plot2 <- d_plot %>%
  mutate(is_teacher = ifelse(is_teacher, "Teachers", "Non-Teachers")) %>%
  mutate(var = ifelse(var == 'tentat', 'Tentativeness', ifelse(var == "certain", "Certainty", "Social")))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(d_plot2, aes(x=year, y=mean, group=var, color=var)) +
  geom_line(size=2.5)+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5, position=position_dodge(0.05), size=2) +
  #geom_bar() +
  papaja::theme_apa() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x='', y='Average Tweet Score', color='LIWC-Dimension') +
  theme(legend.position = "top") +
  theme(text = element_text(size=28)) +
  scale_colour_manual(values=cbbPalette) +
  facet_wrap(~is_teacher, ncol = 2)

# Practice: LDA Topics
dat <- read_csv('6-Analyses/COP/teacher-topics-t3-w10.csv')
dat %>% as.data.frame()

dat <- read_csv('6-Analyses/COP/non-teacher-topics-t3-w10.csv')
dat %>% as.data.frame()
