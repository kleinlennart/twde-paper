## Check non-log scale transformation

year_labels <- as.character(seq(2004, 2022, 1))
year_labels[c(FALSE, TRUE)] <- ""

plot_log_tweets <- time_stat_tweets %>%
  ggplot(aes(x = year, y = n, color = community)) +
  geom_line(linewidth = 0.75) +
  # scale_color_viridis_d(pal) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    limits = c(2006, 2022),
    breaks = seq(2004, 2022, 1),
    labels = year_labels
  ) +
  # scale_y_log10(
  #   breaks = scales::breaks_log(6),
  #   labels = scales::label_log()
  # ) +
  labs(y = "Non-Teacher Tweets") +
  papaja::theme_apa(box = TRUE) +
  theme(axis.title.y = element_text(margin = margin(r = 3, unit = "pt"))) +
  theme(axis.title.x = element_blank()) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    legend.margin = margin(t = -2, unit = "pt")
  )

ggview::ggview(height = 4, width = 6)
ggsave(filename = here::here("plots", "nonlog_tweets.png"), height = 4, width = 6)
