# Has dependencies on
#   "1000 2019-01-30 Presidential Economic Indicators.R"
#   "1010 2019-01-30 Presidential Economic Indicators.R"

## @knitr PlotBondPerfSetup
dat02 <- dat01a %>% 
  filter(Metrics == "Bond Performance")

subtitles_bond_rankings <- paste(subtitles[1:2], collapse = "\n")

## @knitr PlotBondPerfScatterPlot
p0 +
  geom_point(aes(x = Presidents, y = Values, color = Presidents), data = dat02, size = 6, show.legend = FALSE) +
  geom_line(aes(x = as.integer(Presidents), y = Values), data = dat02) +
  labs(
    title = "Bond Performance Ranks for Seven US Presidents",
    subtitle = subtitles_bond_rankings,
    caption = plot_caption
  ) +
  theme(axis.text = element_text(size = 12))

## @knitr PlotBondPerfBarChart
p0 +
  geom_col(aes(x = Presidents, y = Values, fill = reorder(Presidents, Values, sort)), dat02, show.legend = FALSE) +
  #scale_fill_brewer(type = "qual", palette = color_brewer_palette) +
  scale_fill_brewer(type = "seq", palette = "RdYlGn", direction = -1) +
  labs(
    title = "Bond Performance Ranks for Seven US Presidents",
    subtitle = subtitles_bond_rankings,
    caption = plot_caption
  ) +
  theme(axis.text = element_text(size = 12))

## @knitr PlotBondPerfBarChartCoorFlip
ggplot(dat02) +
  geom_col(
    aes(x = Presidents,
        y = Values,
        fill = reorder(Presidents, Values, sort)),
    show.legend = FALSE) +
  scale_y_continuous(breaks = 1:7) +
  scale_fill_brewer(type = "seq", palette = "RdYlGn", direction = -1) +
  theme(
    title = element_text(size = 18),
    plot.margin = margin(30, 30, 20, 30),
    plot.subtitle = element_text(size = 10, hjust = 0),
    plot.caption = element_text(size = 10, hjust = 0),
    axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.25)) +
  coord_flip() +
  labs(
    title = "Bond Performance Rankings for 7 US Presidents",
    subtitle = subtitles_bond_rankings,
    caption = plot_caption,
    x = "President",
    y = "Ranking")

## @knitr PlotOverallRankings
# First, restructure the data.
dat03 <- dat01a %>% 
  spread(key = Metrics, value = Values)

colnames(dat03)[-1] <- dat_metrics$column_names

dat03b <- dat03[, c("Presidents", "overall_rank")]

dat03b$Presidents <- reorder(dat03$Presidents, dat03$overall_rank, sort)

# View the sorted overall_rank
ggplot(dat03b, aes(fill = Presidents)) +
  geom_col(aes(x = Presidents,
               y = overall_rank),
           show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Overall Ranking of 14 Economic\nIndicators for 7 US Presidents",
    subtitle = paste(subtitles[-3], collapse = "\n"),
    y = "Overall Rank",
    x = "President",
    caption = plot_caption
  ) +
  scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  theme(
    title = element_text(size = 24),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 12, margin = margin(0, 0, 10, 0), hjust = 0),
    plot.margin = margin(20, 10, 0, 10),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    axis.text = element_text(size = 14)
  )

## @knitr TablePresSortedByOverallRank
kableExtra::kable(dat03b %>% arrange(overall_rank)) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped",
                          "condensed"),
    position = "left", full_width = FALSE)