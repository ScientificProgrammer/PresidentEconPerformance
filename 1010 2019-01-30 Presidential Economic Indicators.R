# Has dependencies on
#  "1000 2019-01-30 Presidential Economic Indicators.R"

## @knitr PlotAllEconIndSetup
  p0 <- ggplot() +
    theme(
      title = element_text(size = 18),
      plot.margin = margin(30, 30, 20, 30),
      plot.subtitle = element_text(size = 10, hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0),
      axis.title.x = element_text(margin = margin(10, 0, 10, 0)),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)
    ) +
    labs(x = "President", y = "Ranking") +
    scale_y_continuous(breaks = seq.int(length(unique(dat01a$Values)))) +
    scale_color_brewer(type = "qual", palette = color_brewer_palette)


## @knitr PlotAllEconIndScatterPlot
  p0 +
    geom_point(aes(x = Presidents, y = Values, color = Presidents), data = dat01a, size = 4, show.legend = FALSE) +
    geom_line(aes(x = as.integer(Presidents), y = Values), data = dat01a) +
    facet_wrap(. ~ Metrics) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption
    )

## @knitr PlotAllEconIndBarChart
  p0 +
    geom_col(aes(x = Presidents, y = Values, fill = Presidents), data = dat01a, show.legend = FALSE) +
    #geom_point(aes(x = as.integer(Presidents), y = Values, color = Presidents), data = dat01a, size = 4) +
    #geom_line(aes(x = as.integer(Presidents), y = Values), data = dat01a) +
    scale_fill_brewer(type = "seq", palette = color_brewer_palette) +
    facet_wrap(. ~ Metrics) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption
    )
  
