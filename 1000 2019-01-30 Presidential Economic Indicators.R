# Analyzing Bloomberg raw data for 'Best-to-worst economic performance, 1977-2019, under seven presidents'
#
# Source:
#   Ranking the Trump Economy. 2019 Jan 28 [cited 2019 Jan 30];
#   Available from: https://www.bloomberg.com/opinion/articles/2019-01-28/trump-economy-lags-clinton-s-obama-s-reagan-s-and-even-carter-s
#

## @knitr Initialization
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(knitr)

dat01 <- read.csv("0105 2019-01-28 Bloomberg - Presidential Economic Indicators.csv")
#dat02 <- read.csv("0110 2019-01-30 Metric Name Lookups.csv")

# Metric Data Mappings
dat_metrics <- tibble(original = c("Auto sales",
                                 "Bond performance",
                                 "Debt-to-income ratio*",
                                 "Deficit as share of GDP*",
                                 "Disposable income per capita",
                                 "Dollar value",
                                 "Gap between U.S. and global stocks",
                                 "Gross domestic product",
                                 "Home equity",
                                 "Hourly wages",
                                 "Manufacturing jobs",
                                 "Nonfarm payrolls",
                                 "Productivity",
                                 "S&P 500 Index",
                                 "Overall Rank"),
                      pretty = c("Auto Sales",
                                 "Bond Performance",
                                 "Debt-to-Income Ratio*",
                                 "Deficit (Share of GDP)*",
                                 "Disp. Income (per capita)",
                                 "Dollar Value",
                                 "Gap: U.S. & Global Stocks",
                                 "Gross Domestic Product",
                                 "Home Equity",
                                 "Hourly Wages",
                                 "Manufacturing Jobs",
                                 "Nonfarm Payrolls",
                                 "Productivity",
                                 "S&P 500 Index",
                                 "Overall Rank"),
                      column_names = c("auto_sales",
                                       "bond_perf",
                                       "debt_to_inc_ratio",
                                       "deficit",
                                       "disp_inc_per_cap",
                                       "dollar_val",
                                       "us_global_stock_gap",
                                       "GDP",
                                       "home_equity",
                                       "hourly_wages",
                                       "manufacturing_jobs",
                                       "nonfarm_payrolls",
                                       "productivity",
                                       "sp500_index",
                                       "overall_rank")
               )

dat01a <- dat01 %>% 
  mutate(Presidents = factor(
                        x = Presidents,
                        levels = c("Carter",
                                   "Reagan",
                                   "H.W. Bush",
                                   "Clinton",
                                   "W. Bush",
                                   "Obama",
                                   "Trump")),
         Metrics = factor(x = Metrics,
                          levels = dat_metrics$original,
                          labels = dat_metrics$pretty
                          )
  )

plot_title <- "Economic Indicators for 7 US Presidents"
subtitles <- vector("character", 3)
subtitles[1] <- "Date Range: 1979 - 2017"
subtitles[2] <- "Rankings are based on annual percentage change"
subtitles[3] <- "*Annual percentage-point change"

plot_subtitle <- paste(subtitles, collapse = "\n")
plot_caption <- "Data Source:\n    Ranking the Trump Economy by Matthew A. Winkler\n    Bloomberg.com\n    2019-01-28" 
color_brewer_palette = "RdYlGn"

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
p1 <- p0 +
  geom_point(aes(x = Presidents, y = Values, color = Presidents), data = dat01a, size = 4, show.legend = FALSE) +
  geom_line(aes(x = as.integer(Presidents), y = Values), data = dat01a) +
  facet_wrap(. ~ Metrics) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  )
p1

## @knitr PlotAllEconIndBarChart
p2 <- p0 +
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
p2

## @knitr PlotBondPerfSetup
dat02 <- dat01a %>% 
  filter(Metrics == "Bond Performance")

subtitles_bond_rankings <- paste(subtitles[1:2], collapse = "\n")

## @knitr PlotBondPerfScatterPlot
p3 <- p0 +
  geom_point(aes(x = Presidents, y = Values, color = Presidents), data = dat02, size = 6, show.legend = FALSE) +
  geom_line(aes(x = as.integer(Presidents), y = Values), data = dat02) +
  # scale_x_continuous(labels = function(x) levels(dat01a$Presidents)[x],
  #                    breaks = seq.int(length(dat01a$Presidents)),
  #                    minor_breaks = 1:7) +
  # scale_color_brewer(type = "qual", palette = color_brewer_palette) +
  labs(
    title = "Bond Performance Ranks for Seven US Presidents",
    subtitle = subtitles_bond_rankings,
    caption = plot_caption
  ) +
  theme(
    axis.text = element_text(size = 12)
  )
p3

## @knitr PlotBondPerfBarChart
p4 <- p0 +
  geom_col(aes(x = Presidents, y = Values, fill = reorder(Presidents, Values, sort)), dat02, show.legend = FALSE) +
  #scale_fill_brewer(type = "qual", palette = color_brewer_palette) +
  scale_fill_brewer(type = "seq", palette = "RdYlGn", direction = -1) +
  labs(
    title = "Bond Performance Ranks for Seven US Presidents",
    subtitle = subtitles_bond_rankings,
    caption = plot_caption
  ) +
  theme(
    axis.text = element_text(size = 12)
  )
p4

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

## @knitr PairwisePlot

# First, restructure the data.
dat03 <- dat01a %>% 
  spread(key = Metrics, value = Values)
colnames(dat03)[-1] <- dat_metrics$column_names

dat03b <- dat03[, c("Presidents", "overall_rank")]
dat03b$Presidents <- reorder(dat03$Presidents, dat03$overall_rank, sort)
#dat03b$Presidents <- factor(dat03b$Presidents, levels = rev(levels(dat03b$Presidents)))

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

# Convert the data frame to a matrix
mat01 <- as.matrix(dat03[-1])
dimnames(mat01)[[1]] <- as.character(dat03$Presidents)

# Create a function to compute correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method = "spearman") ^ 2
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.lower <- function(x, y, cex = 1, pch = 1, bg = NULL, ...) {
  lines(x[order(x)], y[order(x)], col = "gray")
  points(x, y, cex = 2 * cex, pch = pch, bg = bg)
  text(x, y, labels = dat03$Presidents, pos = 4, col = "gray")
}

plot_cols <- rainbow(length(dimnames(mat01)[[1]])) 

par(xpd = NA)

pairs(
  mat01,
  upper.panel = panel.cor,
  pch = 21,
  bg = plot_cols,
  main = "Pairwise Correlations for 7 Presidents and 14 Economic Indicators and An Overall Rank",
  cex.main = 1.25,
  oma = c(4, 4, 8, 12))

legend(x = 1,
       y = 1,
       pch = 21,
       pt.bg = plot_cols,
       legend = dimnames(mat01)[[1]],
       xpd = TRUE)

# Compute a matrix of Pearson correlation coefficients
mat01_corr <- cor(mat01) ^ 2

# Get the correlation coefficients for unique pairs
dim_len <- dim(mat01_corr)[[1]]
df_len <- choose(dim_len, 2)

df_mat01_corr <-
  data.frame(metric1 = character(df_len),
             metric2 = character(df_len),
             corr = numeric(df_len),
             stringsAsFactors = FALSE)

start_indices <- df_len - rev(cumsum(seq.int(1, dim_len - 1))) + 1
stop_indices <- cumsum(seq.int(dim_len - 1, 1))

for(i in 1:(dim_len - 1)) {

    row_indices <- seq(from = start_indices[i], to = stop_indices[i], by = 1)
    df_mat01_corr[row_indices, "corr"] <- mat01_corr[-1 * seq(1, i, by = 1), i]
    df_mat01_corr[row_indices, "metric1"] <- dimnames(mat01_corr)[[1]][-1 * seq(1, i, by = 1)]
    df_mat01_corr[row_indices, "metric2"] <- dimnames(mat01_corr)[[2]][i]
    #cat(paste(i, dim_len - i, paste0(i, ":", dim_len - 1), "\n", sep = "\t"))
}

# Identify the most highly correlated pairs - overall_rank excluded
df_mat01_corr_sorted1 <- df_mat01_corr %>% 
  filter(metric1 != "overall_rank", corr >= 0.65) %>% 
  arrange(desc(corr), metric1, metric2)

kable(df_mat01_corr_sorted1)

most_corr <- sort(unique(c(df_mat01_corr_sorted1$metric1, df_mat01_corr_sorted1$metric2)))

dat03a <- dat03 %>%
  select(Presidents, most_corr)

par(xpd = NA)

pairs(
  dat03a[-1],
  upper.panel = panel.cor,
  lower.panel = panel.lower,
  xlim = c(0,10),
  ylim = c(0, 8),
  pch =  21,
  bg = plot_cols,
  main = "Pairwise Plots: Most strongly correlated economic indicators",
  cex.main = 2,
  oma = c(4, 4, 8, 4)
)

# Identify the most highly correlated factors with overall_rank 
df_mat01_corr_sorted2 <- df_mat01_corr %>%
  filter(metric1 == "overall_rank") %>% 
  arrange(desc(corr), metric1, metric2)

kable(df_mat01_corr_sorted2)

# Review pairwise plots of the most highly correlated items
dat04 <- as_tibble(mat01) %>%
  mutate(President = dimnames(mat01)[[1]]) %>% 
  select(President, overall_rank, deficit, dollar_val, sp500_index, auto_sales)

# Make a pairwise plot with the most highly correlated values
par(xpd = NA)
pairs(dat04[-1],
      upper.panel = panel.cor,
      main = "Pairwise Plots for Most Highly Correlated Variables",
      pch = 21,
      bg = plot_cols,
      lower.panel = panel.lower,
      xlim = c(0, 10),
      ylim = c(0, 8))
