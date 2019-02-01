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
