# Has dependencies on
#   "1000 2019-01-30 Presidential Economic Indicators.R"
#   "1010 2019-01-30 Presidential Economic Indicators.R"

## @knitr PairwisePlotsCorrSetup
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
  
  # Create a function to plot the lower panels
  panel.lower <- function(x, y, cex = 1, pch = 1, bg = NULL, ...) {
    lines(x[order(x)], y[order(x)], col = "gray")
    points(x, y, cex = 2 * cex, pch = pch, bg = bg)
    text(x, y, labels = dat03$Presidents, pos = 4, col = "gray")
  }  

  # Convert the data frame to a matrix
  mat01 <- as.matrix(dat03[-1])
  
  dimnames(mat01)[[1]] <- as.character(dat03$Presidents)
  
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
  }

## @knitr PairwiseCorrTable1    
  # View all correlations for overall_rank 
  df_mat01_corr_sorted1 <- df_mat01_corr %>%
    filter(metric1 == "overall_rank") %>% 
    arrange(desc(corr), metric1, metric2)
  
  most_corr <- sort(unique(c(df_mat01_corr_sorted1$metric1, df_mat01_corr_sorted1$metric2)))
  
  kable(df_mat01_corr_sorted1)

  
## @knitr PairwiseCorrTable2
  # Identify the most highly correlated pairs - overall_rank excluded
  df_mat01_corr_sorted2 <- df_mat01_corr %>% 
    filter(metric1 != "overall_rank", corr >= 0.65) %>% 
    arrange(desc(corr), metric1, metric2)
  
  kable(df_mat01_corr_sorted2)
  
## @knitr PairwisePlotsCorrPlot1
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

## @knitr PairwisePlotsCorrPlot2      
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
