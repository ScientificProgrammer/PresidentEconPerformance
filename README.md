## What is the objective of this project?

The objective of [ScientificProgrammer/PresidentEconPerformance](https://github.com/ScientificProgrammer/PresidentEconPerformance) is to perform comprehensive, quantitative assessments of US economic performance using well-established statistical techniques with publicly available, objectively measured data.

Politicians frequently cite positive economic indicators while ignoring negative ones. US stock market performance is often used as a proxy for economic performance. Although the stock market is a key key component of the US economy, and, its value can be measured very objectively, it is just one component of the US economy.

I was inpsired to create this project after reading a [Bloomberg.com article](https://www.bloomberg.com/opinion/articles/2019-01-28/trump-economy-lags-clinton-s-obama-s-reagan-s-and-even-carter-s) [1] that ranked the economic performance of 7 different US presidents' administrations. The article reported an **overall rank** that was computed as an equally weighted average of each president's rank on 14 different economic measures.

## Methodology

### Raw Data
In the *-omics* sciences, we often work with data sets that have *high dimensionality* (*e.g.* hundreds or thousands of features). Although 14 features is a relatively small number by *omics* standards, many of the strategies used with *omics* data could be applied to this data set. Besides the difference in number of features, there is one other key difference between this data and *omics* data sets: the predictive features in this data set contain ordinal ranked values, whereas those in *omics* data sets tend to be continous integer-scale or ratio scale data.   

The following quote from the article explains the methodology that was used.

> "...Measured by 14 gauges of economic activity and financial performance, the U.S. economy is not doing as well under Trump as it did under all but one of the four Republicans and three Democrats who have occupied the White House since 1976.
>
> "These yardsticks, compiled by Bloomberg, assess a broad range of activity — from job and wage growth to the strength of the real estate and auto industries to the health of stock and bond investments that deliver security to workers and retirees alike. They are:
>
> 1. Total nonfarm payrolls
> 1. Manufacturing jobs
> 1. Value of the dollar compared to major currencies
> 1. Gross domestic product
> 1. Federal budget deficit (or surplus) as a percentage of GDP
> 1. Disposable income per capita
> 1. Household debt as a percentage of disposable income
> 1. Home equity
> 1. Car sales
> 1. Hourly wages
> 1. Productivity
> 1. Bond-market performance
> 1. The Standard & Poor’s 500 Index of U.S. stocks
> 1. Gap between U.S. and global stock performance

> "By compiling and ranking the annual improvement in these measures under each of the last seven presidents, an average economic-progress score can be assigned. The scoring gives equal weight to each measure to avoid confusion over valuations that anyone could consider arbitrary."

### Statistical Techniques

1. Dimensionality reduction using PCA for ordinal scale (rank) data. [2]

## Citations
1. Matthew A. Winkler, [Ranking the Trump Economy](https://www.bloomberg.com/opinion/articles/2019-01-28/trump-economy-lags-clinton-s-obama-s-reagan-s-and-even-carter-s), *Bloomberg.com* [Internet]. Online. 2019 Jan 28 [cited 2019 Jan 30].

1. [Exploratory factor analysis for ordinal categorical data](http://dwoll.de/rexrepos/posts/multFApoly.html), *R Examples Repository*, [Internet]. Online. [cited 2019 Jan 30].