# Purpose

Purpose of this work folder.

Ideally store a minimum working example data set in data folder.

Add binary files in bin, and closed R functions in code. Human Readable
settings files (e.g. csv) should be placed in settings/

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) max used (Mb)
    ## Ncells 467774 25.0    1006305 53.8   660382 35.3
    ## Vcells 867371  6.7    8388608 64.0  1770599 13.6

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(fmxdat)
library(tseries)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(knitr)
library(MTS)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(ggthemes)
library(rmgarch)
```

    ## Loading required package: rugarch
    ## Loading required package: parallel
    ## 
    ## Attaching package: 'rugarch'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     reduce
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     sigma
    ## 
    ## 
    ## Attaching package: 'rmgarch'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

``` r
library(rugarch)
library(mgarchBEKK)
```

    ## Loading required package: mvtnorm

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Loading and Wrangling

This section deals with loading and wrangling the data into a usable
format.

``` r
global_indices <- readRDS("data/Global_Indices.rds")
local_indices <- readRDS("data/LCL_Indices.rds")
USDZAR <- readRDS("data/USDZAR.rds")

SP <- global_indices %>% #This includes rand returns
    filter(Tickers == "SPXT") %>% 
    select(c(date, Returns, Rand_Returns)) %>% 
    rename(SP500 = Returns)

lcl_index <- "J200" # I create this variable so the choice of SA index can easily be changed
JSE <- local_indices %>% 
    filter(Tickers == lcl_index) %>% 
    select(c(date, Returns)) %>% 
    rename(JSE40 = Returns)

joinedDF <- left_join(SP, JSE, by = 'date')

#Plot the returns
returns_plotter(joinedDF, c("S&P 500", "Rand Returns", "JSE Top 40"))
```

    ## $`S&P 500`

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

    ## 
    ## $`Rand Returns`

![](README_files/figure-markdown_github/unnamed-chunk-2-2.png)

    ## 
    ## $`JSE Top 40`

![](README_files/figure-markdown_github/unnamed-chunk-2-3.png)

# Stratification

This analysis will first focus on seeing whether the JSE experience
higher volatility when the S&P and the rand experiences higher
volatility. I then investigate whether all variables experienced it
during the GFC and Covid. This follows the practical

``` r
#Winsorizing the data to reduce influence of extreme returns
Idxs <- joinedDF %>% 
    gather(Index, Returns, -date) %>% 
    mutate(Year = format(date, "%Y")) %>% 
    group_by(Index) %>% 
    mutate(Top = quantile(Returns, 0.99), Bot = quantile(Returns, 0.01)) %>% 
    mutate(Returns = ifelse(Returns > Top, Top, 
                         ifelse(Returns < Bot, Bot, Returns))) %>% 
    ungroup()

#The analyze_volatility_periods function returns a table for stratification analysis
# of high and low volatility periods for a specific series

results_SP <- analyze_volatility_periods(joinedDF, "SP500", Idxs)
kableExtra::kable(results_SP$HighVol, caption = "S&P 500")
```

<table>
<caption>
S&P 500
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
JSE40
</td>
<td style="text-align:right;">
0.2299355
</td>
<td style="text-align:right;">
0.1703735
</td>
<td style="text-align:left;">
High_Vol SP500
</td>
<td style="text-align:right;">
1.349596
</td>
</tr>
<tr>
<td style="text-align:left;">
Rand_Returns
</td>
<td style="text-align:right;">
0.2156021
</td>
<td style="text-align:right;">
0.1716540
</td>
<td style="text-align:left;">
High_Vol SP500
</td>
<td style="text-align:right;">
1.256027
</td>
</tr>
</tbody>
</table>

``` r
kableExtra::kable(results_SP$LowVol, caption = "S&P 500")
```

<table>
<caption>
S&P 500
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rand_Returns
</td>
<td style="text-align:right;">
0.1527510
</td>
<td style="text-align:right;">
0.1716540
</td>
<td style="text-align:left;">
Low_Vol SP500
</td>
<td style="text-align:right;">
0.8898772
</td>
</tr>
<tr>
<td style="text-align:left;">
JSE40
</td>
<td style="text-align:right;">
0.1385775
</td>
<td style="text-align:right;">
0.1703735
</td>
<td style="text-align:left;">
Low_Vol SP500
</td>
<td style="text-align:right;">
0.8133743
</td>
</tr>
</tbody>
</table>

``` r
results_rand <- analyze_volatility_periods(joinedDF, "Rand_Returns", Idxs)
kableExtra::kable(results_rand$HighVol, caption = "Rand Returns")
```

<table>
<caption>
Rand Returns
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
JSE40
</td>
<td style="text-align:right;">
0.2120507
</td>
<td style="text-align:right;">
0.1703735
</td>
<td style="text-align:left;">
High_Vol Rand_Returns
</td>
<td style="text-align:right;">
1.244622
</td>
</tr>
<tr>
<td style="text-align:left;">
SP500
</td>
<td style="text-align:right;">
0.1938070
</td>
<td style="text-align:right;">
0.1492991
</td>
<td style="text-align:left;">
High_Vol Rand_Returns
</td>
<td style="text-align:right;">
1.298112
</td>
</tr>
</tbody>
</table>

``` r
kableExtra::kable(results_rand$LowVol, caption = "Rand Returns")
```

<table>
<caption>
Rand Returns
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
JSE40
</td>
<td style="text-align:right;">
0.1301606
</td>
<td style="text-align:right;">
0.1703735
</td>
<td style="text-align:left;">
Low_Vol Rand_Returns
</td>
<td style="text-align:right;">
0.7639721
</td>
</tr>
<tr>
<td style="text-align:left;">
SP500
</td>
<td style="text-align:right;">
0.1263740
</td>
<td style="text-align:right;">
0.1492991
</td>
<td style="text-align:left;">
Low_Vol Rand_Returns
</td>
<td style="text-align:right;">
0.8464484
</td>
</tr>
</tbody>
</table>

``` r
results_JSE <- analyze_volatility_periods(joinedDF, "JSE40", Idxs)
kableExtra::kable(results_JSE$HighVol, caption = "JSE Top 40")
```

<table>
<caption>
JSE Top 40
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rand_Returns
</td>
<td style="text-align:right;">
0.2071305
</td>
<td style="text-align:right;">
0.1716540
</td>
<td style="text-align:left;">
High_Vol JSE40
</td>
<td style="text-align:right;">
1.206674
</td>
</tr>
<tr>
<td style="text-align:left;">
SP500
</td>
<td style="text-align:right;">
0.2047659
</td>
<td style="text-align:right;">
0.1492991
</td>
<td style="text-align:left;">
High_Vol JSE40
</td>
<td style="text-align:right;">
1.371514
</td>
</tr>
</tbody>
</table>

``` r
kableExtra::kable(results_JSE$LowVol, caption = "JSE Top 40")
```

<table>
<caption>
JSE Top 40
</caption>
<thead>
<tr>
<th style="text-align:left;">
Index
</th>
<th style="text-align:right;">
SD
</th>
<th style="text-align:right;">
Full_SD
</th>
<th style="text-align:left;">
Period
</th>
<th style="text-align:right;">
Ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Rand_Returns
</td>
<td style="text-align:right;">
0.1246018
</td>
<td style="text-align:right;">
0.1716540
</td>
<td style="text-align:left;">
Low_Vol JSE40
</td>
<td style="text-align:right;">
0.7258890
</td>
</tr>
<tr>
<td style="text-align:left;">
SP500
</td>
<td style="text-align:right;">
0.1042043
</td>
<td style="text-align:right;">
0.1492991
</td>
<td style="text-align:left;">
Low_Vol JSE40
</td>
<td style="text-align:right;">
0.6979563
</td>
</tr>
</tbody>
</table>

# ARCH Tests

To test for ARCH effects I create a function that fits a simple AR(1) to
each return series. I then run Ljung-Box tests on the residuals of each
of those models. The null of “No ARCH effects” is rejected for all three
series.

``` r
ret_df <- joinedDF %>% 
    select(c(-date))


ljungbox_tests <- function(df) {
  results <- data.frame(Series = character(),
                        TestStatistic = numeric(),
                        PValue = numeric(),
                        LagOrder = numeric(),
                        stringsAsFactors = FALSE)

  for (series in names(df)) {
    # Fit AR(1) model
    model <- lm(df[[series]] ~ lag(df[[series]]), data = df, na.action = na.exclude)

    # Perform Ljung-Box test on squared residuals
    test_result <- Box.test(residuals(model)^2, lag = 12, type = "Ljung-Box", fitdf = 1)

    # Compile results
    results <- rbind(results, data.frame(Series = series,
                                         TestStatistic = test_result$statistic,
                                         PValue = test_result$p.value,
                                         LagOrder = 12))
  }

  rownames(results) <- NULL

  return(results)
}


arch_results_lb <- ljungbox_tests(ret_df)
kable(arch_results_lb, caption = "Ljung-Box Tests")
```

<table>
<caption>
Ljung-Box Tests
</caption>
<thead>
<tr>
<th style="text-align:left;">
Series
</th>
<th style="text-align:right;">
TestStatistic
</th>
<th style="text-align:right;">
PValue
</th>
<th style="text-align:right;">
LagOrder
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SP500
</td>
<td style="text-align:right;">
66.35250
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Rand_Returns
</td>
<td style="text-align:right;">
27.65200
</td>
<td style="text-align:right;">
0.003659
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
JSE40
</td>
<td style="text-align:right;">
65.24189
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
12
</td>
</tr>
</tbody>
</table>

``` r
arch_results_march <- MarchTest(ret_df)
```

    ## Q(m) of squared series(LM test):  
    ## Test statistic:  85.86316  p-value:  3.530509e-14 
    ## Rank-based Test:  
    ## Test statistic:  84.14425  p-value:  7.693846e-14 
    ## Q_k(m) of squared series:  
    ## Test statistic:  150.1212  p-value:  7.265193e-05 
    ## Robust Test(5%) :  107.2135  p-value:  0.1041489

#GARCH modelling

## Univariate GARCH models

I will now fit various univariate GARCH models to determine the best
specification.

``` r
uniGarchFitter <- function(data){
    models <- c("sGARCH", "gjrGARCH", "apARCH")
    dist.model <- "norm"
    
    result_list <- list()
    
    #Loop through each column in DF
    for (i in 1:ncol(data)) { 
        resultDF <- data.frame(
            Model = character(),
            Akaike = integer(),
            Bayes = integer(),
            Shibata = integer(),
            HannanQuinn = integer()
        )
        
        j = 0
        
        #For each column loop through each type of model and fit it
        for (model_type in models) {
            j = j+1 #additional counter since we are looping though a string list
            spec <- ugarchspec(
                variance.model = list(model = model_type, garchOrder = c(1, 1)),
                mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                distribution.model = dist.model
                )
            
            fit <- ugarchfit(spec = spec, data = as.data.frame(data[i]))
            
            IC <- infocriteria(fit)
            
            resultDF[j, 1] <- model_type #Place the IC for that particular model in the DF
            resultDF[j, 2:5] <- IC
            
            
        }
        
        #add that result DF to the main list
        result_list[[colnames(data)[i]]] <- resultDF 
        
    }
    
    return(result_list)
}

garch_df <- joinedDF %>% 
    select(c(SP500, JSE40, Rand_Returns)) %>% 
    mutate(SP = SP500 * 100,
           JSE = JSE40 * 100,
           Rand = Rand_Returns * 100) %>% 
    select(c(SP, JSE, Rand))

uGarch_tables <- uniGarchFitter(garch_df)

kableExtra::kable(uGarch_tables$SP, caption = "S&P 500")
```

<table>
<caption>
S&P 500
</caption>
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
Akaike
</th>
<th style="text-align:right;">
Bayes
</th>
<th style="text-align:right;">
Shibata
</th>
<th style="text-align:right;">
HannanQuinn
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sGARCH
</td>
<td style="text-align:right;">
5.666573
</td>
<td style="text-align:right;">
5.730815
</td>
<td style="text-align:right;">
5.665967
</td>
<td style="text-align:right;">
5.692329
</td>
</tr>
<tr>
<td style="text-align:left;">
gjrGARCH
</td>
<td style="text-align:right;">
5.611453
</td>
<td style="text-align:right;">
5.688544
</td>
<td style="text-align:right;">
5.610585
</td>
<td style="text-align:right;">
5.642361
</td>
</tr>
<tr>
<td style="text-align:left;">
apARCH
</td>
<td style="text-align:right;">
5.618397
</td>
<td style="text-align:right;">
5.708336
</td>
<td style="text-align:right;">
5.617220
</td>
<td style="text-align:right;">
5.654455
</td>
</tr>
</tbody>
</table>

``` r
kableExtra::kable(uGarch_tables$JSE, caption = "JSE Top 40")
```

<table>
<caption>
JSE Top 40
</caption>
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
Akaike
</th>
<th style="text-align:right;">
Bayes
</th>
<th style="text-align:right;">
Shibata
</th>
<th style="text-align:right;">
HannanQuinn
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sGARCH
</td>
<td style="text-align:right;">
6.013685
</td>
<td style="text-align:right;">
6.077927
</td>
<td style="text-align:right;">
6.013079
</td>
<td style="text-align:right;">
6.039441
</td>
</tr>
<tr>
<td style="text-align:left;">
gjrGARCH
</td>
<td style="text-align:right;">
5.951093
</td>
<td style="text-align:right;">
6.028184
</td>
<td style="text-align:right;">
5.950225
</td>
<td style="text-align:right;">
5.982000
</td>
</tr>
<tr>
<td style="text-align:left;">
apARCH
</td>
<td style="text-align:right;">
5.956980
</td>
<td style="text-align:right;">
6.046919
</td>
<td style="text-align:right;">
5.955803
</td>
<td style="text-align:right;">
5.993038
</td>
</tr>
</tbody>
</table>

``` r
kableExtra::kable(uGarch_tables$Rand, caption = "Rand")
```

<table>
<caption>
Rand
</caption>
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
Akaike
</th>
<th style="text-align:right;">
Bayes
</th>
<th style="text-align:right;">
Shibata
</th>
<th style="text-align:right;">
HannanQuinn
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sGARCH
</td>
<td style="text-align:right;">
6.051999
</td>
<td style="text-align:right;">
6.116242
</td>
<td style="text-align:right;">
6.051393
</td>
<td style="text-align:right;">
6.077755
</td>
</tr>
<tr>
<td style="text-align:left;">
gjrGARCH
</td>
<td style="text-align:right;">
6.044490
</td>
<td style="text-align:right;">
6.121581
</td>
<td style="text-align:right;">
6.043622
</td>
<td style="text-align:right;">
6.075398
</td>
</tr>
<tr>
<td style="text-align:left;">
apARCH
</td>
<td style="text-align:right;">
6.005175
</td>
<td style="text-align:right;">
6.095115
</td>
<td style="text-align:right;">
6.003999
</td>
<td style="text-align:right;">
6.041234
</td>
</tr>
</tbody>
</table>

For the S&P 500 and JSE Top 40 the gjrGARCH performs best. For the Rand
it is the apARCH. I therefore select the gjrGARCH as my univariate
specification. This follows directly from the practicals.

## Multivariate GARCH

``` r
garch_xts <- joinedDF %>% 
    rename(SP = SP500,
           JSE = JSE40,
           Rand = Rand_Returns) %>% 
    tbl2xts::tbl_xts()
```

### DCC

I first fit an Engly type DCC and then a DCC model based on the
univariate gjrGARCH specification. The results are practically the same.

``` r
DCCpre <- dccPre(garch_xts, include.mean = T, p = 0)
```

    ## Sample mean of the returns:  0.006516667 0.01051822 0.01181206 
    ## Component:  1 
    ## Estimates:  0.000103 0.236741 0.729124 
    ## se.coef  :  6.7e-05 0.067086 0.073942 
    ## t-value  :  1.548631 3.528925 9.860745 
    ## Component:  2 
    ## Estimates:  0.000355 0.102784 0.760644 
    ## se.coef  :  0.000249 0.078853 0.151865 
    ## t-value  :  1.428891 1.303482 5.00869 
    ## Component:  3 
    ## Estimates:  0.00026 0.199472 0.707175 
    ## se.coef  :  0.00017 0.088052 0.121476 
    ## t-value  :  1.530618 2.265381 5.821511

``` r
StdRes <- DCCpre$sresi

DCC <- dccFit(StdRes, type="Engle")
```

    ## Estimates:  0.8551849 0.04095877 20 
    ## st.errors:  0.09290129 0.02618852 8.369476 
    ## t-values:   9.205307 1.563997 2.389636

``` r
Rhot <- DCC$rho.t

ReturnSeries = garch_xts
DCC.TV.Cor = Rhot




Rhot <- 
  renamingdcc(ReturnSeries = garch_xts, DCC.TV.Cor = Rhot)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## ℹ Please use `tibble::as_tibble()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
head(Rhot %>% arrange(date))
```

    ## # A tibble: 6 × 3
    ##   date       Pairs       Rho
    ##   <date>     <chr>     <dbl>
    ## 1 2000-04-30 SP_SP     1    
    ## 2 2000-04-30 SP_Rand   0.470
    ## 3 2000-04-30 SP_JSE    0.589
    ## 4 2000-04-30 Rand_SP   0.470
    ## 5 2000-04-30 Rand_Rand 1    
    ## 6 2000-04-30 Rand_JSE  0.414

``` r
str(Rhot)
```

    ## tibble [2,556 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ date : Date[1:2556], format: "2000-04-30" "2000-05-31" ...
    ##  $ Pairs: chr [1:2556] "SP_SP" "SP_SP" "SP_SP" "SP_SP" ...
    ##  $ Rho  : num [1:2556] 1 1 1 1 1 1 1 1 1 1 ...

``` r
dcc_JSE <- ggplot(Rhot %>% filter(grepl("JSE_", Pairs ), !grepl("_JSE", Pairs)) ) + 
    geom_line(aes(x = date, y = Rho, colour = Pairs), linewidth = 1) + 
    
    annotate("rect", xmin = as.Date("2007-06-22"), xmax = as.Date("2009-06-23"),
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.4)+
    annotate("rect", xmin = as.Date("2020-03-15"), xmax = as.Date("2022-06-20"),
    ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.4)+
    
    theme_fmx()+
    ggtitle("Dynamic Conditional Correlations: JSE")+
    scale_x_date(date_breaks = "4 years", date_labels = "%Y")

finplot(dcc_JSE)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
uspec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                    distribution.model = "sstd")

multi_univ_garch_spec <- multispec(replicate(ncol(garch_xts), uspec))

spec.dcc = dccspec(multi_univ_garch_spec, dccOrder = c(1, 1), distribution = "mvnorm", 
                   lag.criterion = c("AIC", "HQ", "SC", "FPE")[1], 
                   model = c("DCC", "aDCC")[1])

cl = makePSOCKcluster(10)

multf = multifit(multi_univ_garch_spec, garch_xts, cluster = cl)

fit.dcc = dccfit(spec.dcc, data = garch_xts, solver = "solnp", 
    cluster = cl, fit.control = list(eval.se = FALSE), fit = multf)

RcovList <- rcov(fit.dcc)  # This is now a list of the monthly covariances of our DCC model series.
covmat = matrix(RcovList, nrow(garch_xts), ncol(garch_xts) * ncol(garch_xts), 
    byrow = TRUE)
mc1 = MCHdiag(garch_xts, covmat)
```

    ## Test results:  
    ## Q(m) of et: 
    ## Test and p-value:  11.2619 0.3374808 
    ## Rank-based test: 
    ## Test and p-value:  15.89436 0.1026967 
    ## Qk(m) of epsilon_t: 
    ## Test and p-value:  74.19735 0.8857767 
    ## Robust Qk(m):  
    ## Test and p-value:  78.46597 0.8021065

``` r
dcc.time.var.cor <- rcor(fit.dcc)

dcc.time.var.cor <- aperm(dcc.time.var.cor, c(3, 2, 1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)

dcc.time.var.cor <- renamingdcc(ReturnSeries = garch_xts, DCC.TV.Cor = dcc.time.var.cor)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## ℹ Please use `tibble::as_tibble()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
dcc_JSE_gjr <- ggplot(dcc.time.var.cor %>% dplyr::filter(grepl("JSE_", Pairs ), !grepl("_JSE", Pairs)) ) + 
    geom_line(aes(x = date, y = Rho, colour = Pairs), linewidth = 1) + 
    
    annotate("rect", xmin = as.Date("2007-06-22"), xmax = as.Date("2009-06-23"),
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.4)+
    annotate("rect", xmin = as.Date("2020-03-15"), xmax = as.Date("2022-06-20"),
    ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.4)+
    
    theme_fmx()+
    ggtitle("Dynamic Conditional Correlations: JSE")+
    scale_x_date(date_breaks = "4 years", date_labels = "%Y")

finplot(dcc_JSE_gjr)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Go-Garch

Next I fit a Go-Garch model.

``` r
spec.go <- gogarchspec(multi_univ_garch_spec, 
                       distribution.model = 'mvnorm', # or manig.
                       ica = 'fastica') # Note: we use the fastICA
cl <- makePSOCKcluster(10)
multf <- multifit(multi_univ_garch_spec, garch_xts, cluster = cl)

fit.gogarch <- gogarchfit(spec.go, 
                      data = garch_xts, 
                      solver = 'hybrid', 
                      cluster = cl, 
                      gfun = 'tanh', 
                      maxiter1 = 40000, 
                      epsilon = 1e-08, 
                      rseed = 100)

print(fit.gogarch)
```

    ## 
    ## *------------------------------*
    ## *        GO-GARCH Fit          *
    ## *------------------------------*
    ## 
    ## Mean Model       : CONSTANT
    ## GARCH Model      : sGARCH
    ## Distribution : mvnorm
    ## ICA Method       : fastica
    ## No. Factors      : 3
    ## No. Periods      : 284
    ## Log-Likelihood   : 1529.42
    ## ------------------------------------
    ## 
    ## U (rotation matrix) : 
    ## 
    ##         [,1]   [,2]  [,3]
    ## [1,] -0.7109 -0.281 0.645
    ## [2,]  0.0999  0.867 0.488
    ## [3,]  0.6962 -0.411 0.588
    ## 
    ## A (mixing matrix) : 
    ## 
    ##        [,1]     [,2]     [,3]
    ## [1,] 0.0432  0.00777 -0.00653
    ## [2,] 0.0220 -0.01288 -0.04328
    ## [3,] 0.0219  0.03677 -0.02633

``` r
# Extracting time-varying conditional correlations: You know the drill...
gog.time.var.cor <- rcor(fit.gogarch)
gog.time.var.cor <- aperm(gog.time.var.cor,c(3,2,1))
dim(gog.time.var.cor) <- c(nrow(gog.time.var.cor), ncol(gog.time.var.cor)^2)
# Finally:
gog.time.var.cor <-
renamingdcc(ReturnSeries = garch_xts, DCC.TV.Cor = gog.time.var.cor)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## ℹ Please use `tibble::as_tibble()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
gog_JSE_gjr <- ggplot(gog.time.var.cor %>% dplyr::filter(grepl("JSE_", Pairs ), !grepl("_JSE", Pairs)) ) + 
    geom_line(aes(x = date, y = Rho, colour = Pairs), linewidth = 1) + 
    
    annotate("rect", xmin = as.Date("2007-06-22"), xmax = as.Date("2009-06-23"),
    ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.4)+
    annotate("rect", xmin = as.Date("2020-03-15"), xmax = as.Date("2022-06-20"),
    ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.4)+
    
    theme_fmx()+
    ggtitle("Dynamic Conditional Correlations: JSE")+
    scale_x_date(date_breaks = "4 years", date_labels = "%Y")

finplot(gog_JSE_gjr)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

### BEKK-GARCH

Lastly I fit a BEKK-GARCH model to estimate the spillover effects
between the 3 return series.

``` r
garch_df <- joinedDF %>% 
    select(c(SP500, JSE40, Rand_Returns)) %>% 
    mutate(SP = SP500 * 100,
           JSE = JSE40 * 100,
           Rand = Rand_Returns * 100) %>% 
    select(c(SP, JSE, Rand))

garch_matrix <- as.matrix(garch_df)


estimated <- mgarchBEKK::BEKK(garch_matrix)
```

``` r
BEKK_estimates <- estimated$est.params
names(BEKK_estimates) <- c("Constants", "ARCH estimates", "GARCH estimates")

BEKK_se <- estimated$asy.se.coef
names(BEKK_se) <- c("Constants' standard errors", "ARCH standard errors", 
                    "GARCH standard errors")

BEKK_estimates
```

$Constants \[,1\] \[,2\] \[,3\] \[1,\] 0.6650514 -0.7573048 2.33085802
\[2,\] 0.0000000 1.3220500 -2.23007857 \[3,\] 0.0000000 0.0000000
0.04540093

$`ARCH estimates` \[,1\] \[,2\] \[,3\] \[1,\] 0.53607600 0.4747793
0.4511420 \[2,\] -0.02733169 -0.2254145 -0.1677014 \[3,\] -0.07922857
0.1554714 0.1483722

$`GARCH estimates` \[,1\] \[,2\] \[,3\] \[1,\] 0.52019209 1.1721377
1.0643220 \[2,\] 0.02598961 -0.7976212 -0.4348628 \[3,\] 0.40528022
0.2632717 -0.2119068

``` r
BEKK_se
```

$`Constants' standard errors` \[,1\] \[,2\] \[,3\] \[1,\] 2.323466
2.263693 3.541815 \[2,\] 0.000000 2.620605 3.489223 \[3,\] 0.000000
0.000000 1.979644

$`ARCH standard errors` \[,1\] \[,2\] \[,3\] \[1,\] 0.1867899 0.3023454
0.3390503 \[2,\] 0.1840774 0.2588054 0.2719637 \[3,\] 0.1266753
0.1045829 0.1780824

$`GARCH standard errors` \[,1\] \[,2\] \[,3\] \[1,\] 0.3183889 0.2233751
0.3439757 \[2,\] 0.3585999 0.2079060 0.7804449 \[3,\] 0.5018321
0.2043701 1.3113928

``` r
matrix_rename <- function(matrix, headers){
    colnames(matrix) <- headers
    rownames(matrix) <- headers
    return(matrix)
}

renamed_BEKK_estimates <- lapply(BEKK_estimates, matrix_rename, 
                                 headers = c("SP", "JSE", "Rand"))
renamed_BEKK_se <- lapply(BEKK_se, matrix_rename, 
                          headers = c("SP", "JSE", "Rand"))


k_constants <- kable(renamed_BEKK_estimates$Constants,
                     caption = 'Constants')
k_constants_se <- kable(renamed_BEKK_se$`Constants' standard errors`,
                        caption = "Constants' standard errors")

cat('<div style="display: inline-block; width: 45%;">', k_constants, '</div>')
```

<table>
<caption>
Constants
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
SP
</th>
<th style="text-align:right;">
JSE
</th>
<th style="text-align:right;">
Rand
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SP
</td>
<td style="text-align:right;">
0.6650514
</td>
<td style="text-align:right;">
-0.7573048
</td>
<td style="text-align:right;">
2.3308580
</td>
</tr>
<tr>
<td style="text-align:left;">
JSE
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
1.3220500
</td>
<td style="text-align:right;">
-2.2300786
</td>
</tr>
<tr>
<td style="text-align:left;">
Rand
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0454009
</td>
</tr>
</tbody>
</table>

``` r
cat('<div style="display: inline-block; width: 45%; margin-left: 10px;">', k_constants_se, '</div>')
```

<table>
<caption>
Constants’ standard errors
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
SP
</th>
<th style="text-align:right;">
JSE
</th>
<th style="text-align:right;">
Rand
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
SP
</td>
<td style="text-align:right;">
2.323466
</td>
<td style="text-align:right;">
2.263693
</td>
<td style="text-align:right;">
3.541815
</td>
</tr>
<tr>
<td style="text-align:left;">
JSE
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
2.620605
</td>
<td style="text-align:right;">
3.489223
</td>
</tr>
<tr>
<td style="text-align:left;">
Rand
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
0.000000
</td>
<td style="text-align:right;">
1.979645
</td>
</tr>
</tbody>
</table>
