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
    ## Ncells 467080 25.0    1004322 53.7   660382 35.3
    ## Vcells 863123  6.6    8388608 64.0  1770661 13.6

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.2

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
head(joinedDF)
```

    ## # A tibble: 6 × 4
    ##   date         SP500 Rand_Returns    JSE40
    ##   <date>       <dbl>        <dbl>    <dbl>
    ## 1 2000-04-30 -0.0301     0.00790  -0.0582 
    ## 2 2000-05-31 -0.0205    -0.000407  0.00126
    ## 3 2000-06-30  0.0247    -0.000159  0.0565 
    ## 4 2000-07-31 -0.0156     0.00937   0.0120 
    ## 5 2000-08-31  0.0621     0.0634    0.105  
    ## 6 2000-09-30 -0.0528    -0.0199   -0.0155
