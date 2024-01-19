#Stratification functions
identify_vol_periods <- function(df, series_name) {
    df %>%
        select(date, series_name) %>%
        mutate(Year = format(date, "%Y")) %>%
        arrange(date) %>%
        group_by(Year) %>%
        summarise(SD = sd(!!sym(series_name)) * sqrt(12)) %>%
        mutate(HighVol = SD > quantile(SD, 0.75, na.rm = TRUE),
               LowVol = SD < quantile(SD, 0.25, na.rm = TRUE)) %>%
        select(Year, HighVol, LowVol)
}

Perf_comparisons <- function(df, Ys, Alias){
    #This function first calculates the full sample SD for JSE and the Rand
    #It then calculates the SD during either high or low volatility periods of S&P
    #   and compares with full sample SD

    Unconditional_SD <-
        df %>%
        group_by(Index) %>%
        mutate(Full_SD = sd(Returns) * sqrt(12)) %>%
        filter(Year %in% Ys) %>%
        summarise(SD = sd(Returns) * sqrt(12), across(.cols = starts_with("Full"), .fns = max)) %>%
        arrange(desc(SD)) %>% mutate(Period = Alias) %>%
        group_by(Index) %>%
        mutate(Ratio = SD / Full_SD)

    return(Unconditional_SD)

}


analyze_volatility_periods <- function(joinedDF, series_name, all_indices) {
    # Identify high and low volatility periods for the specified series
    vol_periods <- identify_vol_periods(joinedDF, series_name)

    # Extract years of high and low volatility
    high_vol_years <- filter(vol_periods, HighVol) %>% pull(Year)
    low_vol_years <- filter(vol_periods, LowVol) %>% pull(Year)

    # Preparing dataset by filtering out the current series
    Idxs_no_series <- all_indices %>% filter(Index != series_name)

    # Comparing other indices during the series' high volatility periods
    perf_hi_series <- Perf_comparisons(Idxs_no_series, Ys = high_vol_years, Alias = paste("High_Vol", series_name))
    perf_low_series <- Perf_comparisons(Idxs_no_series, Ys = low_vol_years, Alias = paste("Low_Vol", series_name))

    # Combine and return the results
    list(HighVol = perf_hi_series, LowVol = perf_low_series)
}
