ljungbox_tests <- function(df) {
    results <- data.frame(Series = character(),
                          TestStatistic = numeric(),
                          PValue = numeric(),
                          LagOrder = numeric(),
                          stringsAsFactors = FALSE)

    #Loop through each series
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
