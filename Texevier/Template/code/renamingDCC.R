renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {

    ncolrtn <- ncol(ReturnSeries)
    namesrtn <- colnames(ReturnSeries)
    paste(namesrtn, collapse = "_")

    nam <- c()
    xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)

    nam <- c()
    for (j in 1:(ncolrtn)) {
        for (i in 1:(ncolrtn)) {
            nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
        }
    }

    colnames(DCC.TV.Cor) <- nam

    # So to plot all the time-varying correlations wrt SBK:
    # First append the date column that has (again) been removed...
    DCC.TV.Cor <-
        data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
        mutate(date = as.Date(date)) %>%  tbl_df()

    DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)

    DCC.TV.Cor
}
