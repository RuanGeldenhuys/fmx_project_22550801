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
}
