returns_plotter <- function(df, names){
    datalist <- list()
    plotlist <- list()

    for (i in c(2:4)) {

        #Mutate dataframr to get absolute and squared returns
        #Put each series' data into a separate df, stored in a list
        datalist[[i-1]] <- df %>%
            select(c(1, i)) %>%
            rename(Returns = names(.)[2]) %>%
            mutate(`Absolute Returns` = abs(Returns)) %>%
            mutate(`Squared Returns` = Returns^2) %>%
            gather(key = Type, value = Returns, -date)

    }

    names(datalist) <- names

    #Loop through dataframe list
    for (j in c(1:3)) {

        datalist[[j]]$Type <- factor(datalist[[j]]$Type,
                                     levels = c("Returns", "Absolute Returns",
                                                "Squared Returns"))

        #Plot the returns for the series using facet_wrap for seperate plots
        g1 <- ggplot(datalist[[j]]) +
            geom_line(aes(x = date, y = Returns, colour = Type), size = 1.1)+
            geom_hline(yintercept = 0)+
            ggtitle(names[j])+
            facet_wrap(~Type, nrow = 3, scales = "free")+
            guides(alpha = "none", colour = "none") +
            fmxdat::theme_fmx()

        #Store each group of plots in a list
        plotlist[[j]] <- finplot(g1)
    }

    names(plotlist) <- names

    #Return the list of plots
    return(plotlist)
}
