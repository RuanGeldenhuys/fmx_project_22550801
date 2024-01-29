uniGarchFitter <- function(data){
    #GARCH models that will be fitted
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

            #Spec GARCH model
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
