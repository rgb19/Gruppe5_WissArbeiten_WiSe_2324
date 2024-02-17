# Hauptfunktionen:
source("Funktionen-R-Skript 2.R")

categorial_stats <- function(df, column_names, ordered = FALSE) {
    stopifnot(length(column_names) == length(ordered))

    outlist <- list()

    for (i in 1:length(column_names)) {
        sublist <- list()
        data <- df[[column_names[i]]]

        if (ordered[i]) {
            sublist[["median"]] <- median(data)
        }

        sublist[["Empirical Entropy"]] <- empirical_entropy(data)

        outlist[["colname"]] <- sublist
    }

    return(outlist)
}
