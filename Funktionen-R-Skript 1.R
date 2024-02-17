# Hauptfunktionen:


categorial_stats <- function(df, column_names, ordered = FALSE) {
    stopifnot(length(column_names) == length(ordered))

    outlist <- list()

    for (colname in column_names) {
        sublist <- list()

        outlist[["colname"]] <- sublist
    }

    return(outlist)
}
