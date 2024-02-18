# Hauptfunktionen:
source("Funktionen-R-Skript 2.R")

categorial_stats <- function(df, column_names, ordered = FALSE) {
    stopifnot(length(column_names) == length(ordered))

    outlist <- list()

    for (i in 1:length(column_names)) {
        sublist <- list()
        data <- df[[column_names[i]]]

        if (ordered[i]) {
            data_sorted <- sort(data)
            indices <- floor(c(0.25, 0.75) * length(data))
            sublist[["0.25 quantile"]] <- data_sorted[indices[1]]
            sublist[["median"]] <- median(data)
            sublist[["0.75 quantile"]] <- data_sorted[indices[2]]
        }

        h_k <- sapply(unique(data), function(x) length(data[data == x]) / length(data))

        sublist[["Mode"]] <- unique(data)[h_k == max(h_k)]
        sublist[["Empirical entropy"]] <- empirical_entropy(h_k)
        sublist[["Phi dispersion"]] <- phi_dispersion(h_k)

        outlist[[column_names[i]]] <- sublist
    }

    return(outlist)
}

titanic <- read.csv("titanic.csv")
print(categorial_stats(titanic, "Pclass", TRUE))
