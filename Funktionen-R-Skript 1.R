# Hauptfunktionen:
source("Funktionen-R-Skript 2.R")

# 2.ii)
# categorial_stats - Berechnet verschiedene Statistiken für kategoriale
#                    Variablen. Berechnet den Modus, die empirische Entropie,
#                    das Phi-Streuungsmaß sowie die absoluten Häufigkeiten jeder
#                    Klasse. Für ordinale Merkmale werden zusätzlich das 0.25,
#                    das 0.5 sowie das 0.75 Quantil berechnet.
#
# Input: df           - Dataframe mit den zu analysierenden Daten
#        column_names - String oder Vektor von Strings mit den zu analysierenden
#                       Spalten des Dataframes df
#        ordered      - Logischer Wert oder Vektor, welcher für jede Spalte in
#                       column_names angibt, ob das Merkmal ordinal ist
#
# Output: Benannte Liste. Jedes Objekt der Liste hat den Namen der analysierten
#         Spalte und ist wiederum eine benannte Liste

categorial_stats <- function(df, column_names, ordered = FALSE) {
    stopifnot(length(column_names) == length(ordered))

    outlist <- list()

    for (i in 1:length(column_names)) {
        sublist <- list()
        data <- df[[column_names[i]]]
        h_k <- sapply(unique(data),
                      function(x) length(data[data == x]) / length(data))

        sublist[["Frequencies"]] <- table(data)
        sublist[["0.25 quantile"]] <- NA
        sublist[["median"]] <- NA
        sublist[["0.75 quantile"]] <- NA

        if (ordered[i]) {
            data_sorted <- sort(data)
            indices <- floor(c(0.25, 0.75) * length(data))
            sublist[["0.25 quantile"]] <- data_sorted[indices[1]]
            sublist[["median"]] <- median(data)
            sublist[["0.75 quantile"]] <- data_sorted[indices[2]]
        }

        sublist[["Mode"]] <- unique(data)[h_k == max(h_k)]
        sublist[["Empirical entropy"]] <- empirical_entropy(h_k)
        sublist[["Phi dispersion"]] <- phi_dispersion(h_k)

        outlist[[column_names[i]]] <- sublist
    }

    return(outlist)
}
