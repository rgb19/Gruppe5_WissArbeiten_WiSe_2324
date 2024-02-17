# Helfer-Funktionen

empirical_entropy <- function(data) {
    empirical_frequencies <- sapply(unique(data), function(x) {
        h <- length(data[data == x]) / length(data)
        return(h * log(h))
    })
    return(-sum(empirical_frequencies) / log(length(empirical_frequencies)))
}