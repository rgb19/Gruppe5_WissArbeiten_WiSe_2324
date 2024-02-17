# Helfer-Funktionen

empirical_entropy <- function(data) {
    empirical_frequencies <- sapply(unique(data), function(x) {
        h <- length(data[data == x]) / length(data)
        return(h * log(h))
    })
    return(-sum(empirical_frequencies) / log(length(empirical_frequencies)))
}

phi_dispersion <- function(data) {
    h_k <- sapply(unique(data), function(x) length(data[data == x]) / length(data))
    phi_min <- 2 * (1 - max(h_k))
    phi_max <- sum(abs(h_k - 1 / length(h_k)))
    return(phi_min / (phi_min + phi_max))
}
