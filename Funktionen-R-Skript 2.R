# Helfer-Funktionen

# empirical_entropy - Berechnet die empirische Entropie aus einem Vektor
#                     relativer Häufigkeiten
#
# Input: h_k - numerischer Vektor der relativen Häufigkeiten
#
# Output: empirische Entropie
empirical_entropy <- function(h_k) {
    empirical_frequencies <- h_k * log(h_k)
    return(-sum(empirical_frequencies) / log(length(empirical_frequencies)))
}

# phi_dispersion - Berechnet das Phi-Streuungsmaß aus einem Vektor relativer
#                  Häufigkeiten
#
# Input: h_k - numerischer Vektor der relativen Häufigkeiten
#
# Output: Phi-Streuungsmaß
phi_dispersion <- function(h_k) {
    phi_min <- 2 * (1 - max(h_k))
    phi_max <- sum(abs(h_k - 1 / length(h_k)))
    return(phi_min / (phi_min + phi_max))
}
