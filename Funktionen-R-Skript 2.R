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

# Helfer-Funktion für Chi-Quadrat-Test

# chi_square_test - Führt einen Chi-Quadrat-Test für zwei kategoriale Variablen durch
#                   um den Zusammenhang zwischen ihnen zu überprüfen
#
# Input: daten - Datenrahmen, der die Variablen enthält
#        var1 - Name der ersten kategorialen Variable als Zeichenkette
#        var2 - Name der zweiten kategorialen Variable als Zeichenkette
#
# Output: Ergebnis des Chi-Quadrat-Tests
