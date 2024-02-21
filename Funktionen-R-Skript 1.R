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

# 5.v):
# Visualisierung von drei oder vier Variablen
        # 1: Mosaicplot ~ exakt, aber eher für Überblick
        # 2. "Barplot"  ~ (mittels) ggplot2 -> Abhängig einer Var für die anderen
        #                 Plot per Kategorie => Analyse innerhalb einer Gruppe

  ## na.rm im folgenden immer auf TRUE gesetzt! Wenn nicht Standard
  ## In diesen Grafiken macht eine Unterscheidung für
  ## geordnete Faktoren wenig Sinn

# Output: Die Grafiken | Input: Abschnitte eines DF!

mosaiccompare <- function(data,main = paste("Mosaicplot für:",toString(names(data))),...){
  #... weitere Argumente per ... für spezifischere Grafiken
  
  if(length(data) < 3 | length(data) > 4 | is.data.frame(data) == FALSE){
    print("Data in Form eines df, bzw. eines Teils welcher 3/4 Variablen enthält, eingeben")
  }  
  # seperater Test dass die Variablen kategorial sind möglich
  # geht aber über Projekt hinaus; (Fehler-)Meldungen einzelner Funktionen reichen
  else{
    mosaicplot(table(data),main,...)
    # na's werden standardmäßig ommitted; na.action für S3 mosaicplot
  }
    # Farben automatisch erstellen
}

# Für 4 Variablen ist das ganze leicht unübersichtlich

### 2,3 Barplots für die Var. einer Teilgruppe!
   # Es braucht eig. keine Hilfsfunktion für das Teilen
   # Da wir ja eh für jede Kategorie loopen!

bar_split <- function(data,split_by = 1){
  # data ist auch hier ABSCHNITT DER KATEGORIELLEN VAR!!!
  # split_by = x -> x ist Indize der Var. für die wir untersuchen
  # Wir geben nichts weiter an die Plots, wäre mit bsp. ggplot2 möglich
  n <- length(data)
  par(mfrow = c(length(data)-1,1)) # Eine "Seite" pro Gruppe
  
  if(is.factor(data[,split_by]) == TRUE){
    # Bei Titel definitiv, ansonsten (ungetestet) sollte die Logik nicht klappen
    
   for(i in 1:nlevels(data[,split_by])){
    
     temp <- data[data[split_by] == levels(data[,split_by])[i],]
     # temporärer df für die i-te Kategorie
     temp <- temp[,-split_by]
     # Wir untersuchen ja für die anderen Var. bzgl. dieser!
    
     for(j in 1:(n-1)){
       barplot(table(temp[,j]),main = paste("Barplot für:",names(temp)[j],
               "unter",names(data)[split_by],"=",levels(data[,split_by])[i]),
               ylab = "Häufigkeit")
     }
   }
  }
  else{
      print("EINGABEFEHLER: (Zumindest) Die Splitvariable muss ein factor sein")
    }
}

# Schon besser, macht natürlich nur Sinn, wenn wir auch mehr als 2 Klassen haben
titanic <- readRDS("titanic.rds")
bar_split(titanic[c(1,2,4)],split_by = 2)
# Beispielcode für Passagierklasse; Soll es noch besondere Farben geben?



