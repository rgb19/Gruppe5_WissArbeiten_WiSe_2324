# Anwendung der Funktionen zur Interpretation des Datensatzes

our_titanic <- readRDS("titanic.rds")
source("Funktionen-R-Skript 1.R") # Rstudio meckert hier wegen tokens; passt aber 
                                  # bei mir {Felix}; bsp.code werde ich auskommentieren
source("Funktionen-R-Skript 2.R")


# Deskriptive Statistiken:

numeric_data <- our_titanic[, sapply(our_titanic, is.numeric)]
numeric_stats <- calculate_descriptive_stats_numeric(numeric_data)
print("Descriptive Statistics for Numeric Variables:")
print(numeric_stats)

categorical_stats <- calculate_descriptive_stats_categorical(our_titanic)
print("Descriptive Statistics for Categorical Variables:")
print(categorical_stats)
###############################################################################################

 #Deskriptive bivariate Statistiken

bi_categorical_stats <- chi_square_analysis(our_titanic)
print("Descriptive bivariate Statistics for the connection between two Categorical Variables Survived and Pclass")
print(bi_categorical_stats)

#######################################################IV###################################### [Felix]
# Zahlen auf 2 Nachkommastellen gerundet
# T-test; weitere Statistiken für metrische und dichotome Var.; H0 Gleichheit; kein Unterschied
# Interessant: Überlebensstatus, Geschlecht, Schiffsseite
# von den metrischen Var. sind die Anzahl der Familienmitglieder eher uninteressant

MDStats(our_titanic$Age,our_titanic$Survived) # p < 0.012 es wird abgelehnt
# dead  -> 30.63 mit sd von 12.65
# alive -> 28.26 mit sd von 14.08

MDStats(our_titanic$Fare,our_titanic$Survived) # p < 2.7 * 10^-11 << 0.01 abgelehnt
# dead  -> 22.12 mit sd von 31.38821
# alive -> 48.39 mit sd von 66.597
## Hier ein durchaus starker Unterschied, Überlebende zahlten weitaus mehr in der Regel

MDStats(our_titanic$Age,our_titanic$Sex) # p = 0.0006723363; H0 verwerfen
# female -> 27.68 sd von 13.14915
# male   -> 30.83 sd von 13.19883
## Hilft nicht wirklich

MDStats(our_titanic$Fare,our_titanic$Sex) # p = 5.388057e-07; H0 verworfen
# female -> 44.48; sd: 57.99
# male   -> 25.52; sd: 43.14
##  Starker Unterschied; Da für Sex und Survived signifikante Unterschiede
## bestehen, wäre es interessant zu untersuchen, wie stark sich beide unterscheiden
##  chi_square_test(our_titanic, "Sex","Survived") -> p < 2.2e-16 << 0.0001
##  Es erscheint also, dass sich die beiden Merkmale auch echt unterscheiden

#### Datensatz nicht angepasst, Untersuchung zwar interessant, aber
#### 691 NA's; Einfach nicht aussagekräftig
{
#### Hilfsdatensatz:
h_titanic <- our_titanic
h_titanic$Side <- as.factor(h_titanic$Side) 
h_titanic <- h_titanic[!(is.na(h_titanic$Side)),]

MDStats(h_titanic$Age,as.factor(h_titanic$Side)) # p=0.091; H0 wird behalten
# Backbord   -> 33.863; sd: 15.67
# Steuerbord -> 37.46;  sd: 14.26

MDStats(h_titanic$Fare,as.factor(h_titanic$Side))# p= 0.22; H0 beibehalten
# Backbord ->   71.09; sd: 56.18
# Steuerbord -> 84.86; sd: 92.18
## In beiden Fällen können wir keinen signifkanten Unterschied feststtellen
## Es wären auch zu wenige Daten
}

#### Für Alter und Ticketpreis besteht ein Unterschied bzgl. des Überlebens
#### Jüngere Personen und teuere Tickets lebten eher
#### Auch zahlten Frauen im Vergleich mehr

####### V ###### Visualierung mehrerer kategorialer Variablen [Felix]
## Side und Deck haben zu viele NA's; Zu viel Verlust wenn in den Plots

## Abscpeichern Grafiken: 
pdf("Ergebnisse_Grafiken.pdf")
mosaiccompare(our_titanic[c(1,2,4)])
# Passagiere erster Klasse haben die größte Gruppe Überlebender
# Von diesen waren ein Mehrteil Frauen
# Allgemein waren die meisten Toten Männer, weitaus mehr
# als die (ungleiche) Verteilung der Geschlechter vermuten lässt

bar_split(our_titanic[c(1,2,4)],split_by = 1)
bar_split(our_titanic[c(1,2,4,9)],split_by = 4)
dev.off()
