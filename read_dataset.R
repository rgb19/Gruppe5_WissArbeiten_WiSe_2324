library(tidyr)

titanic <- read.csv("titanic.csv")

# drop unneccessary "Ticket" and "PassengerId" columns
titanic <- subset(titanic, select = -c(Ticket, PassengerId))


# separate names into relevant particles and remove name
titanic <- separate(titanic, "Name", into = c(NA, "form_of_address"), sep = "[,.] ", extra = "drop")

# combine addresses with the same meaning into a single address
f_young_unmarried <- c("Ms", "Mlle", "Miss")
f_married <- c("Mrs", "Mme")

titanic[titanic$form_of_address %in% f_young_unmarried, ]$form_of_address <- "Ms"
titanic[titanic$form_of_address %in% f_married, ]$form_of_address <- "Mrs"

# estimate missing age values by taking the mean of all other passengers with the the same address
nas <- titanic[is.na(titanic$Age), ]
for (foa in unique(nas$form_of_address)) {
    subframe <- titanic[titanic$form_of_address == foa, ]
    avg_age <- round(mean(subframe$Age, na.rm = TRUE))
    titanic[is.na(titanic$Age) & titanic$form_of_address == foa, ]$Age <- avg_age
}

#Kabinen Informationen
#Backbord/Steuerbord
        #Erinnerung: ungerade->Steuerbord

substrRight <- function(x){
  as.numeric(substr(x, nchar(x), nchar(x)))
}

cab<-sapply(titanic$Cabin, substrRight)
cab<-unname(cab)
titanic$Side<-ifelse(cab%%2, 'Steuerbord', 'Backbord')

#Fehlende Kabinennummern zu "NA"
titanic$Cabin[which(titanic$Cabin== "")]<-NA

#Deck
titanic$Deck<- as.character(gsub("[0-9]", "", titanic$Cabin))
