library(tidyr)

titanic <- read.csv("titanic.csv")

# drop unneccessary "Ticket" and "PassengerId" columns
titanic <- subset(titanic, select = -c(Ticket, PassengerId))

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