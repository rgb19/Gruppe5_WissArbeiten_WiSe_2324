library(tidyr)

titanic <- read.csv("titanic.csv")

# drop unneccessary "Ticket" and "PassengerId" columns
titanic <- subset(titanic, select = -c(Ticket, PassengerId))

# separate names into relevant particles and remove name
titanic <- separate(titanic, "Name", into = c(NA, "form_of_address"), sep = "[,.] ", extra = "drop")

# estimate missing age values by taking the mean of all other passengers with the the same form of address
nas <- titanic[is.na(titanic$Age), ]
for (foa in unique(nas$form_of_address)) {
    subframe <- titanic[titanic$form_of_address == foa, ]
    avg_age <- round(mean(subframe$Age, na.rm = TRUE))
    titanic[is.na(titanic$Age) & titanic$form_of_address == foa, ]$Age <- avg_age
}
