library(tidyr)

titanic <- read.csv("titanic.csv")

# drop unneccessary "Ticket" and "PassengerId" columns
titanic <- subset(titanic, select = -c(Ticket, PassengerId))