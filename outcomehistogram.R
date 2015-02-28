setwd("C:/Users/KOLeary/Documents/GitHub/ProgrammingAssignment3")

HarKydwRVE

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)


##To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset), run
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
