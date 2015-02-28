library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)

lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))

str(lapply)


outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[,column] <- sapply(data[,column], as.character)
data[,column] <- sapply(data[,column], as.numeric)
##na.omit(outcome)
##newdataframe <- dataread[, c(11, 17, 23)]
##as.numeric(as.character(outcome))
#outcome[, 11] <- as.numeric(outcome[, 11])
#outcome[, 17] <- as.numeric(outcome[, 17])
#outcome[, 23] <- as.numeric(outcome[, 23])
#outcome <- outcome[complete.cases(outcome),]
##naremove <- complete.cases(newdataframe)

##head(newdataframe)
##head(outcome[complete.cases(outcome),])
#str(outcome)


specific.state <- subset(outcome, State=='CA')
specific.state[specific.state=="Not Available"] <- NA 
specific.state[,11] <- as.numeric(specific.state[,11])
best.outcome <- min(na.omit(specific.state[,11])) 
best.hospital <- subset(specific.state, specific.state[,11] == best.outcome)
str(best.hospital)




original.dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
state.input.dataframe <- subset(original.dataframe, original.dataframe[, 7] == "TX")
state.input.dataframe[state.input.dataframe=="Not Available"] <- NA
state.input.dataframe <- na.omit(state.input.dataframe)

rank.dataframe <- transform(state.input.dataframe, 
                            Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, 
                            FUN = function(x) rank(x, ties.method = "first")))
rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47])
##num <- min(rank.dataframe[, 47]) 
ordered.dataframe <- rank.dataframe[order(rank.dataframe$Hospital.Name),]
test <- ordered.dataframe[, c(2, 7, 11, 47)]
tail(test)




best.hospital <- subset(ordered.dataframe, ordered.dataframe[, 47] == 4)


ranksubset <- rank.dataframe[, c(2, 7, 11, 47)]

checksubset <- subset(rank.dataframe, rank.dataframe[, 47] == 4)

checksubset[, c(2, 7, 11, 47)]                     

str(checksubset)

colnames(original.dataframe)


x = c(3,2,4,1)
order(x, )

df <- data.frame(item = rep(c('a','b','c'), 3),
                 year = rep(c('2010','2011','2012'), each=3),
                 count = c(1,4,6,3,8,3,5,7,9))

newdf <- df[order(df$item),]
transform(newdf, 
          year.rank = ave(count, year, 
                          FUN = function(x) rank(x, ties.method = "first")))
df

lapply(seq_len(ncol(df)), function(i) df[,i])##turn a dataframe into a list

ncol(df)


newdf <-rank.dataframe[order(rankdataframe$),]

newdf <- df[order(df$item),]


rankall <- function(outcome, num = "best") {
  
  original.dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## assign original.dataframe as read csv file, this creates a dataframe
  ailments <- c("heart attack", "heart failure", "pneumonia") ##create a vector of medical conditions
  if(!outcome %in% ailments) stop("invalid outcome") ##if inputted outcome is not(!) in ailments vector, then stop execution and print message
  original.dataframe[original.dataframe=="Not Available"] <- NA ## convert "Not Available" to NA in the specific state dataframe
  
  
  if(outcome == "heart attack"){ ##if inputted outcome is equal to "heart attack"...
    original.dataframe[, 11] <- as.numeric(original.dataframe[, 11]) ##convert formatting from character to numeric in heart attack column
    ##original.dataframe <- na.omit(original.dataframe) ##remove nas from dataframe
    ##ordered.dataframe <- original.dataframe[order(original.dataframe$Hospital.Name),] ##order the original.dataframe by hospital name
    
    rank.dataframe <- transform(original.dataframe, ## add a column to the dataframe that contains the rank of given column
                                Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, 
                                               FUN = function(x) rank(x, ties.method = "first")))
    rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47]) ##convert the new column from a factor format to numeric
    ##print(head(rank.dataframe[, c(2, 7)]))
    
    if (num == "best") { ## if inputted num is equal to 'best'..
      num <- min(rank.dataframe[, 47])  } ## assign num to the highest rank (1) 
    if (num == "worst") { ## if inputted num is equal to 'worst'...
      num <- max(rank.dataframe[, 47])  } ## assign num to the lowest rank, or highest number in rank column
    if (num > nrow(rank.dataframe)) return(NA) ## if inputted num is greater than the number of rows in the rank.dataframe, return NA
    
    best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num) ## assign best.hosptial to the subset of rank.dataframe where the inputted num is found in the rank column
    final.table <- best.hospital[, c(2, 7)]
    return(final.table)
  } } ##EXTRA CHARACTER HERE!

if(outcome == "heart failure"){ 
  original.dataframe[, 17] <- as.numeric(original.dataframe[, 17]) 
  ##original.dataframe <- na.omit(original.dataframe)
  ##ordered.dataframe <- original.dataframe[order(original.dataframe$Hospital.Name),]
  
  rank.dataframe <- transform(original.dataframe,  
                              Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State, 
                                             FUN = function(x) rank(x, ties.method = "first")))
  rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47])
  
  if (num == "best") {
    num <- min(rank.dataframe[, 47])  }
  if (num == "worst") {
    num <- max(rank.dataframe[, 47])  }
  if (num > nrow(rank.dataframe)) return(NA)
  
  best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num)
  final.table <- best.hospital[, c(2, 7)]
  return(final.table)
} 

if(outcome == "pneumonia"){ 
  original.dataframe[, 23] <- as.numeric(original.dataframe[, 23]) 
  ##original.dataframe <- na.omit(original.dataframe)
  ##ordered.dataframe <- original.dataframe[order(original.dataframe$Hospital.Name),]
  
  rank.dataframe <- transform(original.dataframe, 
                              Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State, 
                                             FUN = function(x) rank(x, ties.method = "first")))
  rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47]) ##convert the new column from a factor format to numeric
  
  if (num == "best") { ## if inputted num is equal to 'best'..
    num <- min(rank.dataframe[, 47])  } ## assign num to the highest rank (1) 
  if (num == "worst") { ## if inputted num is equal to 'worst'...
    num <- max(rank.dataframe[, 47])  } ## assign num to the lowest rank, or highest number in rank column
  if (num > nrow(rank.dataframe)) return(NA) ## if inputted num is greater than the number of rows in the rank.dataframe, return NA
  
  best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num) ## assign best.hosptial to the subset of rank.dataframe where the inputted num is found in the rank column
  final.table <- best.hospital[, c(2, 7)]
  return(final.table)
} 
}

list <- (1:5)
str(list)

df <- data.frame(item = rep(c('a','b','c'), 3),
                 year = rep(c('2010','2011','2012'), each=3),
                 count = c(1,4,6,3,8,3,5,7,9))

str(df)
newdata <- order(df[3], df$item)
str(newdata)