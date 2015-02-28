rankhospital <- function(state, outcome, num = "best") {
  original.dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## assign original.dataframe as read csv file, this creates a dataframe
  ailments <- c("heart attack", "heart failure", "pneumonia") ##create a vector of medical conditions
  if(!state %in% original.dataframe$State) stop("invalid state") ## if the inputted state is not(!) in the original.dataframe$State column, stop execution of function and print message
  if(!outcome %in% ailments) stop("invalid outcome") ##if inputted outcome is not(!) in ailments vector, then stop execution and print message
  

  state.input.dataframe <- subset(original.dataframe, State==state) ##state.input.dataframe is assigned as a subset of the original.dataframe where the State column is equal to inputted state
  state.input.dataframe[state.input.dataframe=="Not Available"] <- NA ## convert "Not Available" to NA in the specific state dataframe
  
  
  
  if(outcome == "heart attack"){ ##if inputted outcome is equal to "heart attack"...
    state.input.dataframe[, 11] <- as.numeric(state.input.dataframe[, 11]) ##convert formatting from character to numeric in heart attack column
    state.input.dataframe <- na.omit(state.input.dataframe) ##remove nas from dataframe
    ordered.dataframe <- state.input.dataframe[order(state.input.dataframe$Hospital.Name),] ##order the state.input.dataframe by hospital name

    rank.dataframe <- transform(ordered.dataframe, ## add a column to the dataframe that contains the rank of given column
                                Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, 
                                               FUN = function(x) rank(x, ties.method = "first")))
    rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47]) ##convert the new column from a factor format to numeric
    
    if (num == "best") { ## if inputted num is equal to 'best'..
      num <- min(rank.dataframe[, 47])  } ## assign num to the highest rank (1) 
    if (num == "worst") { ## if inputted num is equal to 'worst'...
      num <- max(rank.dataframe[, 47])  } ## assign num to the lowest rank, or highest number in rank column
    if (num > nrow(rank.dataframe)) return(NA) ## if inputted num is greater than the number of rows in the rank.dataframe, return NA. Stop() didn't work for some reason.

    best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num) ## assign best.hosptial to the subset of rank.dataframe where the inputted num is found in the rank column
    
    return(best.hospital[, 2]) ## return the subset of best.hospital where the column number is equal to 2, i.e Hospital Name
  } 
  
  if(outcome == "heart failure"){ 
    state.input.dataframe[, 17] <- as.numeric(state.input.dataframe[, 17]) 
    state.input.dataframe <- na.omit(state.input.dataframe)
    ordered.dataframe <- state.input.dataframe[order(state.input.dataframe$Hospital.Name),]
    
    rank.dataframe <- transform(ordered.dataframe,  
                                Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State, 
                                               FUN = function(x) rank(x, ties.method = "first")))
    rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47])
    
    if (num == "best") {
      num <- min(rank.dataframe[, 47])  }
    if (num == "worst") {
      num <- max(rank.dataframe[, 47])  }
    if (num > nrow(rank.dataframe)) return(NA)
    
    best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num)
    return(best.hospital[, 2])
  } 
  
  if(outcome == "pneumonia"){ 
    state.input.dataframe[, 23] <- as.numeric(state.input.dataframe[, 23]) 
    state.input.dataframe <- na.omit(state.input.dataframe)
    ordered.dataframe <- state.input.dataframe[order(state.input.dataframe$Hospital.Name),]
    
    rank.dataframe <- transform(ordered.dataframe, 
                                Rank.Col = ave(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State, 
                                               FUN = function(x) rank(x, ties.method = "first")))
    rank.dataframe[, 47] <- as.numeric(rank.dataframe[, 47])
    
    if (num == "best") {
      num <- min(rank.dataframe[, 47])  }
    if (num == "worst") {
      num <- max(rank.dataframe[, 47])  }
    if (num > nrow(rank.dataframe)) return(NA)
    
    best.hospital <- subset(rank.dataframe, rank.dataframe[, 47] == num)

    return(best.hospital[, 2]) 
  } 
}


