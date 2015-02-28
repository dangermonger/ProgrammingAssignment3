best <- function(state, outcome) { 
  
  original.dataframe <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## assign original.dataframe as read csv file, this creates a dataframe
  ailments <- c("heart attack", "heart failure", "pneumonia") ##create a vector of medical conditions
  if(!state %in% original.dataframe$State) stop("invalid state") ## if the inputted state is not(!) in the original.dataframe$State column, stop execution of function and print message
  if(!outcome %in% ailments) stop("invalid outcome") ##if inputted outcome is not(!) in ailments vector, then stop execution and print message
  
  state.input.dataframe <- subset(original.dataframe, State==state) ##state.input.dataframe is assigned as a subset of the original.dataframe where the State column is equal to inputted state
  state.input.dataframe[state.input.dataframe=="Not Available"] <- NA ## convert "Not Available" to NA in the specific state dataframe
  
  if(outcome == "heart attack"){ ##if inputted outcome is equal to "heart attack"...
    state.input.dataframe[, 11] <- as.numeric(state.input.dataframe[, 11]) ##convert formatting from character to numeric in heart attack column
    lowest.mortality <- min(na.omit(state.input.dataframe[, 11])) ## assign lowest.mortality to the lowest value in the heart attack column and remove NAs
    best.hospital <- subset(state.input.dataframe, state.input.dataframe[, 11] == lowest.mortality) ## assign best.hospital to the subset of the state.input.dataframe dataframe where the heart attack column is equal to lowest.mortality 
    return(best.hospital[, 2]) ## return the subset of best.hospital where the column number is equal to 2, i.e Hospital Name
  } 
  
  if(outcome == "heart failure"){ 
    state.input.dataframe[, 17] <- as.numeric(state.input.dataframe[, 17]) 
    lowest.mortality <- min(na.omit(state.input.dataframe[, 17])) 
    best.hospital <- subset(state.input.dataframe, state.input.dataframe[, 17] == lowest.mortality) 
    return(best.hospital[, 2])
  } 
  
  if(outcome == "pneumonia"){ 
    state.input.dataframe[, 23] <- as.numeric(state.input.dataframe[, 23]) 
    lowest.mortality <- min(na.omit(state.input.dataframe[, 23]))
    best.hospital <- subset(state.input.dataframe, state.input.dataframe[, 23] == lowest.mortality) 
    return(best.hospital[, 2]) 
  } 
  
} 

##You have to convert the 'Not Available's to NAs for the na.omit function
##You have to convert the columns from character to numeric


##best("TX", "heart attack")
##best("TX", "heart failure")
##best("MD", "heart attack")
##best("MD", "pneumonia")
##best("BB", "heart attack")
##best("NY", "hert attack")
