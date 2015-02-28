rankall <- function(outcome, num = "best") {
  original_dataframe <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  column_namer <- if (outcome == "heart attack") { ##if input is "heart attack", column_namer is assigned to the following...
    suppressWarnings(original_dataframe[, 11] <- as.numeric(original_dataframe[, 11]))## the 11th column (heart attack) of original_dataframe is converted to numeric formatting while suppressing NA coersion warnings
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"## column_namer is assigned to the heart attack column heading name
  } else if (outcome == "heart failure") {
    suppressWarnings(original_dataframe[, 17] <- as.numeric(original_dataframe[, 17]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    suppressWarnings(original_dataframe[, 23] <- as.numeric(original_dataframe[, 23]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  
  state_split <- split(original_dataframe[, c("Hospital.Name", "State", column_namer)], original_dataframe$State) ##splits the three column subset of original_dataframe by state
  
  order_and_checker <- function(looped_state_split, num) { ## the function takes a looped state.split dataframe and num from pre_result
    str(looped_state_split)
    ordered_loop <- order(looped_state_split[3], looped_state_split$Hospital.Name,  na.last=NA) ## ordered_loop coerces into a list an ordered a single column dataframe from the third column of looped_state_split (death rate) and then orders by hospital name and then removes NAs.
    str(ordered_loop)
    
    if (num == "best") {##if inputted num is "best"...
      looped_state_split$Hospital.Name[ordered_loop[1]]##creates a single column dataframe from the first column of ordered_loop
    } else if (num == "worst") {
      looped_state_split$Hospital.Name[ordered_loop[length(ordered_loop)]]
    } else if (is.numeric(num)) {
      looped_state_split$Hospital.Name[ordered_loop[num]]
    } else {
      stop("invalid num")
    }
  }
  
  pre_result <- lapply(state_split, order_and_checker, num) ##lapply coerces the state_split dataframe into a list of dataframes which are each passed, along with the inputted num, to the order_and_checker function
  data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))##creates a dataframe with two columns, 'hosptial' and 'state', taking row names from the preresult list.
}



 
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)

##lists objects can have attributes, like names. Unlist won't unlist named attributes.
##na.last = NA removes NAs when ordering
##[3] creates a dataframe of third column, while [, 3] subsets a list of the third column
