rankall <- function(outcome, num = "best") {
  original_dataframe <- read.csv("outcome-of-care-measures.csv", colClasses="character")##read dataframe
  
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
  ##str(state_split) is a list of 54 dataframes divided by state
  
  order_and_checker <- function(looped_state_split, num) { ##takes a looped state.split dataframe and num from pre_result lapply
    ##str(looped_state_split) is a looped series of dataframes passed to the order_and_checker function
    order_index <- order(looped_state_split[3], looped_state_split$Hospital.Name,  na.last=NA) ## each looped_state_split dataframe is ordered by death rate and hostpital name and NAs are removed.
    ##str(order_index) is a looped series of integers representing the row numbers of the ordered dataframes. You can use this vector to order data.
    
    if (num == "best") {##if inputted num is "best"...
      looped_state_split$Hospital.Name[order_index[1]]##subset the hosptial.name column where order_index is 1
    } else if (num == "worst") {##else if inputted num is 'worst'...
      looped_state_split$Hospital.Name[order_index[length(order_index)]]##subset the hosptial.name column where the subset of the order_index is equal to the length of the order index, i.e. the last element
    } else if (is.numeric(num)) {##else if num is inputted..
      looped_state_split$Hospital.Name[order_index[num]]## subset the hosptial.name column where order_index equals the inputted num
    } else {
      stop("invalid num")
    }
  }
  
  pre_result <- lapply(state_split, order_and_checker, num) ##lapply passes the list of state_split dataframes, along with the inputted num, to the order_and_checker function
  ##str(pre_result) is a list of 54 hospitals by state where the rank is equal to the inputted number
  data.frame(hospital = unlist(pre_result), state = names(pre_result), row.names = names(pre_result))##create a dataframe where the first column 'hospital' is the unlisted pre_result, the 2nd column and row names are derived from named attributes (states) in preresult.
}



 
##head(rankall("heart attack", 20), 10)
##tail(rankall("pneumonia", "worst"), 3)

##lists objects can have attributes, like names. Unlist won't unlist named attributes.
##na.last = NA removes NAs when ordering
##[3] subsets a dataframe on the third column, while [, 3] subsets a vector of the third column
