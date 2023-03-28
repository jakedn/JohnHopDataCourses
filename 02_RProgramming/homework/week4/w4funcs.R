
## finds the best hospital as per the description in homework pdf
best <- function(statename, outname) {
  res <- rankhospital(statename, outname)
  res
}

rankhospital <- function(state, outcome, num = "best") {
  outcomes = c("heart attack", "heart failure", "pneumonia")
  outcomes_list = setNames(
    c(
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" , 
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      ),
    outcomes)
  ## Read outcome data
  data <- read.csv(
    "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
    colClasses = "character"
  )
  
  ## Check that state and outcome are valid
  if( !(state %in% state.abb) ) stop("invalid state")
  if(! (outcome %in% outcomes) ) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
  data <- data[data$State == state, ] # takes all rows with state
  
  # we need to convert column to numeric to properly sort
  
  data[[ outcomes_list[[outcome]] ]] <-
    as.numeric(data[[ outcomes_list[[outcome]] ]])
  
  # order gives back a permutation of the order by value
  # this allows us to sort the row of a data frame
  data <- data[order(data[[ outcomes_list[[outcome]] ]], data$Hospital.Name), ]
  
  # now we get the amount of none na values there are
  count <- sum( !is.na(data[[ outcomes_list[[outcome]] ]]) )
  if(num == "best") return(data$Hospital.Name[1])
  if(num == "worst") return(data$Hospital.Name[count])
  if(as.numeric(num) > count) return(NA)
  else return(data$Hospital.Name[as.numeric(num)])
  
}

rankall <- function(outcome, num = "best") {
  outcomes = c("heart attack", "heart failure", "pneumonia")
  outcomes_list = setNames(
    c(
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
      "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" , 
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    ),
    outcomes)
  ## Read outcome data
  data <- read.csv(
    "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
    colClasses = "character"
  )
  
  ## Check that state and outcome are valid
  if(! (outcome %in% outcomes) ) stop("invalid outcome")
  
  # we need to convert column to numeric to properly sort
  data[[ outcomes_list[[outcome]] ]] <-
    as.numeric(data[[ outcomes_list[[outcome]] ]])
  
  # order gives back a permutation of the order by value
  # this allows us to sort the row of a data frame
  data <- 
    data[
      order(
        data$State ,
        data[[ outcomes_list[[outcome]] ]], 
        data$Hospital.Name
            ), ]
  data <- data[!is.na(data[[ outcomes_list[[outcome]] ]]), ]
  
  res <- data.frame()
  bystate <- split(data, data$State)
  for(st in names(bystate)) {
    df <- bystate[[st]]
    names <- df$Hospital.Name
    name <- NA
    if(num == "best") {
      name <- names[[1]]
    } else if(num == "worst") {
      name <- names[[length(names)]]
    } else if( as.numeric(num) > length(names) ) {
      name <- NA
    }else {
      name <- names[as.numeric(num)]
    }
    print(name)
    res <- rbind( res, c(name, st) )
  }
  print(res)
  colnames(res) <- c("hospital", "state")
  res
}