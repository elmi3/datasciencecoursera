df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#dim(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])
library(dplyr)

best <- function(state, outcome){
  #2 Hospital.Name
  #7 State
  #11 Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  s <- 0
  if (outcome == "heart attack"){
    s <- 11
  }else if(outcome == "heart failure"){
    s <- 17
  }else if(outcome == "pneumonia"){
    s <- 23
  }else{
    stop("Please enter valid condition.")
  }
  if (!state %in% df$State){
    stop("Please enter valid state.")
  }
  
  result <- df %>%
    filter(State == state) %>%
    filter(.[[s]] == min(.[[s]])) %>%
    pull(Hospital.Name)
  result
}
