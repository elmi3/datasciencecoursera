rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Convert relevant columns to numeric (suppress warnings for NA conversion)
  df[, 11] <- suppressWarnings(as.numeric(df[, 11]))  # heart attack
  df[, 17] <- suppressWarnings(as.numeric(df[, 17]))  # heart failure
  df[, 23] <- suppressWarnings(as.numeric(df[, 23]))  # pneumonia
  
  ## Map the outcome to the appropriate column index
  s <- 0
  if (outcome == "heart attack") {
    s <- 11
  } else if (outcome == "heart failure") {
    s <- 17
  } else if (outcome == "pneumonia") {
    s <- 23
  } else {
    stop("invalid outcome")
  }
  
  ## Check that the state is valid
  if (!state %in% df$State) {
    stop("invalid state")
  }
  
  ## Filter data for the specified state and non-NA values for the outcome
  result <- df %>%
    filter(State == state) %>%
    filter(!is.na(.[[s]])) %>%
    # filter(.[[s]] == min(.[[s]], na.rm = TRUE)) %>%
    arrange(Hospital.Name) %>%    # Sort alphabetically to handle ties
    arrange(.[[s]]) %>%
    pull(Hospital.Name)
  if (num == "worst"){
    result = result[length(result)]
  }else if (num == "best"){
    result = result[1]
  }else{
    result = result[num]
  }
  
  ## Return the first hospital in the sorted, filtered set
  return(result)
}