rankall <- function(outcome, num="best") {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  df[, 11] <- suppressWarnings(as.numeric(df[, 11]))  # heart attack
  df[, 17] <- suppressWarnings(as.numeric(df[, 17]))  # heart failure
  df[, 23] <- suppressWarnings(as.numeric(df[, 23]))  # pneumonia
  
  states <- sort(unique(df$State))
  print(states)
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
  ranking <- data.frame(
    state = character(),
    hospital = character()
  )
  
  for (state in states){
    result <- df %>%
      filter(State == state) %>%
      filter(!is.na(.[[s]])) %>%
      arrange(Hospital.Name) %>%
      arrange(.[[s]]) %>%
      pull(Hospital.Name)
    if (num == "worst"){
      result = result[length(result)]
    }else if (num == "best"){
      result = result[1]
    }else{
      result = result[num]
    }
    cur <- data.frame(state = state, hospital = result)
    ranking <- rbind(ranking, cur)
  }
  return(ranking)
}