complete <- function(directory, id=1:332){
  id_nobs <- data.frame(id = integer(), nobs = integer())
  for (i in id){
    name <- file.path(directory, sprintf("%03d.csv", i))
    df <- read.csv(name)
    cases <- sum(!is.na(df$nitrate) & !is.na(df$sulfate))
    id_nobs <- rbind(id_nobs, data.frame(id = i, nobs = cases))
  }
  print(id_nobs)
}

