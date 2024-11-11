pollutantmean<-function(directory, pollutant, id){
  res <- c()
  for (i in id){
    name <- file.path(directory, sprintf("%03d.csv", i))
    df <- read.csv(name)
    res <- c(res, df[[pollutant]][!is.na(df[[pollutant]])])
  }
  mean(res)
}