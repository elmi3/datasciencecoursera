corr <- function(directory, threshold = 0) {
  corr <- numeric(0)
  
  file_list <- list.files(directory, full.names = TRUE)
  
  for (name in file_list) {
    df <- read.csv(name)
    df_filtered <- df[!is.na(df$sulfate) & !is.na(df$nitrate), ]
    
    if (nrow(df_filtered) >= threshold) {
      c <- cor(df_filtered$sulfate, df_filtered$nitrate)
      corr <- c(corr, c)
    }
  }
  
  return(corr)
}


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))