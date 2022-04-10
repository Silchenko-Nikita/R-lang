complete <- function(directory, id = 1:332) {
  res <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(res) <- c("id", "nobs")
  
  for (i in id)
  {
    filename <- str_pad(paste(i, ".csv", sep = ""), width = 7, pad = "0")
    filepath <- file.path(directory, filename)
    dframe <- read.csv(filepath)
    
    nitdata <- dframe[["nitrate"]]
    suldata <- dframe[["sulfate"]]
    
    nobs <- nrow(dframe[!is.na(nitdata) & !is.na(suldata), ])
    res[nrow(res) + 1, ] <- c(i, nobs)
  }
  res
}