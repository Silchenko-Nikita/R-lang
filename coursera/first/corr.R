corr <- function (directory, threshold = 0) {
  compl <- complete("specdata")
  id = 1:332
  res = c()
  for (i in id)
  {
    filename <- str_pad(paste(i, ".csv", sep = ""), width = 7, pad = "0")
    filepath <- file.path(directory, filename)
    dframe <- read.csv(filepath) 
    if (compl[i, "nobs"] > threshold)
    {
      cr <- cor(dframe[["nitrate"]], dframe[["sulfate"]], use = "pairwise.complete.obs")
      res <- c(res, cr)
    }
  }
  
  res
}