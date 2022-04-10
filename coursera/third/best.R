best <- function(state, outcome) {
  df <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  illnessMap <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  outcomeName <- illnessMap[[outcome]]
  if (is.null(outcomeName))
  {
    stop('invalid outcome')
  }
  
  dfSubset <- df[df$State==state, ]
  if (nrow(dfSubset) == 0)
  {
    stop('invalid state')
  }
  
  neededData <- dfSubset[, c("Hospital.Name", outcomeName)]
  neededData <- neededData[neededData[outcomeName] != "Not Available", ]
  
  neededData[which.min(as.numeric(neededData[[outcomeName]])), ][['Hospital.Name']]
}