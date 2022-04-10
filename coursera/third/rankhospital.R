rankhospital <- function(state, outcome, num = "best") {
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
  outcomes <- neededData[[outcomeName]]
  mapping <- list(
    "best" = 1,
    "worst" = length(outcomes)
  )
  mappingRes <- if (class(num) == "numeric") num else mapping[[num]]
  
  sorted <- neededData[order(as.numeric(neededData[, outcomeName]), neededData[, "Hospital.Name"]),]
  sorted[mappingRes, ][["Hospital.Name"]]
}