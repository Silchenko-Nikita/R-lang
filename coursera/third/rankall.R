rankall <- function(outcome, num = "best") {
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
  
  neededData <- df[, c("State", "Hospital.Name", outcomeName)]
  neededData <- neededData[neededData[outcomeName] != "Not Available" & !is.na(neededData[outcomeName]), ]
  outcomes <- neededData[[outcomeName]]
  
  sorted <- neededData[
    order(neededData[, "State"], as.numeric(neededData[, outcomeName]), 
          neededData[, "Hospital.Name"]),]
  
  mapping <- list(
    "best" = 1,
    "worst" = nrow(sorted)
  )
  mappingRes <- if (class(num) == "numeric") num else mapping[[num]]
  
  res <- aggregate(c(sorted[outcomeName], sorted["Hospital.Name"]),
                   by = list(sorted$State), 
                   function(x) x[if (num == "worst") length(x) else mappingRes])
  res <- res[, !(colnames(res) %in% c(outcomeName))]
  colnames(res) <- c("state", "hospital")
  res <- res[, c("hospital", "state")] 
  rownames(res) <- res[["state"]]
  res
}