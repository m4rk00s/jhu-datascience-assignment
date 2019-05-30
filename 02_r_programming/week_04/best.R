best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  # dictionary of the outcome indexes
  index_outcome <- c(11, 17, 23)
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  names(index_outcome) <- valid_outcome
  
  # subset the data if the outcome is valid
  if (outcome %in% valid_outcome)
    res <- data[c(2, 7, index_outcome[[outcome]])]
  else
    stop("invalid outcome")
  
  # get rid the na's
  res <- res[complete.cases(res), ]
  
  # change coloum 03 (rate) from char to numeric
  res[, 3] <- as.numeric(res[, 3])
  
  # change the header name
  names(res) <- c("Name", "State", "Rate")
  
  # take only from certain state
  res <- subset(res, State == state)
  
  # test if the state is exists
  if (is.na(res[1, 1])) {
    stop("invalid state")
  }
  
  # order
  res <-  res[order(res$Rate, res$Name), ]
  
  res[1, 1]
}