rankall <- function(outcome, num = "best") {
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
  
  # list all unique states
  list_state <- unique(res$State)
  
  # initialize empty dataframe
  df <- data.frame(Name=character(), State=character())
  
  for (item in list_state) {
    temp <- subset(res, State == item)
    
    if (num == "best") {
      temp <- temp[order(temp$Rate, temp$Name), ]
      df <- rbind(df, temp[1, c(1, 2)])
    } else if (num == "worst") {
      temp <- temp[order(-temp$Rate, temp$Name), ]
      df <- rbind(df, temp[1, c(1, 2)])
    } else {
      temp <- temp[order(temp$Rate, temp$Name), ]
      if (is.na(temp[num, 2])) temp[num, 2] <- item
      df <- rbind(df, temp[num, c(1, 2)])
    }
  }
  
  df[order(df$State), ]
}