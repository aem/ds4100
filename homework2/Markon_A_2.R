loadDataIfNeeded <- function() {
  # Loads the data from the acquisitions CSV
  # Assumes the CSV is located in the current working directory
  # Args:
  #   None
  # Returns: None
  if (!exists("acquisitions")) {
    acquisitions <<- read.csv("acquisitions.csv", header=TRUE)
    acquisitions[,1] <<- as.Date(acquisitions[,1], format="%m/%d/%Y")
  }
}

dateVectorDiff <- function(dates, last=NA) {
  # Calculates and returns the differences between each date in a vector of dates
  # Args:
  #   dates [Vector<Date>]: the vector of dates to diff
  #   last [Date]: the terminal value to use as a diff for the last element
  #                in the vector. defaults to NA
  # Returns: [Vector<Date>] the vector of differences between successive elements
  if (class(dates) != "Date") {
    stop("Given vector is not of class 'Date,' cannot process")
  }
  count <- length(dates)
  result <- vector('numeric', length=count)
  for(i in 1:count) {
    if (i == count) {
      ifelse(is.na(last), result[i] <- NA, result[i] <- as.numeric(difftime(last, dates[i]), units="days"))
    } else {
      result[i] <- as.numeric(difftime(dates[i+1], dates[i]), units="days")
    }
  }
  return(result)
}

leastInterval <- function() {
  # Calculates the lowest interval in between acquisitions in the given data set
  # Args:
  #   None
  # Returns: [Numeric] the lowest interval between acquisitions
  loadDataIfNeeded()
  diffs <- dateVectorDiff(acquisitions[,1])
  return(min(diffs, na.rm=TRUE))
}