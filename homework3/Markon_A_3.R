%m%drequire("lubridate")

loadDataIfNeeded <- function() {
  # Loads the data from the acquisitions CSV
  # Assumes the CSV is located in the current working directory
  # Args:
  #   None
  # Returns: None
  if (!exists("strikes")) {
    strikes <<- read.csv("Bird\ Strikes.csv", header=TRUE)
    strikes$FlightDate <<- as.Date(strikes$FlightDate, format="%d/%m/%Y")
    strikes$FlightYear <<- year(as.Date(strikes$FlightDate, format="%d/%m/%Y"))
  }
}
loadDataIfNeeded()

YearWithMostBirdStrikes <- function() {
  # Returns the year which had the most bird strikes
  # Args:
  #   None
  # Returns: [character] the correct year
  # `table` function produces a sortable table of frequencies
  # `names` just returns the year as opposed to a table with both the name and the frequency
  return(names(sort(table(strikes$FlightYear), decreasing=TRUE)[1]))
}

GetBirdStrikeFrequencies <- function() {
  # Gets the frequency of bird strikes by year
  # Args:
  #   None
  # Returns: [Data Frame] the frequency of bird strikes for each year
  return(as.data.frame(table(strikes$FlightYear)))
}

GetAirlineWithMostStrikes <- function() {
  # Gets the airline with the most bird strikes
  # Args:
  #   None
  # Returns: [Character] the name of the airline with the most bird strikes
  AirlineStrikes <- as.data.frame(sort(table(strikes$Aircraft..Airline.Operator), decreasing=TRUE))
  return(AirlineWithMostStrikes(AirlineStrikes))
}

AirlineWithMostStrikes <- function(AirlineStrikes) {
  # Takes the sorted data frame of airline strikes and filters out bad
  # categories, returning the top result
  # Args:
  #   AirlineStrikes [Data Frame]: the data frame containing the frequencies of bird strikes
  # Returns: [Character] the airline with the greatest frequency
  return(
    as.character(
      AirlineStrikes[which(AirlineStrikes[,1] != "UNKNOWN" &
                             AirlineStrikes[,1] != "BUSINESS" &
                             AirlineStrikes[,1] != "MILITARY"), 1:2][1,1]
      )
    )
}

print("original set")
system.time(GetAirlineWithMostStrikes())
print("2x data set")
strikes <- rbind(strikes, strikes)
system.time(GetAirlineWithMostStrikes())
print("4x data set")
strikes <- rbind(strikes, strikes)
system.time(GetAirlineWithMostStrikes())
# the time of the operations took time linearly proportional to the size of the data set, 
# which was expected 