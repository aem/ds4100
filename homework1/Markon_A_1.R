# I opted to read the data as a text file instead of a zip file for convenience
# and code clarity. This syntax is much more concise than uncompressing the file
# and then reading it
loadDataIfNeeded <- function() {
  if (!exists("delays")) {
    delays <<- read.table("AirlineDelays.txt", header=TRUE, sep=",")
  }
}

TotalNumDelays <- function(carrier) {
  # Counts the total number of departure delays for a carrier
  # Only delay values > 0 are counted, as a negative delay value
  # is actually a flight arriving early
  # Args:
  #   carrier [string]: the airline's carrier code
  # Returns: [numeric] the total number of delays
  loadDataIfNeeded()
  return(length(which((delays[,6] > 0 | delays[,7] > 0) & delays[,3] == carrier)))
}

TotalDelaysByOrigin <- function(origin) {
  # Counts the total number of delays for flights originating form an airport
  # Only delay values > 0 are counted, as a negative delay value
  # is actually a flight arriving early
  # Args:
  #   origin [string]: the origin airport's code
  # Returns: [numeric] the total number of flight delays from an airport
  loadDataIfNeeded()
  return(length(which((delays[,6] > 0 | delays[,7] > 0) & delays[,4] == origin)))
}

AvgDelay <- function(carrier, dest) {
  # Calculates the average arrival delay for a carrier's flights to an airport
  # - NA values are not calculated in the average, as a flight that arrives on time
  #   should not factor into the average
  # - Values are rounded to 2 decimal points, more than 2 decimal points when
  #   calculating times in minutes is a trivial difference
  # Args:
  #   carrier [string]: the airline's carrier code
  #   dest [string]: the arrival airport code
  # Returns: [numeric] the average arrival delay
  loadDataIfNeeded()
  return(round(mean(delays[which(delays[,3] == carrier & delays[,5] == dest),7], na.rm=TRUE), digits=2))
}

TotalNumDelays("DL")  # 31019

TotalDelaysByOrigin("JFK")  # 4612

AvgDelay("AA", "JFK") # 5.96
