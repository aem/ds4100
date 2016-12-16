require('RCurl')
require('XML')

moreFiveBids <- function() {
  # Determine which eBay products from the dataset have more than 5 bids
  # Args:
  #   None
  # Returns: [Numeric] the number of given products which saw more than 5 bids
  rawXml <- xmlTreeParse('http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/auctions/ebay.xml')
  ebayList <- xmlToList(rawXml)
  result <- 0
  for (i in 1:(round(length(ebayList) / 7))) {
    if (as.numeric(ebayList[[((i - 1) * 7) + 5]]$num_bids) > 5) {
      result <- result + 1
    }
  }
  return(result)
}

loadTrading <- function() {
  # Load the trading data into the global scope
  # Args:
  #   None
  # Returns: None
  rawXml <- xmlTreeParse('http://www.barchartmarketdata.com/data-samples/getHistory15.xml')
  tradingList <<- xmlToList(rawXml)
  tradingList[[1]] <<- NULL  # first element is a 200 status code
}
loadTrading()

highestClosingPrice <- function() {
  # Determine the highest closing price from the data set
  # Args:
  #   None
  # Returns: [Numeric] the highest closing price
  max <- 0
  for(i in 1:length(tradingList)) {
    if (as.numeric(tradingList[[i]]$close) > max) {
      max <- as.numeric(tradingList[[i]]$close)
    }
  }
  return(max)
}

totalVolume <- function() {
  # Determine the total trading volume of the given data set
  # Args:
  #   None
  # Returns: [Numeric] the total trading volume
  total <- 0
  for(i in 1:length(tradingList)) {
    total <- total + as.numeric(tradingList[[i]]$volume)
  }
  return(total)
}

averageVolume <- function() {
  # Determine the average volume of trades throughout the dat
  # Args:
  #   None
  # Returns: [Numeric] the average trading volume
  return(totalVolume() / length(tradingList))
}

highestClosingPrice()
totalVolume()
averageVolume()
