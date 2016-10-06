library('openxlsx')
library('stringi')
library('reshape2')
require('lubridate')

coordinates <- read.xlsx("2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", startRow=3)

# use these to determine which start months qualify which seasons
winterStartMonths <- c(month.name[11], month.name[12], month.name[1])
springStartMonths <- c(month.name[2], month.name[3], month.name[4])
summerStartMonths <- c(month.name[5], month.name[6], month.name[7])

subtractMonths <- function(m1, m2) {
  # Finds the difference between two months 
  # Args:
  #   m1, m2 [Character]: the full name of a month
  # Returns: [Numeric] the number of months between the two given months
  return(match(m1, month.name) - match(m2, month.name))
}

getSeasonFromStartMonth <- function(month) {
  # Find the corresponding season based on the start month of a multi-month period
  # Args:
  #   month [Character]: the month to find the season of
  # Returns: [Character] the season following the given month
  ifelse(!is.na(match(month, winterStartMonths)),
         return('Winter'),
         return(ifelse(!is.na(match(month, springStartMonths)),
                'Spring',
                ifelse(!is.na(match(month, summerStartMonths)),
                       'Summer',
                       'Fall'))))
}

convertToRangesAndNormalize <- function(dates) {
  # Given a vector of date ranges with dates in the form of "Date to Date", converts the range to
  # a season or range of seasons (Winter, Spring, Summer, Fall, Half-Year, Full-Year)
  # Args:
  #   dates [Vector]: the dates to bin
  # Returns: [Vector] the normalized dates
  split <- colsplit(string=dates, pattern=" to ", names=c('start', 'end'))
  split$start <- ifelse(
    (is.na(as.Date(split$start, format='%m/%d/%Y')) & is.na(as.Date(split$start, format='%B %d, %Y'))),
    split$start,
    ifelse(is.na(as.Date(split$start, format='%m/%d/%Y')),
           month.name[month(as.Date(split$start, format='%B %d, %Y'))],
           month.name[month(as.Date(split$start, format='%m/%d/%Y'))])
  )
  split$end <- ifelse(
    (is.na(as.Date(split$end, format='%m/%d/%Y')) & is.na(as.Date(split$end, format='%B %d, %Y'))),
    split$end,
    ifelse(is.na(as.Date(split$end, format='%m/%d/%Y')),
           month.name[month(as.Date(split$end, format='%B %d, %Y'))],
           month.name[month(as.Date(split$end, format='%m/%d/%Y'))])
  )
  split$range <- subtractMonths(split[,2], split[,1]) + 1
  split$normalized <- 
    ifelse(is.na(split$range),
          NA,
          ifelse(split$range >= 10,
                 'Year-Round',
                 ifelse(split$range >= 6,
                        'Half-Year',
                        getSeasonFromStartMonth(split$start))))
  return(split$normalized)
}

standardizeDates <- function() {
  # Normalize ranges of dates from the coordinates data set
  # Args:
  #   None
  # Returns: None
  coordinates$Season1Normalized <<- convertToRangesAndNormalize(coordinates$Season1Date)
}

acceptsWIC <- function() {
  # Get a data frame of all markets that accept WIC
  # Args:
  #   None
  # Returns: [Data Frame] the markets which accept WIC
  return(coordinates[which(!is.na(coordinates$WIC) & coordinates$WIC == 'Y'),])
}

acceptsWIC()