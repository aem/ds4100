library('XML')
library('stringi')

senators <- xmlToDataFrame('http://www.xmldatasets.net/temp/179681356453762.xml')

senatorName <- function(state) {
  # Finds the name of senators from the given state
  # Args:
  #   state [Character]: the state to search
  # Returns: [Character] the names of the senators
  return(senators[which(senators$state == state), c(2, 3)])
}

senatorPhone <- function(name) {
  # Finds the phone number of the given senator
  # Args:
  #   name [Character]: the name of the senator to look up
  # Returns: [Character] the senator's phone number
  parts <- unlist(stri_split(name, regex=' '))
  last <- parts[length(parts)]
  first <- paste(parts[1:(length(parts) - 1)], collapse=' ')
  return(senators[which(senators$last_name == last & senators$first_name == first), 7])
}