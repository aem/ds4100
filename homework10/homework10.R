require('openxlsx');

uffidata <- read.xlsx('uffidata.xlsx')[1:96,2:3]

yearData <- aggregate(uffidata, list(uffidata[,1]), mean)[,c(2,3)]

weightedMovingAverage <- function(data) {
  # Calculate the predicted average 2017 home price using a WMA model
  # Args:
  #   data [Data.Frame]: data for average home price each year
  # Returns: [List] a list of the full data frame, MAD, and confidence interval bounds
  for(i in 1:3) {
    data[i,3] <- data[i,2]
    data[i,4] <- abs(data[i,2] - data[i,3])
  }
  for(i in 4:(1 + nrow(data))) {
    data[i,3] <- ((data[i-1,2] * 3 + data[i-2,2] * 2 + data[i-3,2]) / 6)
    data[i,4] <- abs(data[i,2] - data[i,3])
  }
  data[nrow(data),1] <- '2017'
  mad <- mean(data[4:(nrow(data)-1),4])
  bottom <- data[nrow(data),3] - 1.96*mad*0.8
  top <- data[nrow(data),3] + 1.96*mad*0.8
  return(list('data'=data, 'MAD'=mad, 'bottom'=bottom, 'top'=top))
}

exponentialSmoothingModel <- function(data, constant) {
  # Calculate the predicted average 2017 home price using aan exponential smoothing model
  # Args:
  #   data [Data.Frame]: data for average home price each year
  #   constant [Number]: the smoothing constant between 0 and 1
  # Returns: [List] a list of the full data frame, MAD, and confidence interval bounds
  data[1,3] <- data[1,2]
  data[1,4] <- abs(data[1,2]-data[1,3])
  for(i in 2:(1+nrow(data))) {
    data[i,3] <- data[i-1,3] + constant * (data[i-1,2] - data[i-1,3])
    data[i,4] <- abs(data[i,2] - data[i,3])
  }
  data[nrow(data),1] <- '2017'
  mad <- mean(data[2:(nrow(data)-1),4])
  bottom <- data[nrow(data),3] - 1.96*mad*0.8
  top <- data[nrow(data),3] + 1.96*mad*0.8
  return(list('data'=data, 'MAD'=mad, 'bottom'=bottom, 'top'=top))
}

linearModel <- function(data) {
  # Calculate the predicted average 2017 home price using a linear model
  # Args:
  #   data [Data.Frame]: data for average home price each year
  # Returns: [List] a list of the full data frame, MAD, and confidence interval bounds
  c <- coefficients(lm(Sale.Price ~ Year.Sold, data=yearData))
  intercept <- c[[1]]
  slope <- c[[2]]
  data[nrow(data) + 1, 1] <- 2017
  for(i in 1:(nrow(data))) {
    data[i,3] <- intercept + slope * data[i,1]
    data[i,4] <- abs(data[i,2] - data[i,3])
  }
  mad <- mean(data[1:(nrow(data)-1),4])
  bottom <- data[nrow(data),3] - 1.96*mad*0.8
  top <- data[nrow(data),3] + 1.96*mad*0.8
  return(list('data'=data, 'MAD'=mad, 'bottom'=bottom, 'top'=top))
}

findBestConstant <- function() {
  # Calculate the ideal smoothing constant between 0 and 1 form the smoothing model
  # Args:
  #   None
  # Returns: [Number] the ideal smoothing constant
  lowestMad <- exponentialSmoothingModel(yearData, 0)[[2]]
  constant <- 0
  for(i in seq(from=0, to=1, by=0.05)) {
    mad <- exponentialSmoothingModel(yearData, i)[[2]]
    if (mad < lowestMad) {
      lowestMad <- mad
      constant <- i
    }
  }
  return(constant)
}

