require('RMySQL')

databaseSetup <- function() {
  # assumes there is a running MySQL server at 127.0.0.1 with the passwordless user `root`
  assign('baseballDb', dbConnect(MySQL(), user='root', password='root', host='localhost'), .GlobalEnv)
  dbSendQuery(baseballDb, 'drop database if exists baseball_history')
  dbSendQuery(baseballDb, 'create database baseball_history')
  dbSendQuery(baseballDb, 'use baseball_history')
  processData()
}

processData <- function() {
  # extracted from history of baseball: https://www.kaggle.com/seanlahman/the-history-of-baseball
  raw <- read.csv('pitching.csv')
  # we only want to look at modern era pitchers. looking at pre-1919 pitchers will
  # skew statistics because the deadball era will skew statistics drastically
  # also only use pitchers with more than 25 innings pitched that season. any less
  # and the ERAs go too high to get any statistically significant data
  modernEra <- raw[which(raw$year >= 1919), c(-21, -28, -29, -30)]
  ifelse(modernEra$league_id == 'AL', modernEra$league_id <- 0, modernEra$league_id <- 1)
  mean <- mean(modernEra$era, na.rm=TRUE)
  sd <- sd(modernEra$era, na.rm=TRUE)
  modernEra$eraDev <- round(abs(modernEra$era - mean / sd), digits=4)
  modernEra <- modernEra[which(modernEra$eraDev < 28),]
  dbWriteTable(baseballDb, 'pitchers', modernEra)
}

drawHistogram <- function() {
  eraData <- fetch(dbSendQuery(baseballDb, 'select era from pitchers'), n=-1)
  maxEra <- fetch(dbSendQuery(baseballDb, 'select max(era) from pitchers'), n=-1)[1,1]
  minEra <- fetch(dbSendQuery(baseballDb, 'select min(era) from pitchers'), n=-1)[1,1]
  breaks <- seq(from=0, to=round(maxEra) + 1, by=1)
  hist(eraData[,1], breaks=breaks,right=FALSE)
}

# list of factors that will likely be significant in the regression model
# abstract out to make for a more readable SQL command
initialFactors <- c(
  'era',
  'stint',
  'w', 'l',
  'h', 'hr', 'bb', 'so', 'baopp',
  'wp', 'hbp', 'bk'
)

# perform our multiple regression to get our initial model
eraData <- fetch(dbSendQuery(baseballDb, paste(paste('select ', paste(initialFactors, collapse=", ")), ' from pitchers')), n=-1)
model <- lm(eraData$era ~ eraData$stint +
              eraData$w + eraData$l +
              eraData$h + eraData$hr + eraData$bb + eraData$so +
              eraData$baopp + eraData$hbp + eraData$wp + eraData$bk, eraData)
summary(model)
# only one insignificant factor above, perform our multiple
# regression to get our ideal model
model <- lm(eraData$era ~ eraData$stint +
              eraData$w + eraData$l +
              eraData$h + eraData$hr + eraData$bb + eraData$so +
              eraData$baopp + eraData$hbp + eraData$bk, eraData)
summary(model)

# Predict ERAs using model and calculate MAD
eraData <- fetch(dbSendQuery(baseballDb, 'select * from pitchers'), n=-1)
eraData$predictedEra <- -0.6920283 + (-0.2554623 * eraData$stint) +
  (-0.0198174 * eraData$w) + (0.0281146 * eraData$l) +
  (-0.0166025 * eraData$h) + (0.0956588 * eraData$hr) +
  (0.0138591 * eraData$bb) + (-0.0082326 * eraData$so) +
  (24.1549309 * eraData$baopp) + (0.0238585 * eraData$hbp) +
  (-0.0371418 * eraData$bk)
eraData$eraPredDev <- abs(eraData$era - eraData$predictedEra)
mean(eraData$eraPredDev, na.rm=TRUE)

# simpler regression model solely based on earned runs and innings pitched
eraData <- fetch(dbSendQuery(baseballDb, 'select era, er, ipouts from pitchers'), n=-1)
model <- lm(eraData$era ~ eraData$er + eraData$ipouts, data=eraData)
summary(model)

# predict ERAs using new model with MAD
eraData$predictedEra <- 5.5046238 + (0.0905811 * eraData$er) + (-0.0159866 * eraData$ipouts)
eraData$eraPredDev <- abs(eraData$era - eraData$predictedEra)
mean(eraData$eraPredDev, na.rm=TRUE)
