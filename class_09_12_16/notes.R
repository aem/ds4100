x <- 99                          # no typedef
x                                # 99
x <- "text"                      # can reassign across data types
x                                # "text"


fraction <- function(x, y) {     # c-like function definitions
  result <- x / y                # standard math operators
  print(result)
}
fraction(2, 3)                   # function calls are c-style as well


a <- c(1, 2, 3, 4)               # array builders are easy
a                                # [1] 1 2 3 4
b <- a + 1                       # array-level mathematical operations
b                                # [1] 2 3 4 5 


ls()                             # list all values (output: [1] x, fraction, a, b)
rm(x)                            # remove value x
#x                                # Error: object 'x' not found


1:10                             # sequences are easy
x <- 2 * (1:5)                   # sequence math, just like arrays
x                                # [1] 2 4 6 8 10
# sequence function is dope
seq(from=2, to=5, by=1)          # [1] 2 3 4 5
seq(from=2, to=5, length=6)      # [1] 2.0 2.6 3.2 3.8 4.4 5.0
seq(from=100, length=4, by=-2.5) # [1] 100.0 97.5 95.0 92.5


x <- seq(from=100, to=500, by=33)
x                                # [1] 100 133 166 199 232 265 298 331 364 397 430 463 496
x[3]                             # 166  !!! NOT ZERO-INDEXED
x[-3]                            # [1] 100 133 199 232 265 298 331 364 397 430 463 496 | exclusive arrays
x[c(5, 7)]                       # [1] 232 298  |  multiple element access 
x[-c(5, 7)]                      # [1] 100 133 166 199 265 331 364 397 430 463 496  |  this is awesome
length(x)                        # standard array methods (output: 13)
x <- 1:5  
x > 2                            # [1] FALSE FALSE TRUE TRUE TRUE
x[x > 2]                         # [1] 3 4 5  | stupid simple filtering
  
  
rm(list=ls())                    # clean up environment


x <- 1:10
y <- seq(from=100, to=300, by=5)
#df <- data.frame(x, y)           # !!! Dot is a part of the name, not a method access!!
                                 # Error in data.frame(x, y) : 
                                 #     arguments imply differing number of rows: 10, 41
x <- 1: length(y)  
df <- data.frame(x, y)           # kind of a 2-D array, but you don't need monogamous data types
df                               # columns must be data monogamous, but the whole data frame doesn't
df[5, 2]                         # row 5, column 2 (output: 120)
df[2, 5]                         # NULL
df[5,]                           #    x    y
                                 # 5  5  120
df$x                             # access column named "x"
ncol(df)                         # [1] 2  |  columns
nrow(df)                         # [1] 41  |  rows 
dim(df)                          # [1] 41 2
length(df$x)                     # [1] 41


x <- 1:10
x
any(x < 0)                       # [1] FALSE
any(x > 5)                       # [1] TRUE
any(x < 0 | x > 10)              # [1] FALSE    !!! logical "or" is only 1 bar
any(x < 0 | x > 9)               # [1] TRUE


mean(y[-3])                      # [1] 202.25
round(mean(y[-3]))               # [1] 202
round(mean(y[-3]), digits=1)     # [1] 202.2
max(y)                           # [1] 300
which(df[,2] > 275)              # [1] 37 38 39 40 41
length(which(df[,2] > 275))      # [1] 5  |  "how many rows have a y value greater than 275?"
df[which(df[,2] > 275),]         #     x   y
                                 # 37 37 280
                                 # 38 38 285
                                 # 39 39 290
                                 # 40 40 295
                                 # 41 41 300
summary(df[,2])                  #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                 #    100     150     200     200     250     300 
colSums(df[2])                   # 8200

aq <- head(airquality)
mean(aq$Solar.R)                 # [1] NA
any(is.na(aq))                   # [1] TRUE
mean(aq$Solar.R,na.rm=TRUE)      # [1] 192.5
which(is.na(aq))                 # [1] 5 11 12
