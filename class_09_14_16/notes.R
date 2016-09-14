if (sum(1:10) >= sqrt(5)) {               # control flow looks like any other language
  print('hello!')
}
ifelse(4 > 5, "greater", "less")          # ternary represented as ifelse function

nums <- 1:4
for (num in nums) {                       # python-like for loops
  print(num)
}
x <- 1
while (x < 5) {                           # while loops yay
  print(x)
  x <- x + 1
}
num                                       # [1] 4      !!!!! no block-level scoping in R

mode(x)                                   # determine data type using mode()
ifelse(mode(x) == "numeric", "y", "n")    # can use in comparisons to guarantee type safety
list <- c(1, 2, TRUE, FALSE)
mode(list)                                # [1] "numeric"    wait what?
list                                      # [1] 1 2 1 0    god dammit, type casting
is.numeric(list)                          # [1] TRUE
list <- c(1, 2, 'a', 'b')
mode(list)                                # [1] "character"   don't even wanna know
list                                      # [1] "1" "2" "a" "b"   jesus christ this is worse than javascript
# something something factors
toConvert <- c(-99, 0, -99)
as.logical(toConvert)                     # [1] TRUE FALSE TRUE   ok this is kinda cool
toConvert <- c("1", "3", "five", "7")
as.numeric(toConvert)                     # [1] 1 3 NA 7    dumb but makes sense
# vector != array, vector must be uniform data type

list <- list(1, "hey", TRUE)              # bag
list                                      # no casting
mode(list)                                # [1] list
class(list)                               # [1] list

d1 <- as.Date('2016-09-14')                # no concept of time
as.numeric(d1)                             # [1] 17058    number of days in unix time
d2 <- as.Date(                             # format strings work properly
  "Dec-27-2014",
  format="%b-%d-%Y"
)
as.numeric(d2)                             # 16431
?strptime                                 # time help
# look into lubridate package
d2-d1                                      # "Time difference of -627 days"   neat
