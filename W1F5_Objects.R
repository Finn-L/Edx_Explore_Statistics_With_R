#We recommend that you use file/ open script and select this file
#Select code in the script window and press ' ctrl r ' to run the code

#You can also copy text from this file and paste it in R Console
#Or use the command source("W1F5_Objects.r") to run the whole content at once

x <- 3
x <- NULL
x <- c(x,3) #concatenate the item 3 to x
x         #running the code x will return the content of x
x <- c(x,5)
x
a <- c(1,2,3,4,5.5) # numeric vector
b <- c("john","david","kumar","jane","mariam") # character vector
c <- c(TRUE,TRUE,FALSE, T, F) #logical vector
c
c[3]
c[c==T]
a[a>1 & a<4]
which(a>1 & a<4)

#Some ways of creating a sequence
seq(2, 100, 4)
2:6
paste("A", 1:6, sep= "")
rep(b, 5)
rep(letters[1:5], 3)

sample(LETTERS, 5, replace=TRUE)  #the function sample can sample with or without replacement
sample(LETTERS, 5, replace=T)
sample(LETTERS, 5, replace=T) #notice how sample generates a new sequence each time

set.seed(200)  # sometimes it is useful to create random, but reproducible numbers
sample(LETTERS, 5, replace=T)
set.seed(200) 
sample(LETTERS, 5, replace=T)

dice  <- sample(1:6, 50, replace=T)

#explore objects
typeof(dice)
dice[0]
str(dice)
is.vector(dice)
head(dice)
tail(dice)
length(dice)
max(dice)
min(dice)
summary(dice)
dice[5] <- 55
plot(x)
which(x>6)

# R has a matrix object
M <- dice
is.vector(M)
dim(M)<- c(10,5)  # rows, columns
?dim()
is.vector(M)
str(M)
M
fix(M) #the command fix() works in windows

#you can perform matrix operations
TM <- t(M)
TM
"some text" -> M[1,1]
M
# everything has to be the same type, the common possible type for integers and string would be string

#a list can hold a collection of different objects
MyList <- list(M, a, dice)
MyList
MyList[[1]][2,]

# data.frame 
#one row one observation, one column one variable
MyDataFrame <- data.frame(a, b, c)
MyDataFrame
str(MyDataFrame)
MyDataFrame$a
MyDataFrame[1,]

#calculating circle area radius 5
5^2 * pi
radius <- 5
radius^2 *pi

CircleArea <- function(radius) {
radius*radius*pi
}

CircleArea(5)
a
CircleArea(a)

plot(a, CircleArea(a))
#explore the function by typing
CircleArea
