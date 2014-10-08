#Let's start exploring R
#Make sure you start R and follow along
#Make some changes to the code to check if you have understood.

5+5
6*7
4^3

a <- 4+56  #assign character
a
a <- c(1,2,3,4,5,7,8,10,11) #the combine function c()
a
a + 12  # useful vector operation
a
b <- a+2 
#lets talk about functions
plot(a)
plot(a,b)  

#built in statistical functions, often you can guess their names
mean(a)  #arithmetic mean
median(b)  #median
sd(a)  #standard deviation

#also try 
max(a)
min(a)

#explore unknown code
hist(sort(rnorm(10000)))
#the innermost function seems to be rnorm(10000), lets run that one
rnorm(10000)
x <- rnorm(10000) #each time you run rnorm() new random values will be created. You will not get teh same values that I got in the video.
mean(x)  
sd(x)
#Use the help functions in R
?plot()
example(plot)

