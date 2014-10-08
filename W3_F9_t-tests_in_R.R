#StatRx_W3_F9_t-tests in R 

#This video will help you get started calculating t-tests in R.
#You will also get access to unlimited exercises for interpreting t tests.
#You will be using the function t.test()

?t.test()
weight <- read.table("clipboard", header=T)
t.test(weight)
t.test(weight$A, weight$B)
t.test(weight$A, weight$B, var.equal=T)
t.test(weight$A, weight$B, paired=T)

#A function for creating random examples of t-tests
ttest.for.examination <- function(x,y,z,k)
{
        subjects <- x
        mean1 <- y
        mean2 <- z
        standarddev <- k
        print( c("Number of measurements:   ", x))
        print( c("Mean of group 1: ", y))
        print( c( "Mean of group 2: ",z))
        print( c("Standard deviation:  ", k))
        group1 <- round(rnorm(x, y, k))
        group2 <- round(rnorm(x, z, k))
        framedata <- cbind(group1, group2)
        print(framedata)
        print( list (t.test(group1, group2, var.equal = T), t.test(group1, group2, var.equal = T, paired = T)))
}
#Now you can run the new function.
ttest.for.examination(13,90,105,10)
ttest.for.examination(13,92,100,10)
#Try running the same arguments twice
ttest.for.examination(13,92,100,10)
#How can you use set.seed() to get reproducible "random" t-tests?