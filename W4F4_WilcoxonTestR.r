#Nonparametric tests in R   Mann Whitney    Wilcoxon
dengue.fewer <-c(3000,3200,3500,5068,5679,6200,6300,7020)
scrub.typhus<-c(4400,4500,5900,6839,7561,9047,12300,14000)
?wilcox.test()
wilcox.test(dengue.fewer,scrub.typhus)
t.test(dengue.fewer,scrub.typhus,var.equal=T)


ttest.wilcox.examples <- function(x,y,z,k)
{
subjects <- x
mean1 <- y
mean2 <- z
standarddev <- k
print( c("Number of measurements:   ", x))
print( c("Mean of group 1: ", y))
print( c( "Mean of group 2: ",z))
print( c("Standard deviation:  ", k))
group1 <- rnorm(x, y, k)
group2 <- rnorm(x, z, k)
framedata <- cbind(group1, group2)
print(framedata)
print( list (t.test(group1, group2, var.equal = T), wilcox.test(group1, group2)))
}

set.seed(570)
ttest.wilcox.examples(13,90,100,10)


#try

A <- c(91,91,93,106,97,108,97,105,106,103,105,96,105,95,90,101)
B <- c(90,89,85,99,93,104,89,103,102,95,103,95,105,87,86,101)
t.test(A,B,paired=T)
wilcox.test(A,B, paired=T)


## Run the code below and you will have created a new function in R 
## called compare.hypothesis.tests() that invents random measurements 
## and calculates a number of different hypothesis tests for you to 
## explore and compare.
compare.hypothesis.tests <- function(n,y,z,k)
{
        sample.size <- n
        mean1 <- y
        mean2 <- z
        standarddev <- k
        group1 <- rnorm(n, y, k)
        group2 <- rnorm(n, z, k)
        framedata <- cbind(group1, group2)
        print(framedata)
        print( list 
               (t.test(group1, group2, var.equal = T), 
                t.test(group1, group2, paired=T),  
                t.test(group1, group2, var.equal=T, alternative = "less"),
                t.test(group1, group2, var.equal=T, alternative = "greater"),
                wilcox.test(group1, group2), 
                wilcox.test(group1, group2, paired=T)))
}
results$time[results$team == "B"]
set.seed(570); compare.hypothesis.tests(13,90,100,10)
