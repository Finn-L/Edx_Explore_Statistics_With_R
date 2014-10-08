set.seed(896); 
run<- data.frame(time=rnorm(50,mean=50,sd=10),training.method=rep(letters[1:5],each=10));  
summary(aov(run$time~run$training.method)) 
## Here we introduce a noticeable performance increase for group A and run the ANOVA again
run[1:10,1] <- run[1:10,1] - 15
summary(aov(run$time~run$training.method))

## Imagine we only had access to rank order between the runners.
kruskal.test(run$time~run$training.method)

## explore the ANOVA results table
summary(aov(run$time~run$training.method))

count <- 0
variances <- NULL
for (i in 1:5){
        count <- count + 1
        variances <- c(variances, var(run$time[run$training.method==letters[count]]))
}
variances
sum(variances)/5


count <- 0
means <- NULL
for (i in 1:5){
        count <- count + 1
        means <- c(means, mean(run$time[run$training.method==letters[count]]))
}
means
var(means) * 10

## Spearman's rank correlation coefficient
#Create some data
set.seed(278)
x <- rnorm(25, mean=100, sd=10)
y <- 2 * x + 20 + rnorm(25, mean=10, sd=4)
cor(x,y)  #If you just want the correlation coefficient
cor(x,y)^2 #Or the coefficient of determination

## If your data is on ordinal scale you can calculate a non-parametric 
## alternative called  Spearman's rank correlation coefficient. 
cor(x,y, method="spearman")
cor(x,y, method="spearman")^2

## Sometimes lack of knowledge about distributions be compensated with computational power
set.seed(570)
group1 <- rnorm(13,90, 10)
group2 <- rnorm(13,100,10)
#So let’s pool the values in one group for a while
group <- c(group1,group2)

#We sample new random groups from the same data, with replacement
dif <- numeric(10000)
for(i in 1:10000){
        dif[i]<-mean(sample(group,13,replace=T))-mean(sample(group,13,replace=T))
}
#Calculate mean difference between the random groups 10000 times and plot a histogram

hist(dif)

#We can construct an empirical cumulative distribution function using ecdf()
#In this case ‘empirical’ means: we have no theory, we figured out by experimentation

Fn <- ecdf(dif)

#Draw a plot of cumulated probabilities to draw a difference in means equal to or smaller than x

plot(Fn)

#Let’s go back to our measured difference from the t.test excercise

measured.dif <- mean(group1)-mean(group2)

#Use Fn to look up how many resamplings were like measured.dif or smaller

Fn(measured.dif)

#Can you see how this corresponds to the p-value of a one-tailed test?
#Look at this result:

t.test(group1,group2, var.equal=T)$p.value

#Let’s make a one-tailed test for those who don’t like dividing by 2  ;)

t.test(group1,group2, var.equal=T, alternative = "less" )$p.value
