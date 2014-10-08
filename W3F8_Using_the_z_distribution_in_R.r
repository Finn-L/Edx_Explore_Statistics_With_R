#SC StatRx_W3_F8_Using the z distribution in R

?dnorm()  #Density, distribution function, quantile function and random generation 

#You will find the same basic functions for other distributions you may need, such as
?dt() ; ?dpois() ; ?dbinom() ; ?dchisq()

#Draw a z curve, a density function for z

#Create a sequence of numbers for your x axis
x<-seq(-4, 4, 0.01) 

#plot for each x the probability to draw x at random from the distribution
plot(x, dnorm(x), type="l")

#You can figure out what the attribute type= does by removing it 
plot(x, dnorm(x))
#Or by looking up ?plot()

#Distribution function, or Cumulative distribution function.
#For each x, what is the probability to draw x or a value lower than x
plot(x, pnorm(x))

#Quantile function is when you ask what value for z is at the 25% percentile. 
qnorm(0.25)
#You may want to find the interval of values for z that includes 95% of the distribution.
#Useful for calculating confidence intervals 
qnorm(c(0.025, 0.975))

#I often round the results, not for calculations but foe presentation.
round(qnorm(c(0.025, 0.975)),2)

#I find it very useful to draw graphs to enchance understanding.
#You will practice using the functions polygon() and text() to add clarity to your graphs.

#z-tests
bt <- seq(60, 120, 1) 
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="blood pressure") 

#Let's use the normal distribution for a statistical test.

#one tailed test
bt <- seq(60, 120, 1) 
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="one tailed test") 
pnorm(72, 90, 10) # probability of randomly selecting a subject bt 72 or lower
abline(v=72) # Draw a line for 72 . v is the x-value for a vertical line 
cord.x <- c(60,seq(60,72,1),72) 
cord.y <- c(0,dnorm(seq(60, 72, 1), 90, 10),0) 
polygon(cord.x,cord.y,col='skyblue') 
text(70, 0.005, "blue area = p = 0.0359") 

#two-tailed test
bt <- seq(60, 120, 1) 
plot(bt, dnorm(bt, 90, 10), type="l", xlim=c(60, 120), main="two-tailed test") 
pnorm(72, 90, 10)  
abline(v=72) 
cord.x <- c(60,seq(60,72,1),72) 
cord.y <- c(0,dnorm(seq(60, 72, 1), 90, 10),0) 
polygon(cord.x,cord.y,col='skyblue') 
cord.x1 <- c(108,seq(108,120,1),120) 
cord.y1 <- c(0,dnorm(seq(108, 120, 1), 90, 10),0) 
polygon(cord.x1,cord.y1,col='skyblue') 
text(65, 0.005, round(pnorm(72, 90, 10), 3)) 
text(115, 0.005, round(pnorm(72, 90, 10), 3)) 
text(75, 0.02,  " p = 0.072 "  ) 
