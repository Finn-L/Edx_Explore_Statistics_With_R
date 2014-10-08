x <- 0:2
plot(x, dbinom(x, 2, 0.5), type = "h", col = "blue", lwd=4, ylim= c(0,0.6)) 
curve(dnorm(x, 1, 0.8), add=T)


## showing probabilities of possible outcomes when tossing a fair coin 
## eight times
x <- 0:8
plot(x, dbinom(x, 8, 0.5), type = "h", col = "blue", lwd=4) 
curve(dnorm(x, 4, 1.5), add=T)

## Create three different distributions and draw histograms of them using the code below:
        
set.seed(400)
NORMAL <- rnorm(10000)
UNIFORM <- runif(10000)
SKEWED <- rep(1:140, 1:140)
opar <- par() #Save original par settings. Read ?par() if you like
par(mfrow= c(3,1)) #Ask for three columns and one row in the graph
hist(NORMAL)
hist(UNIFORM)
hist(SKEWED)
par(opar) #Reset the graph parameters
#Reset can also be achieved by closing the Graphics window

## Calculate the mean and the median of NORMAL, UNIFORM and SKEWED

## The code draws 1000 samples of sizes n=3, n=6 and n=300 from NORMAL. 
## It draws revealing histograms, and calculates means and standard deviations 
## of the sampling distributions.

par(mfrow= c(3,1)) #Ask for three columns and one row in the graph
sampl <- vector() #Create an empty vector
for(i in 1:1000) #Start a loop with 1000 rounds
        sampl <- c(sampl, mean(sample(NORMAL, 3, replace=T)))
#fill sampl with sampl, and the mean of three random items from NORMAL
mean(sampl) 
sd(sampl)
hist(sampl, xlim = c(-2, 2), main = " n = 3 " )
sampl <- vector()
for(i in 1:1000)
        sampl <- c(sampl, mean(sample(NORMAL, 6, replace=T)))
mean(sampl)
sd(sampl)
hist(sampl, xlim = c(-2, 2), main = " n = 6 " )
sampl <- vector()
for(i in 1:1000)
        sampl <- c(sampl, mean(sample(NORMAL, 300, replace=T)))
mean(sampl)
sd(sampl)
hist(sampl, xlim = c(-2, 2), main = " n = 300 " )


## -Easy to read and understand, with rich comments.

## -Short and elegant.

## -Sometimes must be quick to execute.
x <- rnorm(100000) 
y <- rnorm(100000) 
z <- rep(NA, 100000) #z is created empty but with a given size. 
system.time({ 
        for (i in 1:100000) { 
                z[i] <- x[i] + y[i] 
        } 
}) 
system.time( k <- x + y ) 


## ME has a format that gene expression data downloaded from the GEO database 
## could have at some point of your analysis. Note that he values are not 
## carefully modeled to look like gene expresssion data. The purpose of 
## generating the dataset  is to practice how to calculate data spread over 
## several columns or rows in a large matrix.
set.seed(897)
ME <- matrix(rnorm(24000),nrow=1000)
colnames(ME) <- c(paste("A",1:12,sep=""),paste("B",1:12,sep=""))

## 1. How many data points are greater than or equal to zero? 
length(which(ME<=0))

## 2. See how apply() can be used to repeat operations over rows in a matrix. 
## Create a logical vector 'keep' that stores TRUE for every row with mean>0 
## for both groups A and B. 
keep <- (apply(ME[,1:12],1,mean) > 0) & (apply(ME[,13:24],1,mean) > 0)
length(keep)
sum(keep) #make sure you understand sum() applied to logicals  
head(keep)

## 3. Here you can see useful code for passing more arguments to the function mean() 
## when you use it inside apply().  For this purpose you create the function(ME)
## Read about the argument trim in the documentation ?mean()
## The code stores the trimmed means of every ME row in the vector 'trimmed'.
trimmed <- apply(ME,1,function(ME){mean(ME, trim=0.05)})

## Use apply() to calculate a statistical test for every row in ME. 
## You want to ask whether the groups A and B are from the same population or 
## from populations with different means. You can assume data to be normally 
## distributed. You can assume equal variance in the groups.

## Count the number of rows with a p-value equal to, or lower than 0.05.
track <- c()
sum(apply(ME, 1, function(ME){
        (t.test(ME[1:12], ME[13:24], var.equal = T)$p.value <= 0.05)
                          }
      ))