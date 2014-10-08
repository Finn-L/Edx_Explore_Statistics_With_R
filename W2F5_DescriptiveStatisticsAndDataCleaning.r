#W2F5_Descriptive statistics and data cleaning
 

#http://www.hscic.gov.uk/catalogue/PUB13648

obesity <- read.csv("http://www.hscic.gov.uk/catalogue/PUB13648/Obes-phys-acti-diet-eng-2014-tab_CSV.csv", skip=4, nrows=12)

str(obesity)
obesity$Males <-  as.numeric(as.character(gsub(",","",obesity$Males)))
obesity$Females <- as.numeric(as.character(gsub(",","",obesity$Females)))
obesity<- obesity[-1,c(-2, -5:-12)]
obesity
#This is the So called wide format. 
#We would like to have the long format: one row - one observation

install.packages("reshape2")
library("reshape2")
obesitylong <- melt(obesity)
obesitylong  #long format

plot(obesitylong$value~obesitylong$variable)

#  install.packages("lubridate")

#  setting the argument colClasses=  in read.table() can reduce import time of large datasets


body <- read.table("http://www.amstat.org/publications/jse/datasets/body.dat.txt")
dim(body)
str(body)

BodyMeasurements <- c("Biacromial_diameter","Biiliac_diameter","Bitrochanteric_diameter","Chest_depth","Chest_diameter","Elbow_diameter","Wrist_diameter","Knee_diameter","Ankle_diameter","Shoulder_girth","Chest_girth","Waist_girth","Navel_girth","Hip_girth","Thigh_girth","Bicep_girth","Forearm_girth","Knee_girth","Calf_max_girth","Ankle_min_girth","Wrist_min_girth","Age","Weight","Height","Gender")    
names(body) <- BodyMeasurements 

summary(body)
boxplot(body)
?par()
keep.par <- par()
par(mar = c(10,4,4,2)+0.1)

#What does + 0.1 do? Test the code:
c(5, 4, 4, 2) + 0.1

boxplot(body, las=3)
# to restore parameters to defaul, use: par(keep.par)
#or close your plotting window


#A few examples of visualization: Postion, colour, size, plot character
#You can visualize many different variables in the same graph.

x<- 1:10
set.seed(23)
y <- x + rnorm(10)
plot(x,y) #position
plot(x,y, col=x) #colour
plot(x,y, col=x, cex=x) #size
plot(x,y, col=x, cex=x, pch=x) #plot chartacter

x <- rep(1:10, 10)
y <- rep(1:10, each=10)
z <- 1:100
plot(x,y,pch =z)

plot(body$Thigh_girth,body$Bicep_girth)
plot(body$Thigh_girth,body$Bicep_girth, pch=body$Gender)
plot(body$Thigh_girth,body$Bicep_girth, col=body$Gender+1)

#Summarize a variable by binning
breaks <- seq(min(body$Age),max(body$Age), 5)
Age_group <- cut(body$Age, breaks)
body$Age_group <- Age_group

plot(body$Thigh_girth,body$Bicep_girth, pch=body$Gender, col=body$Age_group)
plot(body$Thigh_girth,body$Bicep_girth, pch=body$Gender, col=body$Age_group, cex=(body$Weight/10))

#Do continue exploring the dataset body
