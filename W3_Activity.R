body <- read.table("http://www.amstat.org/publications/jse/datasets/body.dat.txt")
dim(body)  #Check that dimensions are 507  25
BodyMeasurements <- c("Biacromial_diameter","Biiliac_diameter", 
                      "Bitrochanteric_diameter", "Chest_depth","Chest_diameter", 
                      "Elbow_diameter","Wrist_diameter", "Knee_diameter","Ankle_diameter", 
                      "Shoulder_girth","Chest_girth", "Waist_girth","Navel_girth", 
                      "Hip_girth","Thigh_girth", "Bicep_girth","Forearm_girth", 
                      "Knee_girth","Calf_max_girth", "Ankle_min_girth","Wrist_min_girth", 
                      "Age","Weight","Height","Gender") 
names(body) <- BodyMeasurements
#The appropriate variable type of gender is factor
body$Gender <- as.factor(body$Gender)

table(body$Gender)
summary(body$Gender)
dim(body[body$Gender==0,])
dim(body[body$Gender==1,])

boxplot(body)
boxplot(body[, 10:15])

boxplot(body$Shoulder_girth~body$Gender)
hist(body$Shoulder_girth[body$Gender==1])

## A better visual check of normality than a boxplot or a histogram is the so called Q-Q plot 
## (quantile-quantile plot). A Q-Q plot is a scatterplot of quantiles of two distributions. 
## If the distributions are similar in shape, the plot will come out as a straight line x=y.
qqnorm(y=body$Shoulder_girth)
qqline(y=body$Shoulder_girth)
?qqnorm()

qqnorm(y=body$Shoulder_girth[body$Gender==1])
qqline(y=body$Shoulder_girth[body$Gender==1])

## t-test
t.test(body$Shoulder_girth ~ body$Gender, var.equal=T)
