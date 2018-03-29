library(MASS)

#we will split the data into training and test sets(70:30)
set.seed(2)  #to get the same set of random numbers
library(caTools) #split function is present in this package

split<-sample.split(BostonHousing$medv,SplitRatio = 0.7)
# we divided the data with the ratio 0.7

training_data <- subset(BostonHousing,split == TRUE)
testing_data<- subset(BostonHousing, split == FALSE)

#To find the correlation(how strongly related) between two variables the we use below
plot(BostonHousing$crim,BostonHousing$medv,cex = 0.5,xlab = "Crime rate", ylab = "Price for house")
cr<-cor(BostonHousing) #this will give us values

#Checking scatter plots of the boston housing data set
library(lattice)
splom(~BostonHousing[c(1:6,14)], groups = NULL, data = BostonHousing,axis.line.tck = 0,axis.text.alpha = 0)
splom(~BostonHousing[c(7:14)], groups = NULL, data = BostonHousing,axis.line.tck = 0,axis.text.alpha = 0)
# looking at the graph,we can see there is a positive linear trend between mdev and rm
#no relation between lstat and  mdev


#Studying rm and medv, regression of plot
plot(BostonHousing$rm,BostonHousing$medv)
abline(lm(BostonHousing$medv ~ BostonHousing$rm), col = "red") # regression fit line

#we can use corrplot to visualize the correlation

library(corrplot)
corrplot(cr,type = "lower")
corrplot(cr, method = "number")

#Multicolinearity when two or more variables are highly correlation among themselves
#we tend to get less information if we have highly multicolinear variables
#so we can remove one of the predictors to remove multicolinearity
# we can use VIF(variance inflation factor) to identify multicolinearity

library(caret)

#to exclude medv(output)

BostonHousing_a = subset(BostonHousing, select = -c(medv))
numericData <- BostonHousing_a[sapply(BostonHousing_a, is.numeric)]
descrCor<- cor(numericData)

#vif

library(car)
model<- lm(medv~., data = training_data)
vif(model)

#Now to create the model we will use all columns

model<- lm(medv~., data = training_data)
# or
model<- lm(medv~ crim +zn + indus + chas + nox + rm + age + dis + rad 
           + tax + ptratio + b + lstat,data = training_data)

#for description of the model
summary(model)

#removing tax, age and indus
model<- lm(medv~ crim +zn + chas + nox + rm + dis + rad 
           + ptratio + b + lstat,data = training_data)
#for description of the model
summary(model)

#Now we can use this model to predict the output of the test set
pred<- predict(model, testing_data)
pred

#To compare predicted values and actual values, we can use plots
plot(testing_data$medv, type ="l", lty = 1.8, col = "green")
lines(pred,type = "l", col = "blue")

#Now we can use this model to predict the outpit of any sample dataset
#pred<- predict(model,"datasetname")
#pred