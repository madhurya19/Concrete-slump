model<- read.csv(file.choose(),header=T)
attach(model)
plot(model) #import the concrete slump data from UCI repository
lm.model <- lm(Y~X1+X2+X3+X4+X5+X6+X7) # Fittilng the model to regression model
summary(lm.model) # determining the R-square score
lm.model <- lm(ynew ~ X1+X2+X3+X4+X5+X6+X7)
summary(lm.model)
FD= X6-X3
par(mfrow=c(2,2))
plot(lm.model)

qqPlot(lm.model, main="QQ Plot") # plot normal plot 
yhat<-lm.model$fitted.values # plot of predicted values
t<-rstudent(lm.model) # plot of student residuals
plot(yhat,t) # plot of predicted y versus residuals

qqnorm(lm.model$res) 
summary(lm(Y~log(1+X))) # transformations to y
ynew<-(log(Y)) # transformations to y
ynew<- 1/(Y) # transformations to y
ynew<-sqrt(Y) # transformations to y
FD <- X6-X3 
par(mfrow=c(2,2))
plot(lm.model)

print(vif(lm.model)) #to detect multicollinearity

influence.measures(lm.model)

print(vif(lm.model))

cook = cooks.distance(lm.model)
plot(cook,ylab="Cooks distances")
points(23,cook[23],col='red')
dat2 <- model[-59, -70]
attach(dat2)

boxcox(Y~ ., data=model, lambda = seq(-2, 2, 1/10), plotit = TRUE, eps = 1/50, xlab = expression(lambda),ylab = "log-Likelihood")
abline(lm.model,col="blue")
