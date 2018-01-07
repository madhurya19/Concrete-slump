model<- read.csv(file.choose(),header=T)
attach(model)
plot(model)
lm.model <- lm(Y~X1+X2+X3+X4+X5+X6+X7)
summary(lm.model)
lm.model <- lm(ynew ~ X1+X2+X3+X4+X5+X6+X7)
summary(lm.model)
FD= X6-X3
par(mfrow=c(2,2))
plot(lm.model)

qqPlot(lm.model, main="QQ Plot")
yhat<-lm.model$fitted.values
t<-rstudent(lm.model)
plot(yhat,t)
qqnorm(lm.model$res)
summary(lm(Y~log(1+X)))
ynew<-(log(Y))
ynew<- 1/(Y)
ynew<-sqrt(Y)
FD <- X6-X3
par(mfrow=c(2,2))
plot(lm.model)

print(vif(lm.model))

influence.measures(lm.model)

print(vif(lm.model))

cook = cooks.distance(lm.model)
plot(cook,ylab="Cooks distances")
points(23,cook[23],col='red')
dat2 <- model[-59, -70]
attach(dat2)

boxcox(Y~ ., data=model, lambda = seq(-2, 2, 1/10), plotit = TRUE, eps = 1/50, xlab = expression(lambda),ylab = "log-Likelihood")
abline(lm.model,col="blue")
