mydata = read.csv("UsedCars.csv", header = TRUE)
head(mydata,10)
attach(mydata)

#1
lm.res =lm(Price~Age+KM+HP+Metallic+Automatic+CC+Doors+Gears+Weight, data=mydata)
lm.res
names(lm.res)
lm.res$coefficients

lm.sum = summary(lm.res)
lm.sum
names(lm.sum)
lm.sum$coefficients

#2
yhat = fitted(lm.res)
yhat
uhat = resid(lm.res)
uhat
cbind(Age, KM, HP, Metallic, Automatic, CC, Doors, Gears, Weight, yhat, uhat)[1:10,]

#3
bhat = lm.sum$coefficients[,1]
se = lm.sum$coefficients[,2]
tstat = bhat / se
cbind(tstat, lm.sum$coefficients[,3])

#4 & 5
df = lm.res$df.residual
alpha = 1-0.95
qt(1-alpha/2, df)

qnorm(1-alpha/2)

#6
pval = 2 * pt(-abs(tstat), df)
cbind(pval, lm.sum$coefficients[,4])

#7
lm.sum$r.squared
var(yhat) / var(Price)
1 - var(uhat) / var(Price)


#VIF
#8 
install.packages("car")
library(car)
vif(lm.res)

#9
lm.res2 = lm(Weight~Age+KM+HP+Metallic+Automatic+CC+Doors+Gears, data=mydata)
r2.tv = summary(lm.res2)$r.squared
1/(1-r2.tv)


#model Comparision
#10
lm.res3 = lm(Price~Age+KM+HP+Automatic+Gears+Weight, data=mydata)
summary(lm.res3)

#11
summary(lm.res3)$r.squared
summary(lm.res3)$adj.r.squared

lm.sum$r.squared
lm.sum$adj.r.squared

detach(mydata)



