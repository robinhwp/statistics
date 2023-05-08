####################################
# Comparison with productivity data
####################################

# Importing data
prod = read.csv("productivityREG.csv", header=TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

# Partitioning data into train and test sets
set.seed(1234)
train.index = sample(1:nrow(prod), size=0.7*nrow(prod))
prod.train = prod[ train.index,] #train data
prod.test  = prod[-train.index,] #test data


### Regression

fit.reg = lm(productivity ~ ., data = prod.train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #Stepwise variable selection

pred.reg = predict(fit.step.reg, newdata=prod.test, type="response")
mean((prod.test$productivity - pred.reg)^2)  # MSE
mean(abs(prod.test$productivity - pred.reg)) # MAE


### Decision Tree

library(rpart)

my.control = rpart.control(xval=10, cp=0, minsplit=5)
fit.tree = rpart(productivity ~ ., data = prod.train, method="anova", control=my.control)
tmp = printcp(fit.tree)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.prun.tree = prune(fit.tree, cp=cp.tmp)

pred.tree = predict(fit.prun.tree, newdata=prod.test, type="vector")
mean((prod.test$productivity - pred.tree)^2)  # MSE
mean(abs(prod.test$productivity - pred.tree)) # MAE


### Neural Network

library(neuralnet)
library(dummy)

dvar = c(1:4) #find nominal variables
prod2 = dummy(x=prod[,dvar])  # transform nominal variables into dummy variables
prod2 = prod2[,-c(5,7,13,25)] # delete redundant dummy variables
prod2 = cbind(prod[,-dvar], prod2) # combine them
for(i in 1: ncol(prod2)) if(!is.numeric(prod2[,i])) prod2[,i] = as.numeric(prod2[,i])

set.seed(1234)
train.index = sample(1:nrow(prod2), round(0.7*nrow(prod2)))
prod2.train = prod2[ train.index,] #train data                              
prod2.test  = prod2[-train.index,] #test data

max1 = apply(prod2.train, 2, max) 
min1 = apply(prod2.train, 2, min)

sdat.train = scale(prod2.train, center = min1, scale = max1 - min1)
sdat.train = as.data.frame(sdat.train)

sdat.test = scale(prod2.test, center = min1, scale = max1 - min1)
sdat.test = as.data.frame(sdat.test)

vname = names(sdat.train)
f = as.formula(paste("productivity ~", paste(vname[!vname %in% "productivity"], collapse = " + ")))
fit.nn = neuralnet(f, data=sdat.train, hidden=c(3,1), linear.output=T) 

pred.nn = predict(fit.nn, sdat.test) 
pred.nn = pred.nn*(max1[7]-min1[7])+min1[7]
 
mean((prod.test$productivity - pred.nn)^2)  # MSE 
mean(abs(prod.test$productivity - pred.nn)) # MAE 


### Random Forests

library(randomForest)

fit.rf = randomForest(productivity ~ ., data = prod.train, ntree=100, mtry=5, importance=T, na.action=na.omit)

pred.rf = predict(fit.rf, newdata=prod.test , type="response")
mean((prod.test$productivity - pred.rf)^2)  # MSE
mean(abs(prod.test$productivity - pred.rf)) # MAE


### Summary: MSEs

mean((prod.test$productivity - pred.reg)^2)  # Regression
mean((prod.test$productivity - pred.tree)^2) # Decision Tree
mean((prod.test$productivity - pred.nn)^2)   # Neural Network
mean((prod.test$productivity - pred.rf)^2)   # Random Forest


### Summary: MAEs

mean(abs(prod.test$productivity - pred.reg))  # Regression
mean(abs(prod.test$productivity - pred.tree)) # Decision Tree
mean(abs(prod.test$productivity - pred.nn))   # Neural Network
mean(abs(prod.test$productivity - pred.rf))   # Random Forest



##### Scatter Plots

par(mfrow=c(2,2), pty="s")

a = min(prod.test$productivity); b = max(prod.test$productivity)

plot(prod.test$productivity, pred.reg, xlim=c(a,b), ylim=c(a,b),
     xlab="Observed", ylab="Predicted", main="Regression")
abline(a=0, b=1, lty=2)

plot(prod.test$productivity, pred.tree, xlim=c(a,b), ylim=c(a,b),
     xlab="Observed", ylab="Predicted", main="Decision Tree")
abline(a=0, b=1, lty=2)

plot(prod.test$productivity, pred.nn, xlim=c(a,b), ylim=c(a,b),
     xlab="Observed", ylab="Predicted", main="Neural Network")
abline(a=0, b=1, lty=2)

plot(prod.test$productivity, pred.rf, xlim=c(a,b), ylim=c(a,b),
     xlab="Observed", ylab="Predicted", main="Random Forests")
abline(a=0, b=1, lty=2)



### END                      






