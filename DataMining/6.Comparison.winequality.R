###########################
# Comparison with wine data
###########################

#install.packages('adabag')
#install.packages('ROCR')

# Importing data
wine = read.csv("winequalityCLASS.csv", header=TRUE)

# Determining a cutoff
cutoff = 0.5 

# Partitioning data into train and test sets
library(caret)
set.seed(1234)
train.index = createDataPartition(wine$quality, p=0.7, list=FALSE)
wine.train = wine[ train.index,] #train data
wine.test  = wine[-train.index,] #test data


### Regression

fit.reg = glm(quality ~ ., family = binomial(link = "logit"), data = wine.train)
fit.step.reg = step(fit.reg, direction="both", trace=FALSE) #stepwise selection

p.test.reg = predict(fit.step.reg, newdata=wine.test, type="response") #probabilities
yhat.test.reg = ifelse(p.test.reg > cutoff, 1, 0) 

tab = table(wine.test$quality, yhat.test.reg, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity


### Decision Tree

library(rpart)

my.control = rpart.control(xval=10, cp=0, minsplit=5)
fit.tree = rpart(quality ~ ., data = wine.train, method="class", control=my.control)
tmp = printcp(fit.tree)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.prun.tree = prune(fit.tree, cp=cp.tmp)

p.test.tree = predict(fit.prun.tree, newdata=wine.test, type="prob")[,2] #probabilities
yhat.test.tree = ifelse(p.test.tree > cutoff, 1, 0)

tab = table(wine.test$quality, yhat.test.tree, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity


### Neural Network

library(neuralnet)

library(caret)
set.seed(1234)
train.index = createDataPartition(wine$quality, p=0.7, list=FALSE)
wine.train = wine[ train.index,] #train data
wine.test  = wine[-train.index,] #test data

max1 = apply(wine.train, 2, max) 
min1 = apply(wine.train, 2, min)

gdat.train = scale(wine.train, center = min1, scale = max1 - min1)
gdat.train = as.data.frame(gdat.train)

gdat.test = scale(wine.test, center = min1, scale = max1 - min1)
gdat.test = as.data.frame(gdat.test)

gn = names(gdat.train)
f = as.formula(paste("quality ~", paste(gn[!gn %in% "quality"], collapse = " + ")))
fit.nn = neuralnet(f, data = gdat.train, hidden=c(2,1), linear.output=F) 

p.test.nn = predict(fit.nn, gdat.test) 
yhat.test.nn = ifelse(p.test.nn > cutoff, 1, 0)
  
tab = table(gdat.test$quality, yhat.test.nn, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity



### Bagging

library(rpart)
library(adabag)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality) 
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

my.control = rpart.control(xval=0, cp=0, minsplit=5)
fit.bag = bagging(quality ~ ., data = wine.train, mfinal=100, control=my.control)

p.test.bag = predict.bagging(fit.bag, newdata=wine.test)$prob[,2] #probabilities
yhat.test.bag = ifelse(p.test.bag > cutoff, levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.bag, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity



### Boosting

library(rpart)
library(adabag)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality) 
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

my.control = rpart.control(xval=0, cp=0, maxdepth=4)
fit.boo = boosting(quality ~ ., data = wine.train, boos=T, mfinal=100, control=my.control)

p.test.boo = predict.boosting(fit.boo, newdata=wine.test)$prob[,2] #probabilities
yhat.test.boo = ifelse(p.test.boo > cutoff, levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.boo, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity



### Random Forest

library(randomForest)

if(!is.factor(wine.train$quality)) wine.train$quality = factor(wine.train$quality) 
if(!is.factor(wine.test$quality)) wine.test$quality = factor(wine.test$quality)

fit.rf = randomForest(quality ~ ., data = wine.train, ntree=100, mtry=5, importance=T, na.action=na.omit)

p.test.rf = predict(fit.rf, newdata=wine.test , type="prob")[,2] #probabilities
yhat.test.rf = ifelse(p.test.rf > cutoff, levels(wine.test$quality)[2], levels(wine.test$quality)[1])

tab = table(wine.test$quality, yhat.test.rf, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity



### ROC and AUC 

library(ROCR)

# Making predictions
pred.reg  = prediction(p.test.reg, wine.test$quality);  perf.reg = performance(pred.reg,"tpr","fpr")
pred.tree = prediction(p.test.tree, wine.test$quality); perf.tree = performance(pred.tree,"tpr","fpr")
pred.nn   = prediction(p.test.nn, wine.test$quality);   perf.nn = performance(pred.nn,"tpr","fpr")
pred.bag  = prediction(p.test.bag, wine.test$quality);  perf.bag = performance(pred.bag,"tpr","fpr")
pred.boo  = prediction(p.test.boo, wine.test$quality);  perf.boo = performance(pred.boo,"tpr","fpr")
pred.rf   = prediction(p.test.rf, wine.test$quality);   perf.rf = performance(pred.rf,"tpr","fpr")


# Drawing ROCs
plot(perf.reg,  lty=1, col=1, xlim=c(0,1), ylim=c(0,1),xlab="1-Specificity", ylab="Sensitivity", main="ROC Curve")
plot(perf.tree, lty=2, col=2, add=TRUE)
plot(perf.nn,   lty=3, col=3, add=TRUE)
plot(perf.bag,  lty=4, col=4, add=TRUE)
plot(perf.boo,  lty=5, col=5, add=TRUE)
plot(perf.rf,   lty=6, col=6, add=TRUE)

lines(x = c(0, 1), y = c(0, 1), col = "grey")
legend(0.6,0.3, c("Regression","Decision Tree","Neural Network","Bagging","Boosting","Random Forest"), lty=1:6, col=1:6)

# Computing AUCs
performance(pred.reg, "auc")@y.values #Regression
performance(pred.tree,"auc")@y.values #Decision Tree
performance(pred.nn,  "auc")@y.values #Neural Network
performance(pred.bag, "auc")@y.values #Bagging
performance(pred.boo, "auc")@y.values #Boosting
performance(pred.rf,  "auc")@y.values #Random Forest


### END


