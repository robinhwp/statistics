###########################################
# Ensemble with productivity data
###########################################

# Importing data
prod = read.csv("./data/productivityREG.csv", header=TRUE)

# Make categorical variable
prod$quarter <- factor(prod$quarter)
prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)

### Random Forest
library(randomForest)
set.seed(1234)
rf.prod <- randomForest(productivity~., data=prod, ntree=100, mtry=5, 
                           importance=T, na.action=na.omit)
# Variable importance
importance(rf.prod, type=1)
varImpPlot(rf.prod, type=1)
# Plot error rates
plot(rf.prod , type="l")
# Partial dependence plot
partialPlot(rf.prod, pred.data=prod, x.var='incentive')

# Making predictions
pred.rf.prod = predict(rf.prod, newdata=prod, type="response")
head(pred.rf.prod, 5)

# Evaluation
mean((prod$productivity - pred.rf.prod)^2)  # MSE
mean(abs(prod$productivity - pred.rf.prod)) # MAE

# Observed vs. Predited
plot(prod$productivity, pred.rf.prod, xlab="Observed Values", ylab="Fitted Values")
abline(0,1)


