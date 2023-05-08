
# importing data
prod =  read.csv("./data/productivityREG.csv")
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

# Fitting a linear regression model
fit.all = lm(productivity~., data=prod)
fit.step = step(fit.all, direction = "both")
fit.step$anova
summary(fit.step)
 
# Making predictions
pred.reg = predict(fit.step, newdata=prod, type="response")
print(pred.reg)

#Evaluation
mean((prod$productivity = pred.reg)^2) # MSE
mean(abs(prod$productivity - pred.reg)) # MAE


# importing data
wine=read.csv("./data/q")