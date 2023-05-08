####################################
# Logistic Regression with wine data
####################################

# Importing data
wine = read.csv("./data/winequalityCLASS.csv", header=TRUE)

# Fitting a logistic regression model
fit.all = glm(quality ~ ., family = binomial, data = wine)
fit.step = step(fit.all, direction="both") # stepwise vaiable selection
fit.step$anova
summary(fit.step)

# Making predictions
p = predict(fit.step, newdata=wine, type="response") # prediction
cutoff = 0.5 #cutoff
yhat = ifelse(p > cutoff, 1, 0)

# Evaluation
tab = table(wine$quality, yhat, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy(정분류)
tab[2,2]/sum(tab[2,])   # sensitivity(민감도)
tab[1,1]/sum(tab[1,])   # specificity(특이도)


### END

