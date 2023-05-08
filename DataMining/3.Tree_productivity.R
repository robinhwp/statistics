###########################################
# Decision Tree with productivity data
###########################################
# setwd('c:/data')
# install.packages("rpart.plot")
# Importing data
prod = read.csv("./data/productivityREG.csv", header=TRUE)

# Factorizing predictor variables
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)

### Regression Tree
library(rpart)
set.seed(1234)
my.control = rpart.control(xval=10, cp=0.01, minsplit=30)
tree.prod = rpart(productivity~., data=prod, method="anova", control=my.control)
print(tree.prod)
# Display tree
library(rpart.plot)
prp(tree.prod, type=4, extra=1, digits=2, box.palette="Grays")

# Pruning with c-s.e.
cps = printcp(tree.prod)
k = which.min(cps[,"xerror"])
err = cps[k,"xerror"]; se = cps[k,"xstd"]
c = 1 # 1-s.e.
k1 = which(cps[,"xerror"] <= err+c*se)[1]
cp.chosen = cps[k1,"CP"]
tree.pruned.prod = prune(tree.prod, cp=cp.chosen)
print(tree.pruned.prod)
# Display tree
prp(tree.pruned.prod, type=4, extra=1, digits=2, box.palette="Grays")

# Making predictions
pred.tree.prod = predict(tree.pruned.prod, newdata=prod, type='vector')
head(pred.tree.prod, 5)
# 확률이 아닌 생산성 지표 출력

# Evaluation(서로 다른 모형간의 비교할때 사용)
mean((prod$productivity - pred.tree.prod)^2)  # MSE
mean(abs(prod$productivity - pred.tree.prod)) # MAE

# Observed vs. Predicted
plot(prod$productivity, pred.tree.prod, xlab="Observed Values", ylab="Fitted Values")
abline(0,1)

# 관측치의 평균을 가지고 예측치를 설정한다.