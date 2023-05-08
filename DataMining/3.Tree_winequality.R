####################################
# Decision Tree with wine data
####################################
# setwd('c:/data')
# Importing data
wine = read.csv("./data/winequalityCLASS.csv", header=TRUE)

# Factorize for classification
wine$quality = factor(wine$quality)

# Classification Tree
library(rpart)
set.seed(1234)
# 데이터를 10개로 쪼개서 9로 모형을 만들고 1개로 테스트를 cp값이 0이 될때까지 
# 최소 20개까지 쪼개질때까지
my.control = rpart.control(xval=10, cp=0, minsplit=20)
# quality를 반응변수로 하고 모든 설명변수를 쓰겠다.
tree.wine = rpart(quality~., data=wine, method="class", control=my.control)
print(tree.wine)
# Display tree ( 최대한 크기로 자란 나무 : fully grown)
library(rpart.plot)
prp(tree.wine, type=4, extra=1, digits=2, box.palette="Grays")
# 최대로 크게 자란 나무는 갖고 있는 데이터는 잘 설명하지만 
# 새로운 데이터를 적용하면 잘 분류할 수 없다. 

# Pruning with c-s.e.
cps = printcp(tree.wine)
k = which.min(cps[,"xerror"]) # k = 6 이 된다.
# xerror : cross validation 에러가 최소값을 가진 cp값을 사용.
# xstd : 표준오차
err = cps[k,"xerror"]; se = cps[k,"xstd"]
c = 1 # 1-s.e.
k1 = which(cps[,"xerror"] <= err+c*se)[1]
cp.chosen = cps[k1,"CP"]
tree.pruned.wine = prune(tree.wine, cp=cp.chosen)
print(tree.pruned.wine)
# Display tree
prp(tree.pruned.wine, type=4, extra=1, digits=2, box.palette="Grays")

# Making predictions - probability prediction
prob.tree.wine = predict(tree.pruned.wine, newdata=wine, type="prob")
head(prob.tree.wine, 5)
cutoff = 0.5 #cutoff
yhat.tree.wine = ifelse(prob.tree.wine[,2] > cutoff, 1, 0)

# Evaluation(예측성능)
tab = table(wine$quality, yhat.tree.wine, dnn=c("Observed","Predicted"))
print(tab)              # confusion matrix
sum(diag(tab))/sum(tab) # accuracy
tab[2,2]/sum(tab[2,])   # sensitivity
tab[1,1]/sum(tab[1,])   # specificity
