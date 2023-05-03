
### 모형의 진단.

## 오차의 등분산
goose = read.table("./data/goose.txt", header = T)
head(goose, 3)
# 하나의 그래프에 산점도를 겹쳐서 그린다.
par(mfrow=c(1,1))
plot(goose$obsA, goose$photo, pch=19, col="red")
par(new=TRUE)
plot(goose$obsB, goose$photo, pch=20, col="blue")
goose.lm = lm(photo~obsA+obsB, data=goose)
summary(goose.lm)
plot(goose.lm$fitted, goose.lm$resid, pch=19)

goose.lm = lm(photo~obsA, data=goose)
summary(goose.lm)
plot(goose.lm$fitted, goose.lm$resid, pch=19)
# 잔차산점도는 가 증가함에 따라 잔차의 흩어짐이 많아짐 이분산성이 의심됨
library(car)
ncvTest(goose.lm)

# 스코어 검정의 χ^2=81.41 이고, 유의확률 p-값이 매우 작으므로 등분산 가정을 기각


## 선형성
tree = read.table("./data/tree.txt", header = T)
head(tree, 3)
tree.lm = lm(V~D+H, data=tree)
par(mfrow=c(1,2))
plot(tree$D, tree.lm$resid, pch=19)
plot(tree$H, tree.lm$resid, pch=19)
#변수D의 잔차산점도의 경우2차 함수형태의 비선형성이 나타남

par(mfrow=c(1,1))
## 오차의 정규성
goose.lm = lm(photo ~ obsA, data = goose)
qqPlot(goose.lm)
# 잔차가 직선의 형태를 벗어나 곡선의 형태로 
# 직선에서 벗어나고 있음을 보이므로 정규성 가정에 위배되는 것으로 판단.
goose.rstudent = rstudent(goose.lm)
shapiro.test(goose.rstudent)
# W = 0.7192, p-value = 5.971e-08
# W 통계량은 0.7192이고 유의확률 p-value가 매우 작으므규성 가정을 기각함로 정.

### 치료

## Box-Cox변환
energy = read.table("./data/energy.txt", header = T)
head(energy, 3)

energy.lm = lm(Y~X, data = energy)
plot(energy.lm$fitted, energy.lm$resid, pch=19)
# 잔차산점도는 가 증가함에 따라 잔차의흩어짐이 많아짐 이분산성이 의심됨

library(MASS)
boxcox(Y~X, data=energy, lambda = seq(-2, 2, 1/2), plotit = TRUE)

# Box-Cox 변환그림에서는 log-likelihood 값이 최대가 되는
# λ값을 찾으면 됨. 그림에서 λ는 0.5 가 됨. 
# 이는 sqrt(root) 변환에 해당. 


