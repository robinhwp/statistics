
# 3장.변수선택

hospital = read.table("./data/hospital.txt", header = T)
hospital.lm = lm(Y~X1+X2+X3+X4+X5, data = hospital)
summary(hospital.lm)
anova(hospital.lm)

# 분산팽창인자 진단이 요구된다.
# install.packages("fmsb")
library(fmsb)
VIF(lm(X1~X2+X3+X4+X5, data = hospital))
VIF(lm(X2~X1+X3+X4+X5, data = hospital))
VIF(lm(X3~X1+X2+X4+X5, data = hospital))
VIF(lm(X4~X1+X2+X3+X5, data = hospital))
VIF(lm(X5~X1+X2+X3+X4, data = hospital))

# 독립변수들의 상관계수
cor(hospital[,-6])

hospital.lm1 = lm(Y~X2+X3+X4+X5, data = hospital)
summary(hospital.lm)
summary(hospital.lm1)

VIF(lm(X2~X3+X4+X5, data = hospital))
VIF(lm(X3~X2+X4+X5, data = hospital))
VIF(lm(X4~X2+X3+X5, data = hospital))
VIF(lm(X5~X2+X3+X4, data = hospital))

# 모든 가능한 회귀
hald=read.table("./data/hald.txt", header = T)
# install.packages("leaps")
library(leaps)
all.lm=regsubsets(Y~., data = hald)
(rs=summary(all.lm))
names(rs)
rs$rsq
rs$adjr2
rs$cp

# forward selection
start.lm=lm(Y~1, data = hald)
full.lm=lm(Y~., data = hald)
step(start.lm, scope=list(lower=start.lm, upper=full.lm), direction = "forward")

# backward elimination
full.lm = lm(Y~ . , data=hald)
step(full.lm, data=hald, direction="backward")


# stepwise regression
start.lm = lm(Y~1, data=hald) 
full.lm = lm(Y~ . , data=hald)
step(start.lm, scope=list(upper=full.lm), data=hald, direction="both")




