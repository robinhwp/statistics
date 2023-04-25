# 제 4장 모형개발

# 데이타를 읽고
tcrime = read.table("./data/tcrime.txt", header = T)
# 산점도를 그리고
with(tcrime, plot(motor, tcratio, pch=19))
# 독립변수가 1개인 2차다항회귀모형식을 구해본다.
tcrime.lm = lm(tcratio~motor+I(motor^2),data = tcrime )
summary(tcrime.lm)
# 결정계수(summary(tcrime.lm)$r.squared)는 0.976정도로 97.6%가 설명된다.
# 적합된 회귀식은은 
# h.tcratio = -75.45 + 0.454*motor - 0.00004149 * motor^2


# R활용예
# 데이터를 읽고
marathon=read.table("./data/marathon.txt", header = T)
# 산점도를 그려본다.
with(marathon, plot(sect, m1990, pch=19))
# 산점도 그림을 보면 위로볼록했다 아래로 볼록해지는 것처럼 보여 
# 3차다항회귀모형식을 적합해 본다.
marathon.lm = lm(m1990~sect+I(sect^2)+I(sect^3) , data=marathon)
summary(marathon.lm)
# 결정계수(summary(marathon.lm)$r.squared)는 0.998정도로 99.8%가 설명된다.
# 적합된 회귀식은은 
# h.m1990 = 917.59 + 13.785*sect - 0.683*sect^2 + 0.0122*sect^3


# 가변수 회귀모형 
########################################

# 데이터를 읽고
soup = read.table("./data/soup.txt", header = T)
head(soup,2); tail(soup,2)
# soup$D가 factor처럼 보이기 때문에 유형별 분포를 확인해 본다.
table(soup$D)
## 0이 12개이고 1이 15개이다 factor로 보인다.
# soup$D를 factor로 변환
soup$D = factor(soup$D, levels = c(0, 1), label=c("Line0", "Line1"))
# D가 1인 경우와 0인 경우를 다른 모양으로 산점도를 그려본다.
plot(soup$X, soup$Y, type="n") # 일단 아무것도 찍지않고 산점도 영역 확보
points(soup$X[soup$D=="Line1"], soup$Y[soup$D=="Line1"], pch=17, col="blue")
points(soup$X[soup$D=="Line0"], soup$Y[soup$D=="Line0"], pch=19, col="red")
legend("bottomright", legend=levels(soup$D), pch = c(19, 17), col=c("red", "blue") )

# 두 생산라인은 기울기 차이는 없는것 같아 보인다.
# 1) 교호작용이 없는 경우로 적합해 보고 회귀선을 구해본다.
soup.lm = lm(Y ~ X+D, data=soup)
summary(soup.lm)
# 회귀모형은 잔차와 기울기로 Lin0에 해당하는 회귀선을 구해본다.
abline(coef(soup.lm)["(Intercept)"], coef(soup.lm)["X"], lty=2, col="RED")
abline(sum(coef(soup.lm)[c("(Intercept)", "DLine1")]), coef(soup.lm)["X"], lty=2, col="blue")
round(coef(soup.lm), 3)

# 가변수를 이용한 회귀모형 : 교호작용을 고려한 경우
soup2.lm = lm(Y ~ X+D+X:D, data=soup)
summary(soup2.lm)


## 범주가 3개 이산인 경우 가변수를 갖고 있는 경우
library(faraway) # install.packages("faraway")
fruitfly[c(1,26,51,75,101), ]


prater = read.table("./data/prater.txt", header = T)
head(prater, 2)
prater.lm = lm(Y~X1+X2+X3+X4, data=prater)
summary(prater.lm)
start.lm = lm(Y~1, data=prater)
full.lm = prater.lm
step(start.lm, scope = list(upper=full.lm), data=prater, direction = "both")
