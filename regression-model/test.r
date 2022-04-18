
# 3강 선형회귀분석(2)
# 회귀직선을 구하고, 산점도위에 회귀직선을 그려보아라.
market = read.table("./data/market-1.txt", header = T)
head(market)
# 산점도를 그린다.
plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19, main="광고료와 판매액의 산점도")
# 회귀직선을 그린다.
abline(market.lm)
# 아우스 클릭 후 finish를 눌러서 종료
identify(market$X, market$Y)

# 회귀식을 구한다. ( lm : linear model)
market.lm = lm(Y~X, data=market)
summary(market.lm)
# 자유도가 13이고 신뢰구간 95%인 t분포값
(q.val = qt(1-(1-0.95)/2, 13))
(q.val = qt(0.975, 13))

xbar = mean(market$X)
ybar = mean(market$Y)
# 포인트를 찍는데 문자는 17이고 크기는 2.0이고 빨간색으로 찍는다.
points(xbar, ybar, pch=17, cex=2.0, col="red")
text(xbar, ybar, "(8.85, 19.36)")
fx="Y-hat=0.328+2.14*X"
text(locator(1), fx)

#분산분석표 anova
anova(market.lm)

# X의 주어진 값에서 신뢰대 그리기
pred.frame = data.frame(X = seq(3.5, 14.5, 0.2)); pred.frame  
pc = predict(market.lm, int="c", newdata = pred.frame); head(pc)
pp = predict(market.lm, int="p", newdata = pred.frame); head(pp)
pred.X = pred.frame$X; pred.X
plot(market$X, market$Y, ylim=range(market$Y, pp))
matlines(pred.X, pc, lty=c(1,2,2), col="BLUE")
matlines(pred.X, pp, lty = c(1,3,3), col="RED")

# lm : fit linear models : 선형모델적합 함수
# 가중회귀직선()
x = c(1,2,3,4,5)
y = c(2,3,5,8,7)
(w = 1/x)
w.lm = lm(y~x, weights = w)
summary(w.lm)


# 분석사례

# 데이터 만들어 파일을 만든다.(이건 이미 되어 있기때문에 알아서 만든다.)
# 파일위치는 ./data/supermarket.txt
super= read.table("./data/supermarket.txt", header = T); head(super, 3)
attach(super)
plot(price, time, pch=19)
super.lm = lm(time~price, data=super)
summary(super.lm)
# 분산분석표
anova(super.lm)
# suepr.lm의 변수를 확인
names(super.lm)
# 원하는 변수들을 출력
cbind(super, super.lm$residuals, super.lm$fitted.values)
# 잔차 그림 그리기
plot(super$price, super.lm$residuals, pch=19)
abline(h=0, lty=2)

#신뢰대 그리기
p.x=data.frame(price=c(1,145))
pc = predict(super.lm, int="c", newdata=p.x)
pred.x = p.x$price
plot(super$price, super$time, ylim = range(super$time, pc), pch=19)
matlines(pred.x, pc, lty=c(1,2,2), col="BLUE")



# 4장 중회귀모형(1) - multiple linear regression model

# 파일에서 데이터 읽어오기
market2 = read.table("./data/market-2.txt", header = T); head(market2, 3)
# 2번째와 3번째열을 X로 잡아준다.
X = market2[,c(2:3)]
# 첫번째 행을 1번째로 한다.
X = cbind(1, X)
# Y는 파일에서 4번째 열을 가져온다.
Y = market2[,4]
# X와 Y를 행렬로 만들어준다.
X = as.matrix(X)
Y = as.matrix(Y)
# X' = t(X)  동일
(XTX = t(X) %*% X)
# 인버스를 구하는 함수는 solve
XTXI = solve(XTX)
(XTY = t(X) %*%Y)
# XTXI와 XTY를 행렬곱하고, 소숫점 3자리까지 반올림해서 나타냄
(beta = round(XTXI %*% XTY, 3))
