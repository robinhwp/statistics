# 기존 변수들을 모두 지운다.
rm(list=ls())

# market-1.txt를 읽는다.
(market = read.table("./data/market-1.txt", header = T))

# 선형회귀모델 구하기
market.lm = lm(Y~X, data = market)
# 선형회귀모형값 확인
summary(market.lm)

# 산점도
plot(market$X, market$Y, main = "광고료와 판매액의 산점도", xlab="광고료", ylab="총판매액", pch=19)
# 회귀선
abline(market.lm)

# 분산분석표
anova(market.lm)
