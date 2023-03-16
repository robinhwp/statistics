# 1장 연습문제

# 1 
X = c(3,1,5,8,1,4,2,6,9,3,5,7,2,6)
Y = c(39,24,115,105,50,86,47,90,140,112,70,186,43,126)
# 1.1 이 데이터의 산점도를 그려라.
plot(X, Y, pch=19)

# 1.2 최소제곱법에 의한 회귀선을 적합시켜라.
model = lm(Y~X)
paste0("hat_Y = ", round(coef(model)["(Intercept)"],3), " + ",  
                    round(coef(model)["X"],3), "*X")

# 1.3 추정치의 표준오차를 구하라.
summary(model)$sigma

# 1.4 결정계수와 상관계수를 구하라.
paste( "결정계수:", summary(model)$r.squared) # 결정계수
paste("상관계수:", summary(model)$fstatistic["value"]) # 상관계수

