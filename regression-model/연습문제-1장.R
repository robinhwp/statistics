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

# # 2023년 과제물 프로그램

# 교재의 데이터 입력 및 확인
X = c(3, 1, 5, 8, 1, 4, 2, 6, 9, 3, 5, 7, 2, 6)
Y = c(39, 24, 115, 105, 50, 86, 67, 90, 140, 112, 70, 186, 43, 126)
# X, Y 두 변수의 마지막 3개의 값을 확인
tail(cbind(X, Y), 3) 


# 산점도를 그린다.
plot(X, Y, pch=19, main="기계의 사용연도와 정비비용의 산점도", 
     xlab = "사용연도 (단위:년)", ylab = "정비비용 (단위:1000원)")


# 회귀모형 적합 및 요약 정보 확인
machine.lm = lm(Y~X)
summary(machine.lm)

anova(machine.lm)

# 잔차 및 추정값 보기
# 회귀모형 적합 결과(machine.lm)의 변수 확인
names(machine.lm)
# X와 Y와 잔차 및 추정값을 합쳐서 보기
cbind(X, Y, resid(machine.lm), fitted(machine.lm))


# 잔차를 독립변수 X에 대해 산점도를 그려본다.
plot(X, resid(machine.lm), pch=19, main="잔차와 X 산점도", xlab="X", ylab="잔차" )

# 잔차가 0인 라인 타입 2번 선을 그린다.
abline(h=0, lty=2)

# 추정값의 신뢰대 그리기
machine.frame = data.frame(X=seq(from = min(X), to=max(X),by=((max(X)-min(X))/(length(X)-1))))
pc = predict(machine.lm, int="c", newdata=machine.frame)
pp = predict(machine.lm, int="p", newdata=machine.frame)
plot(X, Y, ylim=range(Y, pp), main="추정값의 신뢰대", xlab="사용연도", ylab="정비비용")
matlines(machine.frame$X, pc, lty=c(1,2,2), col="BLUE")
matlines(machine.frame$X, pp, lty=c(1,3,3), col="RED")


