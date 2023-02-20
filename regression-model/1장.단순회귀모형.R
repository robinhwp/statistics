

# 데이터 로드 data/market-1.txt
market=read.table("./data/market-1.txt", header = TRUE)
#회귀직선을 그리려면 선형회귀모형을 구해야한다.
market.lm=lm(Y~X, data = market)
plot(market$X, market$Y, xlab = "광고료", ylab = "매출", main="광고료와 판매액의 산점도도", pch=10)
abline(market.lm)
market.lm.summary = summary(market.lm)
# summary에서 회귀식 추출출
b0=market.lm.summary$coefficients["(Intercept)","Estimate"]
b1=market.lm.summary$coefficients["X","Estimate"]
paste0("$\\hat{Y}=", round(b0,5), " + ", round(b1,5), "X$")
# summary를 이용하지 않고 b0과 b1을 구하면..
bar_x=mean(market$X)
bar_y=mean(market$Y)
b1 = sum((market$X - bar_x)*(market$Y-bar_y))/sum((market$X - bar_x)^2)
b0=bar_y-b1*bar_x
paste0("$\\hat{Y}=", round(b0,5), " + ", round(b1,5), "X$")


# 잔차들의 합은 0이다. → $\sum e_i = 0$ 
sum(market.lm$residuals)
# 잔차들의 $ X_i$에 의한 가중합은 0이다. → $\sum X_ie_i = 0$ 
sum(market$x * market.lm$residuals)

# 분산분석으로 검정
market.lm.anova=anova(market.lm)
# 분산분석에서 F0-값으로 검정
F0=market.lm.anova["X", "F value"]
Fvalue=qf(1-0.05, 1, market.lm.anova["Residuals", "Df"])
paste0("F0=", round(F0, 2), ">F(1,", market.lm.anova["Residuals", "Df"],",0.05)=",round(Fvalue,2))
#분산분석에서 P-값으로 검정 -  “p-값<유의확률 0.05”
paste0( "p-값(",market.lm.anova["X", "Pr(>F)"],")<", 0.05)

# 분산분석표 분석에서의 SSR, SSE, SST
SSR = market.lm.anova["X", "Sum Sq"]
SSE = market.lm.anova["Residuals", "Sum Sq"]
SST = SSR + SSE
paste("SSR =", SSR, ", SSE = ", SSE, ", SST = ", SST)

# b0, b1으로 hat_y를 구해서 SSR과 SSE를 구하는 방법
hat_y = b0 + b1*market$X
SSR = sum((hat_y-bar_y)^2)
SSE = sum((market$Y - hat_y)^2)
SST = SSR + SSE
paste("SSR =", SSR, ", SSE = ", SSE, ", SST = ", SST)

S_xx = sum( (market$X - bar_x)^2)
S_xy = sum((market$X - bar_x)*(market$Y-bar_y))
(SSR = S_xy^2 / S_xx)

# 결정계수(coefficient of determination)
n=length(market$Y)
(RR=SSR/SST)
(RR=market.lm.summary$r.squared)
(RSE=sqrt(market.lm.anova["Residuals", "Mean Sq"]))
(RSE=market.lm.summary$sigma)
(RSE=sqrt(SSE/(n-2)))
(RSE=sqrt(sum((market$Y - hat_y)^2)/(n-2)))

# 상관계수(coefficient fo correlation) r
S_yy=sum((market$Y-bar_y)^2)
(r=S_xy / ( sqrt(S_xx) * sqrt(S_yy)))
if( b1 > 0 ) {
  paste0("if b1 > 0 then R^2 =", sqrt(RR))
} else if( b1 < 0 ) {
  paste0("if b1 < 0 then R^2 =", -sqrt(RR))
}



alpha=0.05
qval=qt(1-alpha/2,market.lm.anova["Residuals", "Df"])

# b0의 100(1-alpha)% 신뢰구간
MSE = market.lm.anova["Residuals", "Mean Sq"]
(b0.lwr=b0 - qval * sqrt( MSE * (1/n + bar_x^2 / S_xx)))
(b0.upr=b0 + qval * sqrt( MSE * (1/n + bar_x^2 / S_xx)))
(b0.lwr1 = market.lm.summary$coefficients["(Intercept)", "Estimate"] - market.lm.summary$coefficients["(Intercept)", "Std. Error"] * qval)
(b0.upr1 = market.lm.summary$coefficients["(Intercept)", "Estimate"] + market.lm.summary$coefficients["(Intercept)", "Std. Error"] * qval)

# b1의 100(1-alpha)% 신뢰구간
(b1.lwr=b1 - qval * sqrt( MSE / S_xx))
(b1.upr=b1 + qval * sqrt( MSE / S_xx))
(b1.lwr1 = market.lm.summary$coefficients["X", "Estimate"] - market.lm.summary$coefficients["X", "Std. Error"] * qval)
(b1.upr1 = market.lm.summary$coefficients["X", "Estimate"] + market.lm.summary$coefficients["X", "Std. Error"] * qval)


# X의 범위를 data frame으로 설정한다.
X=seq(3.5,14.5,0.2)
pred.frame = data.frame(X=X)
# predict 함수를 이용하여 hat_Y의 신뢰구간을 구한다. 
pc = predict(market.lm, int="c", newdata = pred.frame)
head(pc)
# hat_Y_new의 신뢰구간을 구한다.
pp = predict(market.lm, int="p", newdata = pred.frame)
head(pp)

# 직접 hat_Y의 신뢰구간을 구해본다.
hat_y.fit=b0+b1*X 
hat_y.pc.lwr=hat_y.fit - qval * sqrt(MSE*(1/length(market$X)+(X-bar_x)^2/S_xx))
hat_y.pc.upr=hat_y.fit + qval * sqrt(MSE*(1/length(market$X)+(X-bar_x)^2/S_xx))
hat_y.pc=cbind(hat_y.fit, hat_y.pc.lwr, hat_y.pc.upr)
colnames(hat_y.pc)= c("fit", "lwr", "upr")
head(hat_y.pc)

# 직접 hat_Y_new의 신뢰구간을 구해본다.
hat_y.pp.lwr=hat_y.fit - qval * sqrt(MSE*(1+1/length(market$X)+(X-bar_x)^2/S_xx))
hat_y.pp.upr=hat_y.fit + qval * sqrt(MSE*(1+1/length(market$X)+(X-bar_x)^2/S_xx))
hat_y.pp=cbind(hat_y.fit, hat_y.pp.lwr, hat_y.pp.upr)
colnames(hat_y.pp)= c("fit", "lwr", "upr")
head(hat_y.pp)

# 신뢰대 그리기
plot(market$X, market$Y, ylim=range(market$Y, hat_y.pp))
matlines(X, hat_y.pc, lty = c(1,2,3), col="BLUE")
matlines(X, hat_y.pp, lty = c(1,3,3), col="RED")

# H0: b1 = 0
# H1: b1 != 0
# p-value 구하기
(pvalue = market.lm.anova$`Pr(>F)`[1])
(pvalue = market.lm.anova["X", "Pr(>F)"])
(pvalue = 1-pf(market.lm.anova["X", "F value"], 
               market.lm.anova["X", "Df"], 
               market.lm.anova["Residuals", "Df"]))
