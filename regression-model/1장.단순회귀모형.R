

# 데이터 로드 data/market-1.txt
market=read.table("./data/market-1.txt", header = TRUE)
#회귀직선을 그리려면 선형회귀모형을 구해야한다.
market.lm=lm(Y~X, data = market)
with(market, plot(X, Y, xlab = "광고료", ylab = "매출", main="광고료와 판매액의 산점도도", pch=10))
plot(market$X, market$Y, xlab = "광고료", ylab = "매출", main="광고료와 판매액의 산점도도", pch=10)
abline(market.lm)
market.lm.summary = summary(market.lm)
# summary에서 회귀식 추출출
(b0=market.lm.summary$coefficients["(Intercept)","Estimate"])
(b1=market.lm.summary$coefficients["X","Estimate"])
paste0("$\\hat{Y}=", round(b0,5), " + ", round(b1,5), "X$")
# summary를 이용하지 않고 b0과 b1을 구하면..
(bar_x=mean(market$X))
(bar_y=mean(market$Y))
(b1 = sum((market$X - bar_x)*(market$Y-bar_y))/sum((market$X - bar_x)^2))
(b0=bar_y-b1*bar_x)
paste("$", "hat{Y}=", round(b0,5), " + ", round(b1,5), "X$")




# 잔차들의 합은 0이다. → $\sum e_i = 0$ 
sum(market.lm$residuals)
# 잔차들의 $ X_i$에 의한 가중합은 0이다. → $\sum X_ie_i = 0$ 
sum(market$x * market.lm$residuals)

# 분산분석으로 검정
market.lm.anova=anova(market.lm)
# 분산분석에서 F0-값으로 검정
(F0=market.lm.anova["X", "F value"])
(Fvalue=qf(1-0.05, 1, market.lm.anova["Residuals", "Df"]))
paste0("F0=", round(F0, 2), ">F(1,", market.lm.anova["Residuals", "Df"],",0.05)=",round(Fvalue,2))
#분산분석에서 P-값으로 검정 -  “p-값<유의확률 0.05”
paste0( "p-값(",market.lm.anova["X", "Pr(>F)"],")<", 0.05)

# 분산분석표 분석에서의 SSR, SSE, SST
(SSR = market.lm.anova["X", "Sum Sq"])
(SSE = market.lm.anova["Residuals", "Sum Sq"])
(SST = SSR + SSE)
# paste("SSR =", SSR, ", SSE = ", SSE, ", SST = ", SST)

# b0, b1으로 hat_y를 구해서 SSR과 SSE를 구하는 방법
(hat_y = b0 + b1*market$X)
(SSR = sum((hat_y-bar_y)^2))
(SSE = sum((market$Y - hat_y)^2))
(SST = SSR + SSE)
(SST = sum((market$Y-bar_y)^2))
# paste("SSR =", SSR, ", SSE = ", SSE, ", SST = ", SST)

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


# 귀무가설 H0: β1 = β10
# 대립가설 H1: β1 != β10
# p-value 구하기
(pvalue = market.lm.anova$`Pr(>F)`[1])
(pvalue = market.lm.anova["X", "Pr(>F)"])
(pvalue = 1-pf(market.lm.anova["X", "F value"], 
               market.lm.anova["X", "Df"], 
               market.lm.anova["Residuals", "Df"]))

(n=length(market$Y))
# market.lm.anova["Residuals", "Df"]는 (n-2)와 같다.
(c(n -2, market.lm.anova["Residuals", "Df"] ))
(qval=qt(1-0.05/2,market.lm.anova["Residuals", "Df"]))
(β1 = market.lm.summary$coefficients["X","Estimate"])
β10 = β1 + 1 # 임의의값 지정
(t0 = (β1 - β10)/market.lm.summary$coefficients["X","Std. Error"])
if(abs(t0) > qval)
{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립되어 귀무가설을 기각한다.")
}else{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립하지 않으므로 귀무가설을 기각하지 못한다.")
}
# 만약 β10이 0인 경우 교재와 같이 t0 = coefficients["X","Estimate"]/coefficients["X","Std. Error"] 가된다. 
β10 = β1 # 임의의값 지정
(t0 = (β1 - β10)/market.lm.summary$coefficients["X","Std. Error"])
if(abs(t0) > qval)
{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립되어 귀무가설을 기각한다.")
}else{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립하지 않으므로 귀무가설을 기각하지 못한다.")
}
# 만약 β10이 0인 경우이므로 
β10 = 0 # 임의의값 지정
(t0 = (β1 - β10)/market.lm.summary$coefficients["X","Std. Error"])
if(abs(t0) > qval)
{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립되어 귀무가설을 기각한다.")
}else{
  paste0("|t0|=",  abs(t0), "이고, t(n-2;α/2)=", qval, 
         "이므로 |t0| > t(n-2;α/2)가 성립하지 않으므로 귀무가설을 기각하지 못한다.")
}


t0 = (β1 - β1)/market.lm.summary$coefficients["X","Std. Error"]
# 책에서는 β10 = 0
t0 = β1 / market.lm.summary$coefficients["X","Std. Error"]
# 귀무가설을 기각하게 된다.

  
# 귀무가설 H0: b1 = 0
# 대립가설 H1: b1 != 0
# p-value 구하기
(pvalue = market.lm.anova$`Pr(>F)`[1])
(pvalue = market.lm.anova["X", "Pr(>F)"])
(pvalue = 1-pf(market.lm.anova["X", "F value"], 
               market.lm.anova["X", "Df"], 
               market.lm.anova["Residuals", "Df"]))
(pvalue=2*(1-pt(market.lm.summary$coefficients["X", "t value"], 
                market.lm.summary$df[2])))
α=0.05
if(pvalue < α)
{
  paste0("p-value=",  pvalue, " 이므로", 
         "p-value < α=", α, "가 성립되어 귀무가설을 기각한다.")
}else{
  paste0("p-value=",  pvalue, " 이므로", 
         "p-value < α=", α, " 가 성립하지 않으므로 귀무가설을 기각하지 못한다.")
}



# 가중회귀 직선을 구하라
x = c(1,2,3,4,5)
y = c(2,3,5,8,7)
w = 1/x
w.lm = lm(y ~ x, weights=w)
w.lm.summary = summary(w.lm)
b0=w.lm.summary$coefficients["(Intercept)","Estimate"]
b1=w.lm.summary$coefficients["x","Estimate"]
paste0( "가중회귀직선 hat_Y = ", round(b0,4), " + ", round(b1,4) ,"X")




# 분석사례


# 1장 연습문제 3번
bar_x=15.0 # mean(X)
bar_y=13.0 # mean(Y)
S_xx = 160.0 # sum((X - bar_x)^2)
S_xy = 90.0 # sum((X - bar_x)*(Y-bar_y))
S_yy = 83.2 # sum((Y - bar_y)^2)

β1 = S_xy / S_xx
β0=bar_y-β1*bar_x

paste0("(1)회귀선적합 hat_Y =", round(β0,5), " + ", round(β1,5), "X")

# (2) 분산분석표에 나오는 항목 정리
n=20
(x.df=1)
(Residuals.df = n-1)
(SSR = S_xy^2 / S_xx)
(SST = S_yy)
(SSE = SST - SSE)
(MSR = SSR / x.df)
(MSE = SSE / Residuals.df)
(F0 = MSR / MSE)

# (3) 상관계수 r
(r = S_xy / (sqrt(S_xx)*sqrt(S_yy)))



##############################################
# 분석사례
##############################################
# 1) 자료만들기

# 2) 자료를 읽어 산점도 그리기
super = read.table("./data/supermarket.txt", header=T)
head(super)
with(super, plot(price, time, pch=19))

# 3) 회귀모형 적합하기
super.lm = lm(time ~ price, data=super)

# 단순회귀방정식 - 중회귀방정식에서도 사용가능. (chemical.lm$model과 coef(chemical.lm) 함수를 이용)
str_l = paste0("hat_", names(super.lm$model)[1])
str_r = paste0( round(coef(super.lm)["(Intercept)"], 3))
for(i in 2:length(coef(super.lm))) 
{
  str_r = paste0( str_r, " + ", round(coef(super.lm)[names(super.lm$model)[i]], 3),
                  "*", names(super.lm$model)[i])
}
paste0("추정된 회귀방정식: ", str_l, " = ", str_r)

# 기울기 검정

# 결정계수
# f-값

chemical.lm = lm(loss ~ speed+temp, data=chemical)
summary(chemical.lm)

# 추정된 회귀방정식 (chemical.lm$model과 coef(chemical.lm) 함수를 이용)
str_l = paste0("hat_", names(chemical.lm$model)[1])
str_r = paste0( round(coef(chemical.lm)["(Intercept)"], 3))
for(i in 2:length(coef(chemical.lm))) 
{
  str_r = paste0( str_r, " + ", round(coef(chemical.lm)[names(chemical.lm$model)[i]], 3),
                  "*", names(chemical.lm$model)[i])
}
paste0("추정된 회귀방정식: ", str_l, " = ", str_r)

# 결정계수
chemical.lm.summary=summary(chemical.lm)
paste0("결정계수 R.squared = ", round(chemical.lm.summary$r.squared, 3), "으로 ",
       round(chemical.lm.summary$r.squared*100,1), "% 설설명력이 있다.")

# p-value
α = 0.05
for(i in 2:length(coef(chemical.lm))) 
{
  p.value = round(chemical.lm.summary$coefficients[names(chemical.lm$model)[i], "Pr(>|t|)"], 5)
  if (p.value < α )
  {
    print(paste0( names(chemical.lm$model)[i], "의 p-value 가 ", p.value, "으로서 ", 
                  names(chemical.lm$model)[1], "를(을) 설명하는데 유의하다"))
  }
  else
  {
    print(paste0( names(chemical.lm$model)[i], "의 p-value 가 ", p.value, "으로서 ", 
                  names(chemical.lm$model)[1], "를(을) 설명하는데 그리 큰 영향을 준다고 할 수 없다."))
  }
}

#회귀모형적합 및 summary
# summary를 통해서 회귀직선을 적합한다.
b0=super.lm.summary$coefficients["(Intercept)","Estimate"]
b1=super.lm.summary$coefficients["price","Estimate"]
paste0("$hat_{", names(super.lm$model)[1],"}=", round(b0,5), " + ", round(b1,5), " \times ", names(super.lm$model)[2], "$")

paste0("$\/hat{time}=", round(b0,5), " + ", round(b1,5), " time$")
paste0("기울기 t-값=", round(super.lm.summary$coefficients["price","t value"],5))
paste0("p-값=", round(super.lm.summary$coefficients["price","Pr(>|t|)"],9))

# 분산분석표 구하기
(super.lm.anova=anova(super.lm))
(F0=super.lm.anova["price", "F value"])
(pvalue=super.lm.anova["price", "Pr(>F)"])
# 잔차및 추정값 보기
cbind(super, super.lm$residuals, super.lm$fitted.values)
cbind(super, residuals(super.lm), fitted(super.lm))
cbind(super, resid(super.lm), fitted(super.lm))

confint(super.lm, level = 0.9)
coef(super.lm)
formula(super.lm)
summary(super.lm)
anova(super.lm)
#  잔차그림 그리기
plot(super$price, super.lm$residuals, pch=19)
abline(h=0, lty=2)
plot(super.lm$fitted, super.lm$resid, pch=19)
abline(h=0, lty=2)

#추정값의 신뢰대 그리기
p.x=data.frame(price=c(1:45))
pc = predict(super.lm, int="c", newdata=p.x)
plot(super$price, super$time, ylim=range(super$time, pc))
matlines(p.x$price, pc, lty = c(1,2,2), col="blue")



pain=c(0,3,2,2,1)
fpain=factor(pain, levels = 0:3)
levels(fpain)=c("none", "mild", "medium", "severe")
as.numeric(fpain)
as.integer(fpain)



