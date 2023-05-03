

##############################################
# 분석사례
##############################################

# 1) 자료만들기
##############################################

# 2) 자료를 읽어 산점도 그리기
##############################################
super = read.table("./data/supermarket.txt", header=T)
with(super, plot(price, time, pch=19))

# 3) 회귀모형 적합하기
##############################################
super.lm = lm(time ~ price, data=super)

# 단순회귀방정식
# ==============================================

# 단순회귀방정식 - chemical.lm$model과 coef(chemical.lm) 함수를 이용
paste0("hat_", names(super.lm$model)[1], " = ", 
       round(coef(super.lm)["(Intercept)"], 3), " + ",
       round(coef(super.lm)[names(super.lm$model)[2]], 3), "*",
       names(super.lm$model)[2])

# 단순회귀방정식 - 직접 b0과 b1을 구하면..
bar_price=mean(super$price)
bar_time=mean(super$time)
b1 = sum((super$price - bar_price)*(super$time-bar_time))/sum((super$price - bar_price)^2)
b0=bar_time-b1*bar_price
paste0("hat_time = ", round(b0,3), " + ", round(b1,3), "*price")

# t0 값
# ==============================================
# t0 : summary의 t value를 사용
t0 = summary(super.lm)$coefficient ["price","t value"]

# t0 : 직접구해서
n=length(super$price) 
hat_time = b0 + b1 * super$price 
SSE = sum((super$time - hat_time)^2) 
MSE = SSE/(n-2) 
S_xx = sum((super$price - bar_price)^2) 
t0 = (b1 - 0) /sqrt( MSE / S_xx)

# p-value
# ==============================================
# p-value : summary의 Pr(>|t|)를 사용
p.value = summary(super.lm)$coefficient["price","Pr(>|t|)"]

# p-value : 직접 구해서
S_xy = sum((super$price - bar_price)*(super$time-bar_time))
SSR = S_xy^2 / S_xx
MSR = SSR / 1
p.value = 1-pf(MSR/MSE, 1, n-2)


# 결정계수
# ============================================

# R.squared : summary
R.sq=summary(super.lm)$r.squared

# R.squared : 직접 구하기
SST = SSE + SSR
R.sq = SSR/SST


# F-값
# ============================================
# F-값 : summary
F0=summary(super.lm)$fstatistic["value"]

# F-값 : 직접 구하기
F0=MSR/MSE

# 4) 분산분석표 구하기
##############################################
anova(super.lm)

# 검정통계량
# ============================================
F0 = anova(super.lm)["price", "F value"] 

# 유의확률
# ============================================
p.value = anova(super.lm)["price", "Pr(>F)"]


#  잔차그림 그리기
plot(super$price, super.lm$residuals, pch=19)
abline(h=0, lty=2)
plot(super.lm$fitted, super.lm$resid, pch=19)
abline(h=0, lty=2)


#추정값의 신뢰대 그리기
# range(super$price) : 1.5 ~ 42.1 범위입니다.
p.x=data.frame(price=1:45) # 6 ~ 50까지로 x의 범위를 변경했습니다.
pc = predict(super.lm, int="c", newdata=p.x)
pp = predict(super.lm, int="p", newdata=p.x)
# 산점도를 그릴때 신뢰대의 위치를 확인하기 위해서 xlim과 ylim으로 범위를 키웠습니다.
plot(super$price, super$time, ylim=range(pp), xlim=c(min(super$price)-mean(super$price), max(super$price)+mean(super$price)))
matlines(p.x$price, pc, lty = c(1,2,2), col="blue")
matlines(p.x$price, pp, lty = c(1,2,2), col="red")

