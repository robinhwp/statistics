

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

# t_0 값
# ==============================================


# 단순회귀방정식 - 직접 b0과 b1을 구하면..
bar_price=mean(super$price)
bar_time=mean(super$time)
b1 = sum((super$price - bar_price)*(super$time-bar_time))/sum((super$price - bar_price)^2)
b0=bar_time-b1*bar_price
paste0("hat_time = ", round(b0,3), " + ", round(b1,3), "*price")

# 
##############################################
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
