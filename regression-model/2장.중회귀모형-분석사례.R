
###################################################
# 제2장 분석사례(5강강)
###################################################
# 1) 자료읽기
library(xlsx)
chemical=read.xlsx("./data/chemical.xlsx", 1)
head(chemical)

# 2) 기술통계량 및 상관계수 보기
summary(chemical[,-1])
cor(chemical[,-1])

# 3) 산점도 그리기
par(mfrow=c(1,2), pty="s")
plot(chemical$speed, chemical$loss, pch=19)
plot(chemical$temp, chemical$loss, pch=19)

# 4) 회귀모형 적합하기
chemical.lm = lm(loss ~ speed+temp, data=chemical)
summary(chemical.lm)

# 추정된 회귀방정식 1) (chemical.lm$model과 coef(chemical.lm) 함수를 이용)
str_l = paste0("hat_", names(chemical.lm$model)[1])
str_r = paste0( round(coef(chemical.lm)["(Intercept)"], 3))
for(i in 2:length(coef(chemical.lm))) 
{
  str_r = paste0( str_r, " + ", round(coef(chemical.lm)[names(chemical.lm$model)[i]], 3),
                  "*", names(chemical.lm$model)[i])
}
paste0("추정된 회귀방정식: ", str_l, " = ", str_r)

# 추정된 회귀방정식 2) 최소제곱법으로 β를 행렬방정식으로 계산
mX=as.matrix(cbind(1, chemical$speed, chemical$temp))
mY=as.matrix(chemical$loss)
mβ=solve(t(mX)%*%mX)%*%t(mX)%*%mY
paste0("hat_loss = ", round(mβ, 3)[1], " + ",
       round(mβ, 3)[2], "*speed + ",
       round(mβ, 3)[3], "*temp")

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

# 4) 분산분석표 구하기
chemical.lm.anova=anova(chemical.lm)
AVT = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow=3)
colnames(AVT)=colnames(chemical.lm.anova)
rownames(AVT)=c("Regression", "Residuals", "Total")

for(i in 1:(length(rownames(AVT))-1))
{
  AVT["Regression", "Df"]=AVT["Regression", "Df"]+as.double(chemical.lm.anova[i,"Df"])
  AVT["Regression", "Sum Sq"]=AVT["Regression", "Sum Sq"]+as.double(chemical.lm.anova[i,"Sum Sq"])
}
AVT["Residuals", "Df"]=as.double(chemical.lm.anova["Residuals","Df"])
AVT["Residuals", "Sum Sq"]=as.double(chemical.lm.anova["Residuals","Sum Sq"])
AVT["Total", "Df"]=AVT["Regression", "Df"]+AVT["Residuals", "Df"]
AVT["Total", "Sum Sq"]=AVT["Regression", "Sum Sq"]+AVT["Residuals", "Sum Sq"]
AVT["Regression", "Mean Sq"] = AVT["Regression", "Sum Sq"] / AVT["Regression", "Df"]
AVT["Residuals", "Mean Sq"] = AVT["Residuals", "Sum Sq"] / AVT["Residuals", "Df"]
AVT["Regression", "F value"] = round(AVT["Regression", "Mean Sq"] / AVT["Residuals", "Mean Sq"], 1)
AVT["Regression", "Pr(>F)"] = round(1-pf(AVT["Regression", "F value"], 
                                         AVT["Regression", "Df"], 
                                         AVT["Residuals", "Df"]), 6)
print(AVT)

# 6) 잔차 산점도 : (독립변수, 잔차)
par(mfrow=c(1,2), pty="s")
plot(chemical$speed, chemical.lm$resid, pch=19)
abline(h=0, lty=2)
for (i in 1:length(chemical$speed))
{
  if(abs(chemical.lm$resid[i]) > 3.7)
  {
    text(chemical$speed[i]+0.3, chemical.lm$resid[i]-0.4, as.character(i))
  }
}

plot(chemical$temp, chemical.lm$resid, pch=19)
abline(h=0, lty=2)
for (i in 1:length(chemical$temp))
{
  if(abs(chemical.lm$resid[i]) > 3.7)
  {
    text(chemical$temp[i]+0.3, chemical.lm$resid[i]-0.3, as.character(i)) 
  }
}

(pvalue = market.lm.anova$`Pr(>F)`[1])
(pvalue = market.lm.anova["X", "Pr(>F)"])
(pvalue = 1-pf(market.lm.anova["X", "F value"], 
               market.lm.anova["X", "Df"], 
               market.lm.anova["Residuals", "Df"]))