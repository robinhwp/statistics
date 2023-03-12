# 중회귀모형 (multi linear regression model)

# 파일내용(market-2.txt)
# ID   X1    X2    Y   
# 1  4.2   4.5    9.3
# 2  8.5  12.0   18.5
# 3  9.3  15.0   22.8
# 4  7.5   8.5   17.7
# 5  6.3   7.4   14.6
# 6 12.2  18.5   27.9
# 7  6.5   5.5   12.5
# 8 10.4  16.5   25.2
# 9  5.8   3.7   10.8
# 10  9.2  13.5   20.5
# 11  7.2   5.2   14.9
# 12  8.5  15.0   19.2
# 13 10.6  14.4   22.5
# 14 13.9  13.3   28.4
# 15 12.7  12.5   25.6

#파일로드
market2=read.table("./data/market-2.txt", header = T)
head(market2, 3)
X=market2[,c(2,3)]
Y=market2[,4]
X=cbind(1,X)
(X=as.matrix(X))
(Y=as.matrix(Y))
(XTX=t(X)%*%X)
solve(XTX)
(XTY=t(X)%*%Y)
(beta=round(solve(XTX)%*%XTY,5))

#적합된 회귀식은 hat_Y=hat_beta0 + hat_beta1*X1 + hat_beta2 * X2 가 된다.
paste0("적합된 회귀식은 hat_Y=", beta[1,1], "+", beta[2,1],"X1+",beta[3,1], "X2 가 된다.")
X1=10; X2=10 # 이라고 하면
paste0("적합된 회귀식은 hat_Y=", beta[1,1], " + ", beta[2,1],"*", X1, " + ",beta[3,1],"*", X2,"=", +
         beta[1,1]+beta[2,1]*X1+beta[3,1]*X2," 이 된다.")


# 중회귀모형 적합
###########################################
market2.lm = lm(Y~X1+X2, data = market2)
market2.lm2 = lm(Y~X1, data = market2)
market2.lm1 = lm(Y~X2+X1, data = market2)
market2.lm3 = lm(Y~X2, data = market2)
anova(market2.lm)
anova(market2.lm2)
anova(market2.lm1)
summary(market2.lm)
summary(market2.lm1)
summary(market2.lm3)


market2.summary = summary(market2.lm)
#적합된 회귀식은 
paste0("hat_Y=", 
       round(market2.summary$coefficients["(Intercept)","Estimate"],5), " + ", 
       round(market2.summary$coefficients["X1","Estimate"],5), "*X1 + ", 
       round(market2.summary$coefficients["X2","Estimate"],5), "*X2")

# 각종 주요 정보
paste0("Residual Standard error = ", round(market2.summary$sigma, 4))
paste0("Multiple R-squared = ", round(market2.summary$r.squared, 5))
paste0("F-statistic = ", round(market2.summary$fstatistic["value"], 1))
paste0("p-value = ", round(1-pf(market2.summary$fstatistic["value"], market2.summary$fstatistic["numdf"], market2.summary$fstatistic["dendf"]), 14))

# 분산분석표
market2.anova=anova(market2.lm)
paste("SS(X1) =", market2.anova["X1", "Sum Sq"])
paste("SS(X2|X1) =", market2.anova["X2", "Sum Sq"])
paste("SS(X1,X2) =", market2.anova["X1", "Sum Sq"]+market2.anova["X2", "Sum Sq"])

market2.anova["Residuals","Df"]
market2.anova["Residuals","Sum Sq"]
market2.anova["Residuals","Mean Sq"]
(S_y.x=sqrt(market2.anova["Residuals","Sum Sq"]/market2.anova["Residuals","Df"]))

# 멱등행렬(idempotent matrix) 또는 햇행렬(hat matrix) H 
H = X%*%solve(t(X)%*%X)%*%t(X)
HH = H%*%H
HT = t(H)

# 모두 같다.
H[c(3:2),]
HH[c(3:2),]
HT[c(3:2),]

e=Y-X%*%beta
sum(e) # 오차항의 합은 0에 가깝다.
t(X)%*%e # 0.01대(0.02를 안넘는)의 값을 0벡터라고 봐야하나.. 

hat_Y =  beta[1,1] + beta[2,1]*X[,"X1"] + beta[3,1]*X[,"X2"]
re = market2$Y - hat_y
var(re)
sum(hat_Y*e)

X%*%beta

# 단위행렬(Identity matrix) 만들기
diag(3)

# 1 행렬(matrix of 1) 만들기
matrix(1, 3, 3) # 3 x 3 행렬

# 회귀방정식의 신뢰성
#############################################
(n=length(Y))
(bar_Y = mean(Y))
(SST = sum ( (Y - bar_Y)^2 ))
(SST = t(Y)%*%Y - n*(bar_Y)^2)
I=diag(n)
J=matrix(1, n, n)
(SST=t(Y)%*%(I-J/n)%*%Y)

(SSE=t(e)%*%e)
(SSE=sum((Y-hat_Y)^2))
(SSE=t(Y)%*%(I-H)%*%Y)

(SSR = sum ( (hat_Y - bar_Y)^2 ))
(SSR = t(hat_Y)%*%hat_Y - n*(bar_Y)^2)
(SSR = t(Y)%*%(H-J/n)%*%Y)

(k=length(colnames(X))-1)
(MSR = SSR / k)
(MSE = SSE / (n - k - 1))
(F0 = MSR / MSE)

(MSR=(market2.anova["X1","Sum Sq"]+market2.anova["X2","Sum Sq"])/k)
(MSE=market2.anova["Residuals","Sum Sq"]/(n - k - 1))
(F0 = market2.anova["Residuals","Sum Sq"])
market2.summary
# 표준화된 중회귀분석
#############################################

# j = 1, 2일때 sum Z_ij 과 sum (Z_ij)^2 
bar_x1=mean(market2$X1)
s_j1=sum((market2$X1-bar_x1)^2)
z__1= (market2$X1 - bar_x1)/sqrt(s_j1)
sum(z__1) # 0 ( 0에 아주 가까운 숫자 )
sum(z__1^2) # 1

bar_x2=mean(market2$X2)
s_j2=sum((market2$X2-bar_x2)^2)
z__2= (market2$X2 - bar_x2)/sqrt(s_j2)
sum(z__2) # 0 ( 0에 아주 가까운 숫자 )
sum(z__2^2) # 1

bar_y=mean(market2$Y)
S_yy = sum((market2$Y - bar_y)^2)
zY=(market2$Y - bar_y)/sqrt(S_yy)

market2.Z=data.frame(Y=zY,Z1=z__1, Z2=z__2)
par(mfrow=c(1,1))
plot(market2.Z$Y, market2.Z$Z1)
market2.lm1=lm(Y~Z1, data = market2.Z)
abline(market2.lm1)
market2.lm2=lm(Y~Z2, data = market2.Z)
abline(market2.lm2)



# lm.beta 패키지를 이용하여 표준화회귀모형 적합
# install.packages("lm.beta")
library(lm.beta)
market2.lm=lm(Y~X1+X2,data=market2)
(market2.beta=lm.beta(market2.lm))


coef(market2.beta)
(market2.beta.summary=summary(market2.beta))
market2.beta.summary$coefficients["X1","Standardized"]

paste0("적합된 회귀식은 hat_Y*=", 
       round(market2.beta.summary$coefficients["X1","Standardized"],4), "Z1 + ",
       round(market2.beta.summary$coefficients["X2","Standardized"],4), "Z2 가 된다.")


# 구간 추정과 가설 검정
################################################
# hat_vector β는 vector β의 불편추정량





# 변수추가
#####################################################
health = read.table("./data/health.txt", header=T)
head(health,3)

h1.lm = lm(Y ~ X1, data=health)
h2.lm = lm(Y ~ X1+X4, data=health)
anova(h1.lm, h2.lm)
anova(h1.lm)
anova(h2.lm)
50795 - 24049

# matrix
mI = diag(length(market2$Y))
mX = as.matrix(cbind(1, market2$X1, market2$X2))
mY = as.matrix(market2$Y)                
mHatβ = solve(t(mX)%*%mX)%*%t(mX)%*%mY 
mHatY = X%*%mHatβ
mH = mX%*%solve(t(mX)%*%mX)%*%t(mX)
(mI-mH)%*%mY
mX = as.matrix(cbind( c(4,5,5), c(3,2,1)))
mI = diag(length(mX[,1]))
mH = mX%*%solve(t(mX)%*%mX)%*%t(mX)
(mI-mH)%*%mX
mY = as.matrix(cbind( c(1,2,3), c(3,4,2)))
(mI-mH)%*%mY



###################################################
# 분석사례
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

# install.packages("car")
library(car)
health = read.table("./data/health.txt", header=T)
head(health,3)
h4.lm = lm(Y ~ X1+X2+X3+X4, data=health)
avPlots(h4.lm)
plot(health$X1, health$Y)

typeof(health)
colnames(health)


# 테스트
X1=c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)
X2=c(2, 4, 6, 5, 8, 4, 6, 9, 7, 9, 7, 9, 10)
Y=c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

par(mfrow=c(1,2))
resid.YX1=resid(lm(Y~X1))    
resid.X2X1=resid(lm(X2~X1))    
plot(resid.X2X1, resid.YX1)
lm0 = lm(resid.X2X1~ resid.YX1)
abline(lm0)

resid.YX2=resid(lm(Y~X2))    
resid.X1X2=resid(lm(X1~X2))    
plot(resid.X1X2, resid.YX2)
lm1 = lm(resid.X1X2~ resid.YX2)
abline(lm1)

summary(lm0)
summary(lm1)


par(mfrow=c(1,2))
plot(X1,Y)
lm.YX1=lm(Y~X1)
summary(lm.YX1)
abline(lm.YX1)
# 추정된회귀식
paste0("hatY=", round(coef(lm.YX1)["(Intercept)"],4), " + ", 
                round(coef(lm.YX1)["X1"],4), "*",
       names(coef(lm.YX1))[2])

plot(X2,Y)
lm.YX2=lm(Y~X2)
summary(lm.YX2)
abline(lm.YX2)
# 추정된회귀식
paste0("hatY=", round(coef(lm.YX2)["(Intercept)"],4), " + ", 
       round(coef(lm.YX2)[2],4), "*",
       names(coef(lm.YX2))[2])

lm.YX1X2=lm(Y~X1+X2)
summary(lm.YX1X2)
paste0("hatY=", round(coef(lm.YX1X2)["(Intercept)"],4), " + ", 
       round(coef(lm.YX1X2)[2],4), "*",
       names(coef(lm.YX1X2))[2], " + ",
       round(coef(lm.YX1X2)[3],4), "*",
       names(coef(lm.YX1X2))[3])

avPlots(lm.YX1X2)

# 실습코드 : 잔차 e(X1|X2,X3,X4)와 잔차 e(Y|X2,X3,X4)의 산점도도
health = read.table("./data/health.txt", header=T)
# head(health,3)
h4.lm = lm(Y ~ X1+X2+X3+X4, data=health)
h4.lm.y1 = lm(Y ~ X2+X3+X4, data=health)
h4.lm.x1 = lm(X1 ~ X2+X3+X4, data=health)
h4.lm11=lm(resid(h4.lm.y1)~resid(h4.lm.x1))
plot(resid(h4.lm.x1),resid(h4.lm.y1), xlab="X1 | others", ylab = "Y | others")
abline(h4.lm11, col="red", lwd=2)

