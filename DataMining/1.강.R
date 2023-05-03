# 1장 데이터 마이닝이란

#(1) 의류생산데이터 UCI(University of California, Irvine) 머신러닝 저장소
# (Machine Learning Repository)에 탑제되어 있는 데이터 중 하나이다.
# https://archive-beta.ics.uci.edu/ml/datasets#productivity

# 1-1 의류생산성 데이터의 기술통계량 출력

################ productivity pre-processing ##############

prod <- read.csv("./data/productivityORG.csv")
prod$date <-as.Date(prod$date,format='%m/%d/%Y')
prod$quarter <- factor(prod$quarter)
prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)
summary(prod)

# 1-2 의류생산성 데이터의 상자그림 출력
attach(prod)
par(mfrow=c(3,4))
boxplot(target, col="cyan3", xlab="Target Productivity")
boxplot(smv, col="cyan3", xlab="Standard Minute Value")
boxplot(wip, col="cyan3", xlab="Work in Progress")
boxplot(over_time, col="cyan3", xlab="Overtime")
boxplot(incentive, col="cyan3", xlab="Incentive")
boxplot(idle_time, col="cyan3", xlab="Idle TIme")
boxplot(idle_men, col="cyan3", xlab="Idle Men")
boxplot(numchange, col="cyan3", xlab="Number of Changes in Style")
boxplot(numworkers, col="cyan3", xlab="Number of Workers")
boxplot(productivity, col="cyan3", xlab="Productivity")


# 1-3 의류생산성 데이터의 이상치 제거
dout <- rep(0,15)
dout2 <- rep(0,15)
for (i in 6:15){
  t3 <- quantile(prod[,i], 0.75)
  t1 <- quantile(prod[,i], 0.25)
  tq <- IQR(prod[,i], 0.75)
  dout[i] <- t3 + 1.5*tq
  dout2[i] <- t1 - 1.5*tq
}
outindex <- matrix(0,1197, 15)
for (i in 1:1197)
  for (j in 6:15){
    if (prod[i,j] > dout[j] || prod[i,j] < dout2[j]) outindex[i,j] <- 1
  }
prod2 <- prod[apply(outindex,1,sum)==0,]
prodnew <- prod2[,-c(1,11:13)]
head(prodnew)

# 이상치가 제거된 데이터 저장
write.csv(prodnew,"./data/productivityREG.csv",quote=F,row.names=F)

detach(prod) # attach 했던 변수 detach


# (2) 와인품질 데이터
# 와인품질 데이터(Wine Quality Data) 역시 UCI 머신러닝 저장소에 탑재되어 있다.
# https://archive.ics.uci.edu/ml/datasets/wine+quality
# https://archive-beta.ics.uci.edu/ml/datasets#wine

# 1-4 와인품질 데이터의 상자그림 출력
wine <- read.csv("./data/winequalityORG.csv")
attach(wine)
par(mfrow=c(3,4))
boxplot(fixed, col="cyan3", xlab="Fixed Acidity")
boxplot(volatile, col="cyan3", xlab="Volatile Acidity")
boxplot(citric, col="cyan3", xlab="Citric Acid")
boxplot(residsugar, col="cyan3", xlab="Residual Sugar")
boxplot(chlorides, col="cyan3", xlab="Chlorides")
boxplot(freeSD, col="cyan3", xlab="Free Sulfur Dioxide")
boxplot(totalSD, col="cyan3", xlab="Total Sulfur Dioxide")
boxplot(density, col="cyan3", xlab="Density")
boxplot(pH, col="cyan3", xlab="pH")
boxplot(sulphates, col="cyan3", xlab="Sulphates")
boxplot(alcohol, col="cyan3", xlab="Alcohol")

# 1-5 와인품질 데이터의 이상치 제거
dout <- rep(0,12)
dout2 <- rep(0,12)
for (i in 1:11){
  t3 <- quantile(wine[,i], 0.75)
  t1 <- quantile(wine[,i], 0.25)
  tq <- IQR(wine[,i], 0.75)
  dout[i] <- t3 + 1.5*tq
  dout2[i] <- t1 - 1.5*tq
}
outindex <- matrix(0,1599, 12)
for (i in 1:1599)
  for (j in 1:11){
    if (wine[i,j] > dout[j] || wine[i,j] < dout2[j]) outindex[i,j] <- 1
  }
wine2 <- wine[apply(outindex,1,sum)==0,]
table(wine2$quality)
write.csv(wine2,"./data/winequalityCLASS.csv",quote=F,row.names=F)
detach(wine)


# (3) 가변수 생성
# 1-6 의류생성 데이터 중 일부 명목변수의 더미변수화 과정

#install.packages("dummy")

library(dummy)
prod =  read.csv("./data/productivityREG.csv")
summary(prod)
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)
summary(prod)

# Generate dummy variables
dvar = c(1:4)
prod2 = dummy(x=prod[,dvar])
prod2 = prod2[,-c(5, 7, 13, 25)]
prod = cbind(prod[,-dvar], prod2)
for(i in 1: ncol(prod)) if(!is.numeric(prod[,i])) prod[,i] = as.numeric(prod[,i])
print(prod)
