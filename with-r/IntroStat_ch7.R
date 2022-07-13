# 예제 7-3

# 예제 7-5
(mat = matrix(c(84, 83, 82,85,89,86,93,94,96,89,89,87), nrow=3,))
(tm = matrix(rep(0,8), ncol=4))
(mat2 = rbind(mat, tm))

dimnames(mat2)=list(c("1", "2", "3","합계", "평균"),c("A1","A2","A3","A4"))
for (i in 1:4)
{
  mat2[4,i]=round(sum(mat2[1:3,i]), 2)
  mat2[5,i]=round(mean(mat2[1:3,i]), 2)
}

(T = mat2[4, 1:4])
(xi_bar = round(mat2[5,1:4],2))
(xi_barbar=round(mean(mat2[5,1:4]),2))
(S_T=round(sum((mat-xi_barbar)^2),2))
(S_A=round(sum((xi_bar-xi_barbar)^2)*3,2))
(S_E=S_T-S_A)


xi_bar
(d = (xi_bar - xi_barbar)^2)
(dsum=sum(d))




# 데이터 입력
pre <- c(72,80,83,63,66,76,82)
post <- c(78,82,82,68,70,75,88)
(D_bar = mean(pre - post))
(S_d=sd(pre-post))
(obs = D_bar/(S_d/sqrt(7)))

(exam1 <-data.frame(pre, post))
# 두 모평균의 비교
t.test(exam1$pre, exam1$post, mu=0, alternative="less", paired=T)
t.test(pre, post, mu=0, alternative = "less",paired=T)

x <- c(84,83,82,85,89,86,93,94,96,89,89,87)
A <- c(rep(1,3), rep(2,3),rep(3,3),rep(4,3))
A <- factor(A)
aovdat1 <- data.frame(x, A)
aovmodel1 <- aov(x ~ A, data=aovdat1)
summary(aovmodel1)

y <- c(97.8, 97.5, 96.9, 98.5, 98.8, 97.1, 99.2, 98.4, 98.1, 98.2, 97.5, 96.8)
surface <- c(rep(1,3), rep(2,3),rep(3,3),rep(4,3))
manu <- rep(c(1,2,3),4)
surface <- factor(surface)
manu <- factor(manu)
aovdat2 <- data.frame(surface, manu)
aovmodel2 <- aov(y ~ surface + manu, data=aovdat2)
summary(aovmodel2)

dept <- c(rep("Stat",50),rep("DS",25))
regi <- c(rep("Y",20),rep("N",30),rep("Y",13),rep("N",12))
deptregi <- data.frame(dept,regi)
rtable <-xtabs(~dept+regi, data=deptregi)
rtable
ctest <-chisq.test(rtable, correct=F)
ctest

catnum <- c(0:3)
obs <-c(33, 15, 9, 3)
m <- sum(catnum*obs)/sum(obs)
pprob <- round(dpois(catnum, m),3)
pprob
pprob[4] <- 1-sum(pprob[1:3])
pprob
pprob*60
# 기대도수 5 미만 범주를 병합하여 재 분석
obs1 <- c(33, 15, 12)
pprob1 <- pprob[1:3]
pprob1[3] <- 1-sum(pprob[1:2])
pprob1
ctest1 <- chisq.test(obs1, p=pprob1)
ctest1$statistic > qchisq(0.95,1)
