
# 5장 자료의 진단
forbes = read.table("./data/forbes.txt", header = T)
head(forbes, 3)
plot(forbes$temp, forbes$press, pch=19)
forbes$press = 100*log10(forbes$press)
head(forbes, 3)
plot(forbes$temp, forbes$press, pch=19)
forbes.lm = lm(press~temp, data=forbes)
abline(forbes.lm)
muy=mean(abs(forbes$press - fitted(forbes.lm)))
(forbes$press - fitted(forbes.lm))[abs(forbes$press - fitted(forbes.lm)) >= abs(muy)*2]
summary(forbes.lm)
anova(forbes.lm)
length(forbes$press)

forbes.res = ls.diag(forbes.lm)
names(forbes.res)
resid.result = round(cbind(forbes.res$std.res, forbes.res$stud.res, forbes.res$hat), 3)
colnames(resid.result)=c("stardardized resid", "studentized resid", "Hat")
print(resid.result)

rstudent(forbes.lm) # 스튜던트화 잔차
#Bonferroni 유의수준 0.01에서 기각치
qt(0.01/(2*17), 14)
# Bonferroni p-value for obs.12
2*17*(1-pt(12.374, 14))

# 특이값 검정
library(car)
outlierTest(forbes.lm)
