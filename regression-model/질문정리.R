
# 가중회귀모형 

# 데이터 x : 레슨을 완료하는 동안의 총 응답 수
x=c(16,14,22,10,14,17,10,13,19,12,18,11) 
# 데이터 y : 수업 중 컴퓨터 사용 시간 비용
y=c(77,70,85,50,62,70,55,63,88,57,81,51)


x.c = x-mean(x)
olsfit.c=lm(y~x.c)
summary(olsfit.c)

plot(x.c, y)
abline(olsfit.c)

plot(x.c, olsfit.c$residuals, xlab="respose", ylab="OLS fit resisuals")
abline(h=0,lty=2)

plot(x.c, rstudent(olsfit.c), xlab="respose", ylab="OLS fit resisuals")
abline(h=0,lty=2)

plot(x.c, abs(olsfit.c$residuals), xlab="Response", ylab="OLS fit absolute value of residuals")

sd.lm = lm(abs(olsfit.c$residuals)~x.c)
summary(sd.lm)

wghts = 1/((sd.lm$fitted.values)^2)

wlsfit = lm(y~x.c, weights = wghts)
summary(wlsfit)
summary(olsfit.c)

plot(x.c, y, xlab="total # of Response in completing a lession", ylab="Cost of computer time", main="Ordinary vs. Weighted Least Squares")
abline(olsfit.c, col="red")
abline(wlsfit, col="blue")
legend("topleft", c("Weighted", "Unweighted"), lty=c(1,1), lwd=c(2,2), col=c("blue", "red"), bty="n")
       
par(mfrow=c(1,2))
plot(x.c, studres(olsfit.c), xlab="Total # of response in completing a lession", ylab="OLS studentized Deleted Resisuals")
plot(x.c, studres(wlsfit), xlab="Total # of response in completing a lession", ylab="WLS studentized Deleted Resisuals")

plot(x.c, rstudent(olsfit.c), xlab="Total # of response in completing a lession", ylab="OLS studentized Deleted Resisuals")
plot(x.c, rstudent(wlsfit), xlab="Total # of response in completing a lession", ylab="WLS studentized Deleted Resisuals")


https://www.youtube.com/watch?v=DTt0hLyRaTc&list=PLKmcZujz-ZJlcy1F9CUs9kCkP-DVCaQ-T&index=3

# 데이터 x : 레슨을 완료하는 동안의 총 응답 수
x=c(16,14,22,10,14,17,10,13,19,12,18,11) 
# 데이터 y : 수업 중 컴퓨터 사용 시간 비용
y=c(77,70,85,50,62,70,55,63,88,57,81,51)
olsfit=lm(y~x)
summary(olsfit)

plot(x, y)
abline(olsfit)

plot(x, olsfit$residuals, xlab="respose", ylab="OLS fit resisuals")
abline(h=0,lty=2)

plot(x, rstudent(olsfit), xlab="respose", ylab="OLS fit resisuals")
abline(h=0,lty=2)

plot(x.c, abs(olsfit$residuals), xlab="Response", ylab="OLS fit absolute value of residuals")

sd.lm = lm(abs(olsfit$residuals)~x.c)
summary(sd.lm)

wghts = 1/((sd.lm$fitted.values)^2)

wlsfit = lm(y~x, weights = wghts)
summary(wlsfit)
summary(olsfit)

plot(x, y, xlab="total # of Response in completing a lession", ylab="Cost of computer time", main="Ordinary vs. Weighted Least Squares")
abline(olsfit, col="red")
abline(wlsfit, col="blue")
legend("topleft", c("Weighted", "Unweighted"), lty=c(1,1), lwd=c(2,2), col=c("blue", "red"), bty="n")

par(mfrow=c(1,2))
plot(x, olsfit$resid, xlab="Total # of response in completing a lession", ylab="OLS studentized Deleted Resisuals")
plot(x, wlsfit$resid, xlab="Total # of response in completing a lession", ylab="WLS studentized Deleted Resisuals")

plot(x, rstudent(olsfit), xlab="Total # of response in completing a lession", ylab="OLS studentized Deleted Resisuals")
plot(x, rstudent(wlsfit), xlab="Total # of response in completing a lession", ylab="WLS studentized Deleted Resisuals")




