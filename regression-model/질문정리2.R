


x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(3.4, 5.1, 6.7, 9.5, 10.2, 12.6, 15.4, 16.8, 19.2, 21.1)
par(mfrow=c(1,1))
g.lm = lm(y~x)
plot(x, y, pch=19)
abline(g.lm, col="blue")

plot(x, g.lm$resid)
par(new=TRUE)
plot(x, rstudent(g.lm), pch=19, col="blue")

wghts = 1/g.lm$fitted.values^2
w.lm = lm(y~x, weights = wghts)

plot(x, w.lm$resid, pch=19, col="red")
par(new=TRUE)
plot(x, rstudent(w.lm), pch=19, col="red")


par(mfrow=c(1,1))
plot(x, y, pch=19)
abline(g.lm, col="blue")
abline(w.lm, col="red")


plot(x, rstudent(g.lm), pch=19, col="blue")
par(new=TRUE)
plot(x, rstudent(w.lm), pch=19, col="red")

