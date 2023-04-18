# 제2장 실습

library(readxl); library(zoo); library(rlang)
industrial=read_excel("./data/전산업생산지수.xlsx",sheet = "data" )
date_q = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), "month")
industrial_zoo = zoo(industrial[,2:3], date_q)
plot(industrial_zoo, screens = 1, col=c("red", "steelblue"), xlab = "연도", ylab = "전산업생산지수")
legend("topleft", col = col=c("red", "steelblue"), lty=1, c("원계열", "계절조정계열(단위:2020=100)"), bty="n")



# 제3장 실습
library(readxl); 
industrial=read_excel("./data/전산업생산지수.xlsx",sheet = "data" )
industrial_ts = ts(industrial[,2:3], start = 2000, frequency = 12)
dlindustrial_1 = diff(log(industrial_ts[,1]))
dlindustrial_12 = diff(log(industrial_ts[,1]), 12)
dlindustrial = cbind(dlindustrial_1, dlindustrial_12)
spectrum(na.omit(dlindustrial), spans=c(3,3), col=c("red", "steelblue"), 
         main="", lty=c(20,1), lwd=1.5)
legend("topright", col=c("red", "steelblue"), lty=c(20,1), lwd=1.5, 
       c("1차 차분", "12차 차분"), bty = "n")

# 3장 연습 - 과제3번.
library(readxl); 
industrial=read_excel("./data/전산업생산지수.xlsx",sheet = "data" )
spectrum(na.omit(industrial[,2:3]), spans=c(3,3), col=c("red", "steelblue"), 
         main="", lty=c(20,1), lwd=1.5)
legend("topright", col=c("red", "steelblue"), lty=c(20,1), lwd=1.5, 
       c("원계열", "계절조정계열"), bty = "n")



# 과제4-(1), (2)
library(readxl); library(tseries); library(ggplot2); library(forecast)
industrial=read_excel("./data/전산업생산지수.xlsx",sheet = "data" )
industrial_ts = ts(industrial[,3], start = 2000, frequency = 12)
autoplot(cbind("log(산업지수 CPI)"=log(industrial_ts), 
               "diff(log(산업지수 CPI))"=diff(log(industrial_ts))), 
         facets = TRUE, xlab = "연도", ylab = "")
adf.test(log(industrial_ts))
adf.test(diff(log(industrial_ts), 12))
# 시계열도표와 상관도표 및 부분상관도표
ggtsdisplay(log(industrial_ts))
ggtsdisplay(diff(log(industrial_ts)))
