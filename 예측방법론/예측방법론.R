# 제2장 실습

# 프로그램 2-1 : GDP 시계열 읽기  
library(readxl); library(zoo)
gdp=read_excel("./data/데이터.xlsx", sheet="GDP")
data_q = seq(as.Date("1960-01-01"), as.Date("2022-04-01"), "quarter")
# data_q : "1960-01-01" "1960-04-01"  ... "2022-01-01" "2022-04-01"
# 쿼터가 시작되는 날짜를 순서대로 생성되었다.
gdp_zoo = zoo(gdp[,2:3], order.by = data_q )
# data_q에 들어있는 날짜와 매치해서 데이터 정리
head(gdp_zoo)


# 프로그램 2-2 : GDP 시계열도표 작성
plot(gdp_zoo/1000, screens = 1, col = c(2,1), ylab = "GDP(조 원)", xlab="")
legend("topleft", col=c(2,1), lty=1, c("계정조정계열", "원계열"), bty="n")
# lty = 1 레전드 라인 타입, bty="n" : 레전드 박스에 라인 없음

# 프로그램 2-3 : GDP의 차분과 이동평균
gdp = read_excel("./data/데이터.xlsx", sheet="GDP")
gdp_ts = ts(gdp[,2:3]/1000, start=1960, frequency = 4)
dlgdp_1 = diff(log(gdp_ts[,2]))
dlgdp_4 = diff(log(gdp_ts[,2]),4)
dlgdp=cbind(dlgdp_1, dlgdp_4)
spectrum(na.omit(dlgdp), spans=c(3,3), col=c("red", "steelblue"), main="", lty=c(20,1), lwd=1.5)
legend("topright", col=c("steelblue", "red"), lty=c(1,20), lwd=1.5, c("1차차분", "4차차분"), bty="n")
       
       
library(readxl) # 엑셀파일 읽는 함수
library(zoo)    # 시계열 함수
library(rlang)  # 
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
