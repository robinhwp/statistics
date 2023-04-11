# 제2장 실습

library(readxl); library(zoo); library(rlang)
Industrial=read_excel("./data/전산업생산지수.xlsx",sheet = "data" )
date_q = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), "month")
Industrial_zoo = zoo(Industrial[,2:3], date_q)
plot(Industrial_zoo, screens = 1, col=c(2,1))
legend("topleft", col = c(2,1), lty=1, c("원계열", "계절조정계열"), bty="n")


