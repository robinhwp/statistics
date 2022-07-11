
(cat<-c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
(val<-c(5,8,10,12,18,14,10, 9,8,4,2))
(arr<-c(rep(cat[1],val[1])))

for(i in 2:length(cat))
{
  arr<-c(arr, rep(cat[i],val[i]))
}

arr
tb <- sort(names(table(arr)))

df <- data.frame(cat, val)
df

barplot(val, names.arg = df$cat, col=rainbow(length(df$cat)), xlab="결근횟수"
        , ylab="인원수", main="결근일 횟수 분포포")

hist(df$val, breaks = 1:10)


v <-rep(0,length(cat))
x = 0
for (i in 1:length(cat)) {
  x = x + val[i]
  v[i] = x
}
(df2=data.frame(cat, v))

barplot(v, names.arg = df$cat, col=rainbow(length(df$cat)), xlab="결근횟수"
        , ylab="인원수", main="결근일 횟수 누적분포포")




m=3.25
s=0.5
n=25
x=3
pnorm(x,m,s/sqrt(n))



(round(pbinom(315, 1200, 0.25),2) - round(pbinom(285, 1200, 0.25),2))




# 데이터 입력
score = c(88, 83, 83, 85, 94, 88, 91, 96, 89, 83, 81, 80, 84, 89, 83, 79)
(bar.x = mean(score)); (s=sd(score)); (n=length(score))

# 모평균의 95% 신뢰구간
qt(0.975,15) # , 15 : 자유도
qt(0.025, 15, lower.tail = FALSE) 3 






population = c(6, 2, 4, 8, 10)
(population.var = var(population) * (length(population)-1)/length(population))

(df<-data.frame())

for(x in population)
{
  for(y in population)
  {
      df <- rbind(df, c(x,y))
  }
}
(colnames(df)<-c("x1", "x2"))
df$mean = (df$x1 + df$x2)/2
df$var = (df$x1 - df$mean)^2 + (df$x2 - df$mean)^2 
sample.var.mean = mean(df$var)
(sample.table = table(df$var))
(sample.prop = prop.table(sample.table))
(sample.cumsum=cumsum(sample.prop))


dpois(0, 2.5)
dpois(1, 2.5)
dpois(20, 2.5)
dpois(30, 2.5)
dpois(40, 2.5)




n = 15;m=3;s=1;a=0.05



pbinom(7, 100, 0.5) - pbinom(3, 100, 0.5)

pnorm(7, 5, sqrt(4.75)) - pnorm(2, 5, sqrt(4.75))

pnorm(0.918) - pnorm(-0.918)

x = c(88, 83, 83, 85, 94, 88, 91, 96, 
      89, 83, 81, 80, 84, 89, 83, 79)
mean(x)
sd(x)
x.var = (x-mean(x))^2
v = sum(x.var)
v
/16
sqrt(v/16)
sqrt(v/15)

qt(0.05/2, 15, lower.tail = FALSE)
qt(0.05/2, 25, lower.tail = FALSE)

alpha = 0.05
x = c( 8, 1, 10, 15, 15, 10, 5, 19, 20, 9, 10)
(x.mean = mean(x))
(x.sd = sd(x))
(x.estimate = qt(alpha/2, length(x)-1, lower.tail = FALSE))
(x.low = x.mean - x.estimate * x.sd / sqrt(length(x)))
(x.high = x.mean + x.estimate * x.sd / sqrt(length(x)))





/* 5-2 데이터 */
DATA data5_2;
	INPUT 국가분류 $ 주가 @@;
DATALINES;
F 120 K 135 K 170 F 139 K 114 F 163
K 165 K 161 F 147 F 150 F 175 K 145
K 147 K 102 F 235 F 157 K 129 K 129
F 144 K 165 F 161 K 173 F 111 K 145
RUN;

/* 줄기-잎 그림을 그리기위해 그래픽 옵션을 끈다. 예전 방식으로 출력하기위해 */
ODS LISTING;
ODS GRAPHICS OFF;

PROC UNIVARIATE DATA=data5_2 PLOT;
	VAR 주가;
	CLASS 국가분류;
RUN;	






> library(vcd)
> # 기술통계 확인
> summary(Arthritis)
ID          Treatment      Sex          Age          Improved 
Min.   : 1.00   Placebo:43   Female:59   Min.   :23.00   None  :42  
1st Qu.:21.75   Treated:41   Male  :25   1st Qu.:46.00   Some  :14  
Median :42.50                            Median :57.00   Marked:28  
Mean   :42.50                            Mean   :53.36              
3rd Qu.:63.25                            3rd Qu.:63.00              
Max.   :84.00                            Max.   :74.00      

임상시험 참여자 정보는 위약을 치료 받은 그룹은 43명이고 새로운 치료제를 투약한 그룹은 41명이다. 
성별로 보면 여자는 59명이고 남자는 25명이다.
치료후 차도가 거의 없었던 참여자수는 42명이고 약간 좋아진 참여자수는 14명 매우 좋아진 참여자는 28명이다.


> # 분할표 확인 
> (xtabs(~ Treatment + Improved, data = Arthritis))
Improved
Treatment None Some Marked
Placebo   29    7      7
Treated   13    7     21

위약을 투약한 그룹에서는 차도가 거의 없었던 참여자는 29명이고 약간 좋아졌거나 매우 좋아진 참여자는 14명인 반면에 
새로운 치료제를 투약한 그룹에서는 차도가 거의 없었던 참여자는 13명 약간 좋아졌거나 매우 좋아진 참여자는 36명이나 됐다.

mosaicplot(~Treatment+Improved+Sex, data = Arthritis, color=3:5)



library(ggplot2)
# 치료에 따른 향상을 성별로 분할하여 막대그래프로 그린다.
ggplot(data=Arthritis) +
  geom_bar(mapping=aes(x = Treatment, fill = Improved))+
  facet_wrap(~ Sex) 
# 남성과 여성의 참여자 수가 다르기 때문에 그래프를 화면에 채워서서 표시하면
ggplot(data=Arthritis) +
  geom_bar(mapping=aes(x = Treatment, fill = Improved), position="fill")+
  facet_wrap(~ Sex) 




  
# 먼저 NA 값들을 빼준다.
air<-na.omit(airquality)
attach(air)
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~Ozone+Solar.R+Wind+Temp
      , data=air # NA를 제거한 자료를 사용한다.
      , panel = panel.smooth # 패널에 추세선을 구한다.
      , diag.panel = panel.hist # 대각선에 히스토그램을 그리는 옵션
      , lower.panel = panel.cor # 하단패널에 상관계수를 출력한다.
      , main = "공기질 산점도 행렬")


