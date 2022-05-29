
library(datarium)
marketing
plot(marketing$facebook, marketing$sales, main="202135-368864")

color<-c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF")
pie(rep(1,12), col=rainbow(12), labels = "")
par(new=T)
pie(rep(1,1),col="white", radius = 0.5, border="white", labels = "")

library(RColorBrewer)
display.brewer.all(type = "seq")

brewer.pal(9,"Reds")
windows(height = 2.5, width = 8)
barplot(rep(1,9), col = brewer.pal(9,"Reds"), axes = F, main = "Brewer Reds")

display.brewer.all(type = "div")
x11();display.brewer.all(type = "qual")

barplot(rep(1,50), col = cm.colors(50), axes = T, main = "Brewer Reds")


barplot(rep(1,15), col = brewer.pal(15,"BrBG"), axes = T, main = "Brewer Reds")

cumsum(Period)



pie(rep(1,12), col=rainbow(12), clockwise=T, labels = "", border = "white")
par(new=T)
pie(rep(1,1),col="white", radius = 0.5, border="white", labels = "")


plot(c(0,5), c(0,5), type="n", xlab = "", ylab = "", main = "")
for(i in 1:5)
{
  for (j in 1:5)
  {
    rect(i-1, j-1, i, j, col=rainbow(5*5)[(j-1)*5+i])
  }
}

library(ggplot2)
library(dplyr)
dat0 = read.csv("./data/owid-covid-data.csv", header = T)
dat1 = dat0 %>% filter(iso_code == "KOR" |
                         iso_code == "JPN" |
                         iso_code == "USA" |
                         iso_code == "FRA") %>%
  mutate(date = as.Date(date))

ggplot(dat=dat1, aes(x=date, y=new_cases/100, color=iso_code)) +
  labs(x="날짜", y="신규감염자(단위:1/100)") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=dat1, aes(x=date, y=new_cases_per_million, color=iso_code)) +
  labs(x="날짜", y="100만명당신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)


ggplot(dat=dat1, aes(x=date, y=new_deaths, color=iso_code)) +
  labs(x="날짜", y="신규사망자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=dat1, aes(x=date, y=new_cases_per_million, color=iso_code)) +
  labs(x="날짜", y="100만명당신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=dat1, aes(x=date, y=new_deaths_per_million, color=iso_code)) +
  labs(x="날짜", y="100만명당신규사망자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)



library(ggplot2)
library(dplyr)
dat0 = read.csv("./data/owid-covid-data.csv", header = T)
head(dat0)
kor = dat0 %>% filter(iso_code == "KOR") %>%  mutate(date = as.Date(date))
jpn = dat0 %>% filter(iso_code == "JPN") %>%  mutate(date = as.Date(date))
usa = dat0 %>% filter(iso_code == "USA") %>%  mutate(date = as.Date(date))
fra = dat0 %>% filter(iso_code == "FRA") %>%  mutate(date = as.Date(date))

ggplot(dat=kor, aes(x=date, y=new_cases, color=iso_code)) +
  labs(x="날짜", y="한국 신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=jpn, aes(x=date, y=new_cases, color=iso_code)) +
  labs(x="날짜", y="일본 신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=usa, aes(x=date, y=new_cases, color=iso_code)) +
  labs(x="날짜", y="미국 신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

ggplot(dat=fra, aes(x=date, y=new_cases, color=iso_code)) +
  labs(x="날짜", y="프랑스 신규감염자") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)

str(dat0)


# R 패키지 “vcd”에 내장된 “Arthritis” 데이터셋은 류마티스 관절염 환자를 대상으로 한 임상시험 결과 데이터이다. 각 행은 각 환자를 나타내며, 
# 변수 Treatment는 그룹 (Treated = 새로운 치료제를 투약한 그룹, Placebo = 위약을 받은 그룹)을 나타낸다. 
# 변수 Sex는 성별을, Improved는 치료 결과(None = 차도 없음, Some = 약간 좋아짐, Marked = 매우 좋아짐)를 나타낸다. 
# 새로운 치료제 투약 여부가 치료 결과와 연관이 있는지, 성별과 치료 결과 간에 연관이 있는지를 데이터 시각화를 통해서 탐구하시오. (18점)
library(vcd)
str(Arthritis)

data("Arthritis")
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
art

mosaic(art, gp = shading_Friendly)
mosaic(art, gp = shading_max)






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




https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html

require(graphics)
pairs(airquality, panel = panel.smooth, main = "airquality data")

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



?pairs
str(airquality)
