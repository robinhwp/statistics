
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
head(dat0)
dat1 = dat0 %>% filter(iso_code == "KOR" |
                         iso_code == "JPN" |
                         iso_code == "USA" |
                         iso_code == "FRA") %>%
  mutate(date = as.Date(date))

ggplot(dat=dat1, aes(x=date, y=new_cases/100, color=iso_code)) +
  labs(x="날짜", y="신규감염자(단위:1/100)") +
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
