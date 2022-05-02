
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

ggplot(dat=dat1, aes(x=date, y=new_cases/1000, color=iso_code)) +
  labs(x="날짜", y="신규감염자(단위:1000명)") +
  geom_line() + geom_smooth(span=0.2) + facet_wrap(~iso_code)
