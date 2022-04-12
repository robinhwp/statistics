
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
