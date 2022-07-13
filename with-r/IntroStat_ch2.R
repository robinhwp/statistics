

### Chapter 2.5

a<-1
b<-2
c<-a+b
c

height <- c(165, 151, 162, 160, 151, 152, 159, 163, 143, 161)

d<-1:3
d
e<-seq(1, 9, 2)
e
f<-rep(10, 5)
f
g<-c(d, f)
g
h<-c(4:1, seq(0, 9, 3))
h

e+f
e-f
e*f
e/f
d+f

i<-1:4
j<-as.factor(1:4)
j
i+1
j+1
k<-as.character(1:4)
k
l<-c("K", "N", "O", "U")
l
m<-i>2
m

n<-rep(10, 5)
o<-1:5
p<-cbind(n, o)
p
q<-rbind(n, o)
q
r<-matrix(1:4, 2, 2)
r
s<-matrix(c(1, 4, 2, 7), 2, 2)
s
r+s
r %*% s
solve(s)
s[1,2]
s[1,]
s[,2]

name<-c("Kim", "Lee", "Park", "Choi")
age<-c(20, 32, 17, 51)
sex<-as.factor(c("Male", "Female", "Female", "Female"))
dat<-data.frame(name, age, sex)
dat$age
dat$name
dat$sex



### Chapter 2.6

# 패키지 로드
library(ggplot2)
library(forcats)
# 데이터 입력
transp<-c("bicyle", "bus", "bus", "walking", "bus", "bicyle", "bicyle", 
          "bus", "bus", "bus", "bicyle", "bus", "bicyle", "bicyle", "walking", 
          "bus", "bus", "bicyle", "bicyle", "walking", "walking", 
          "bicyle", "bus", "bus", "bus", "bus", "bicyle", 
          "bus", "bus", "bicyle", "bicyle", "bicyle")
dat1<-data.frame(transp)
# 막대그래프 작성
ggplot(data=dat1) + geom_bar(mapping=aes(x=fct_infreq(transp))) + xlab("Transportation")

# 데이터 입력
obesity<-factor(c("underweight", "normal", "overweight", "obese"), 
                levels=c("underweight", "normal", "overweight", "obese"))
count<-c(6, 69, 27, 13)
perc<-count/sum(count)*100
dat2<-data.frame(obesity, count, perc)
# 막대그래프 작성
ggplot(data=dat2) + geom_bar(mapping=aes(x=obesity, y=perc), stat="identity") + xlab("Obesity") + ylab("Percentage (%)")

# 도수분포표 형태로 데이터 정리
table(transp)
dat3<-data.frame(transportation=c("bus", "bicyle", "walking"), count=c(15, 13, 4))
# 원그래프 작성
ggplot(data=dat3) + geom_bar(mapping=aes(x="", y=count, fill=transportation), stat="identity") + 
  coord_polar("y", start=0) + xlab("") + ylab("")

# 데이터 입력
score<-c(93, 83, 91, 68, 75, 87, 89, 96, 97, 67, 83, 81, 87, 80, 64, 
         83, 88, 76, 91, 78, 72, 80, 69, 80, 84, 71, 91, 81, 88, 73)
# 히스토그램 작성
hist(score, main="")

# 데이터 입력
rv<-c(0.8, 0.8, 0.8,  0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1.8, 2, 2.1, 2.3, 2.4, 2.8,
      2.9, 3, 3.2, 3.3, 3.5, 3.8, 3.8, 3.9, 4, 4.2, 4.4, 4.5, 5.1, 5.3, 5.3, 5.4,
      14, 17, 18, 19, 21, 21, 23, 25, 27, 28, 32, 34, 36, 41, 42, 44, 48, 49,
      51, 54, 59, 60, 61, 62, 80, 240)
# 히스토그램 작성
hist(rv, main="", xlab="CRP", breaks=20)

# 난수 생성
set.seed(2021)
rn<-c(rnorm(100, 5, 2), rnorm(100, 10, 2))
# 히스토그램 작성
hist(rn)
hist(rn, breaks=20, main="", xlab="value")
hist(rn, breaks=5, main="", xlab="value")

# 데이터 입력
age<-c(57, 61, 47, 57, 48, 58, 57, 61, 54, 50, 68, 51)
# 평균 계산
m.age<-mean(age)
# 점도표 작성
par(xpd=TRUE )
stripchart(age, axes=F, pch=19, xlim=c(45, 70), method="stack", offset=5, cex=1.5)
axis(1, at=seq(45, 70, 5))
points(m.age, -5, pch=17, cex=2, col="red")

# 데이터 입력
books<-c(6, 0, 1, 3, 1, 5, 2, 3, 1, 3, 67)
m.books<-mean(books)
# 점도표 작성
par(xpd=TRUE )
stripchart(books, axes=F, pch=1, xlim=c(0, 70), method="stack", offset=5, cex=1.5)
axis(1, at=seq(0, 70, 10))
points(m.books, -5, pch=17, cex=2, col="red")

# 데이터 입력
age<-c(57, 61, 47, 57, 48, 58, 57, 61, 54, 50, 68, 51)
# 다섯 수치요약
fivenum(age)
# 상자그림 작성
boxplot(age, ylab="Age")

# 데이터 입력
member<-c(92, 107, 180, 90, 78, 91, 102, 88, 106, 125, 95, 102, 162)
# 상자그림 작성
boxplot(member, ylab="Number of board members")










