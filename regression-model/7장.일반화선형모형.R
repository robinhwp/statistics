# 7장.일반화선형모형

glider = read.csv("./data/sugar_glider_binomial.csv")
head(glider, 3)
logit_m1 = glm(occurr~p_size_km+con_metric, family = binomial(link=logit), data = glider)
summary(logit_m1)
# family = binomial(link=logit) : 반응변수의 확률분포는 이항분포고 연결함수는 로짓함수라는 의미
  | 확률분포 | 연결함수 | 명령어 |
  |---|---|---|
  | 정규분포 | 항등함수 | gaussian(link="identity") |
  | 이항분포 | 로진함수 | binomial(link="logit") |
  | 포아송분포 | 로그함수 | poisson(link="log") |
  | 감마분포 | 역수함수 | Gamma(link="inverse") |
  | 준이항분포 | 로짓함수 | quasibionmial(link="logit") |
  | 준포아송분포 | 로그함수 | quasipossion(link="log") |
  
  
  
f <- function(x){x^2}
x <- 1:250
plot(x, f(x), type='l')

f <- function(x){1/(1+exp(-x))}
x <- seq(-5, 5, .05)
plot(x, f(x), type='l')
