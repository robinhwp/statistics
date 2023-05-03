# 7장.일반화선형모형

glider = read.csv("./data/sugar_glider_binomial.csv")
head(glider, 3)
logit_m1 = glm(occurr~p_size_km+con_metric, family = binomial(link=logit), data = glider)
(summ = summary(logit_m1))
# family = binomial(link=logit) : 반응변수의 확률분포는 이항분포고 연결함수는 로짓함수라는 의미

###
#  | 확률분포 | 연결함수 | 명령어 |
#  |---|---|---|
#  | 정규분포 | 항등함수 | gaussian(link="identity") |
#  | 이항분포 | 로진함수 | binomial(link="logit") |
#  | 포아송분포 | 로그함수 | poisson(link="log") |
#  | 감마분포 | 역수함수 | Gamma(link="inverse") |
#  | 준이항분포 | 로짓함수 | quasibionmial(link="logit") |
#  | 준포아송분포 | 로그함수 | quasipossion(link="log") |
#
(p.value = 1-pchisq(summ$null.deviance - summ$deviance, 
         summ$df.null - summ$df.residual))


# anova를 이용한 유의성 검정
logit_m0 = glm(occurr~1, family = binomial(link=logit), data = glider)
anova(logit_m0, logit_m1, test="Chisq")

(p.value = 1-pchisq(summ$deviance, 
                    summ$df.residual))


# 모형의 선택
logit_m2 = glm(occurr~p_size_km, family = binomial(link = logit), data = glider)
logit_m1 = glm(occurr~p_size_km+con_metric, family = binomial(link = logit), data = glider)
anova(logit_m2, logit_m1, test = "Chisq")

# AIC 함수
AIC(logit_m2, logit_m1)

# 변수선택방법 이용
library(MASS)
stepAIC(logit_m1, direction = "both")


p_size = seq(20, 230, 1)
hat_eta = predict(logit_m2, list(p_size_km=p_size), type="link")
par(mfrow = c(1,2))
with(glider, plot(p_size_km, occurr, xlab='구획의크기(x)', ylab="hat pi(x) \\ occurr", sub="(a)", pch=20))
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col="red")

glider_g=read.csv("./data/sugar_glider_binomial_g.csv")
plot(glider_g$p_size_med, glider_g$cases/glider_g$count, xlab="구획의크기(x)", ylim=c(0,1), ylab="hat pi(x)\\ sample prop.", sub='(b)', pch=20, col="blue")
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col="red")


y = cbind(glider_g$cases, glider_g$count - glider_g$cases)
logit_mg = glm(y~glider_g$p_size_med, family = binomial(link=logit))

summary(logit_mg)


f <- function(x){x^2}
x <- 1:250
plot(x, f(x), type='l')

f <- function(x){1/(1+exp(-x))}
x <- seq(-5, 5, .05)
plot(x, f(x), type='l')
