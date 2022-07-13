
# 예제 6-7
n = 10; s = -0.2; bar_x = 12.2
alpha = 0.05
ttest=(bar_x-12)/(0.2/sqrt(10))   # 검정통계량
ttest_cr = qt(1-alpha/2,n-1)        # 기각역
ttest_pv = (1-pt(ttest,n-1))*2       # 유의확률
cat("검정통계량값: ", ttest, "기각역: ", ttest_cr, "유의확률: ", ttest_pv)

# 예제 6-8
book = c(5, 23, 20, 1, 10, 15, 15, 10, 9, 13, 18, 11, 18, 20, 19, 19)
t.test(book,mu=11, alternative = "greater") 

# 예제 6-9
p0 = 0.6; n = 50 ; hat_p = 0.7
alpha = 0.05
ptest =(hat_p - p0)/sqrt(p0*(1-p0)/n)
ptest_cr = qnorm(1-alpha)
ptest_pv = 1-pnorm(ptest)
cat("검정통계량값: ", ptest, "기각역: ", ptest_cr, "유의확률: ", ptest_pv)

# 예제 6-10
n=12
alpha=0.05
book = c(5, 23, 20, 1, 10, 15, 15, 10, 9, 13, 18, 11)
vtest = var(book)*(12-1)/4^2
vtest_cr = qchisq(1-alpha, n-1)
vtest_pv = 1-pchisq(vtest, n-1)
cat("검정통계량값: ", vtest, "기각역: ", vtest_cr, "유의확률: ", vtest_pv)
