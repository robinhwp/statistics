# 중회귀모형 (multi linear regression model)

# 파일내용(market-2.txt)
# ID   X1    X2    Y   
# 1  4.2   4.5    9.3
# 2  8.5  12.0   18.5
# 3  9.3  15.0   22.8
# 4  7.5   8.5   17.7
# 5  6.3   7.4   14.6
# 6 12.2  18.5   27.9
# 7  6.5   5.5   12.5
# 8 10.4  16.5   25.2
# 9  5.8   3.7   10.8
# 10  9.2  13.5   20.5
# 11  7.2   5.2   14.9
# 12  8.5  15.0   19.2
# 13 10.6  14.4   22.5
# 14 13.9  13.3   28.4
# 15 12.7  12.5   25.6

#파일로드
market2=read.table("./data/market-2.txt", header = T)
head(market2, 3)
X=market2[,c(2,3)]
Y=market2[,4]
X=cbind(1,X)
X=as.matrix(X)
Y=as.matrix(Y)
XTX=t(X)%*%X
XTY=t(X)%*%Y
beta=solve(XTX)%*%XTY
beta=round(beta,3)

#적합된 회귀식은 hat_Y=hat_beta0 + hat_beta1*X1 + hat_beta2 * X2 가 된다.
paste0("적합된 회귀식은 hat_Y=", beta[1,1], "+", beta[2,1],"X1+",beta[3,1], "X2 가 된다.")
X1=10; X2=10 # 이라고 하면
paste0("적합된 회귀식은 hat_Y=", beta[1,1], " + ", beta[2,1],"*", X1, " + ",beta[3,1],"*", X2,"=", +
         beta[1,1]+beta[2,1]*X1+beta[3,1]*X2," 이 된다.")


# 중회귀모형 적합
###########################################
market2.lm = lm(Y~X1+X2, data = market2)
market2.summary = summary(market2.lm)
#적합된 회귀식은 
paste0("hat_Y=", 
       round(market2.summary$coefficients["(Intercept)","Estimate"],5), " + ", 
       round(market2.summary$coefficients["X1","Estimate"],5), "*X1 + ", 
       round(market2.summary$coefficients["X2","Estimate"],5), "*X2")

# 각종 주요 정보
paste0("Residual Standard error = ", round(market2.summary$sigma, 4))
paste0("Multiple R-squared = ", round(market2.summary$r.squared, 5))
paste0("F-statistic = ", round(market2.summary$fstatistic["value"], 1))
paste0("p-value = ", round(1-pf(market2.summary$fstatistic["value"], market2.summary$fstatistic["numdf"], market2.summary$fstatistic["dendf"]), 14))

# 분산분석표
market2.anova=anova(market2.lm)
paste("SS(X1) =", market2.anova["X1", "Sum Sq"])
paste("SS(X2|X1) =", market2.anova["X2", "Sum Sq"])
paste("SS(X1,X2) =", market2.anova["X1", "Sum Sq"]+market2.anova["X2", "Sum Sq"])
length(market2.anova[,"Df"])-1

market2.anova["Residuals","Df"]
market2.anova["Residuals","Sum Sq"]
market2.anova["Residuals","Mean Sq"]
sqrt(market2.anova["Residuals","Sum Sq"]/market2.anova["Residuals","Df"])

length(rownames(market2.anova))

market2.anova[rownames(market2.anova)[3],"Df"]


# 표준화된 중회귀분석
#############################################
