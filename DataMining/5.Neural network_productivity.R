# install.packages('neuralnet')
# install.packages("dummy")
# setwd("c:/data")
 library(neuralnet)
 library(dummy)
 prod =  read.csv("./data/productivityREG.csv", header=TRUE)
 prod$quarter = factor(prod$quarter)
 prod$department = factor(prod$department)
 prod$day = factor(prod$day)
 prod$team = factor(prod$team) 
 # Create dummy variables
 dvar = c(1:4)
 prod2 = dummy(x=prod[,dvar])
 # L개의 범주가 있으면 L개의 더미변수들을 사용하려면 아래의 코드를 사용하지 않음.
 # 교재와 동일하게 하려면 ...
 # prod2 = prod2[,-c(5, 7, 13, 25)] # 모형비교를 위한 데이터세트 생성(범주수-1)
                                  # 이를 삭제하면 범주 수만큼의 가변수 생성
 prod2 = cbind(prod[,-dvar], prod2)
 # 수치적 변수가 아닌 경우에는 수치적 변수로 만들기
 for(i in 1: ncol(prod2)) if(!is.numeric(prod2[,i])) prod2[,i] = as.numeric(prod2[,i])
 # Standardization 
 max1 = apply(prod2, 2, max)
 min1 = apply(prod2, 2, min)
 sdat = scale(prod2, center = min1, scale = max1 - min1) 
 sdat = as.data.frame(sdat)
 # 포뮬러를 쉽게 만들기 위한 코드
 pn = names(sdat)
 f = as.formula(paste("productivity ~", paste(pn[!pn %in% "productivity"], collapse = " + "))) 
 set.seed(1234)
 fit.nn = neuralnet(f, data = sdat, hidden=c(3,1), linear.output=T) 
 plot(fit.nn)
 # Prediction
 pred.nn = predict(fit.nn, sdat)
 pred.nn = pred.nn*(max1[7]-min1[7])+min1[7] # 표준화 값에서 원래값으로 다시 변환
 # Mean Squared Error (MSE)
 mean((prod2$productivity-pred.nn)^2) 
 # Scatter plot (Observed vs. Fitted)
 plot(prod2$productivity, pred.nn, xlab="Observed Values", ylab="Fitted Values")
 abline(0,1)
 