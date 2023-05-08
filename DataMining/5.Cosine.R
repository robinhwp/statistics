install.packages("neuralnet")
library(neuralnet) 
set.seed(130)
ind1 = 1:100
ind2 = ind1/100
cos2 = cos(ind2*4*pi)
cdat = data.frame(cbind(ind2, cos2)) # 신경망모형에서 데이터 형태
cos2.nn = neuralnet(cos2~ind2, data=cdat, hidden=5, linear.output=T)
plot(cos2.nn)

cos.pred = predict(cos2.nn, data.frame(ind2))
plot(ind1, cos.pred)
lines(cos2)
