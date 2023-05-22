

Y1 = matrix(c(0, 2, 5, 0, 4, 6, 2, 0, 1), 3,3, byrow = TRUE)
Y2 = matrix(c(2, 0, 0, 3, 7, 0, 4, 1, 2), 3,3, byrow = TRUE)

df = data.frame()

# Y=1, Y=2의  원래 데이터 생성
for(j in 1:3)
  for(i in 1:3)
  {
    if(Y1[j,i]>0)
    {
      for(k in 1:Y1[j,i])
        df = rbind(df, c(j, i, 1))
    }
  }

for(j in 1:3)
  for(i in 1:3)
  {
    if(Y2[j,i]>0)
    {
      for(k in 1:Y2[j,i])
        df = rbind(df, c(j, i, 2))
    }
  }
names(df) = c("X1", "X2", "Y")

df$X1 = factor(df$X1)
df$X2 = factor(df$X2)
df$Y = factor(df$Y)
print(df)


library(rpart)

#stopping rule 1
mycontrol = rpart.control(maxdepth = 1)
df_fit = rpart(Y~., control=mycontrol, data = df)
print(df_fit)

