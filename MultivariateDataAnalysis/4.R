

beer.data = read.table('./data/beerbrand.csv', header=T, sep=",", row.names=1)
head(beer.data)
summary(beer.data)
str(beer.data)
zbeer=scale(beer.data)
round(apply(zbeer, 2, mean), 3)
round(apply(zbeer, 2, sd), 3)



auto = read.csv("./data/auto.csv")
head(auto)

X=auto[,-1]
# z-standardization
zX = scale(X, center = TRUE, scale = TRUE)
# 0-1 transformation
maxX = apply(X, 2, max)
minX = apply(X, 2, min)
z01X = scale(X, center = minX, scale = maxX - minX)

