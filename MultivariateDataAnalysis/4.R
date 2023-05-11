

beer.data = read.table('./data/beerbrand.csv', header=T, sep=",", row.names=1)
head(beer.data)
summary(beer.data)
str(beer.data)
zbeer=scale(beer.data)
round(apply(zbeer, 2, mean), 3)
round(apply(zbeer, 2, sd), 3)
