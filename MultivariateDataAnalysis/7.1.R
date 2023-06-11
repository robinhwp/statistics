data7 = read.csv("./data/data7-1.csv")
data7_g1 = data7[data7$group=="g1", -1]
data7_g2 = data7[data7$group=="g2", -1]
g1_mean = sapply(data7_g1, mean)
g2_mean = sapply(data7_g2, mean)

n1 = nrow(data7_g1)
n2 = nrow(data7_g2)

cov_g1 = cov(data7_g1)
cov_g2 = cov(data7_g2)