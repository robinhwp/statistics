############# association rules #################
# install.packages("arules")
# install.packages("arulesViz")

########## basket1 data ###########################
setwd("c:/data")

t1 <- c("meat","rice","lettuce")
t2 <- c("meat","lettuce","cookie")
t3 <- c("rice","lettuce","cookie")
t4 <- c("meat","cookie","beverage")
t5 <- c("rice","lettuce","beverage")

b1 <- rbind(t1,t2,t3,t4,t5)
write.table(b1,"basket1.txt",quote=F,row.names=F, col.names=F, sep=",")

library(arules)
tr1 = read.transactions("basket1.txt", format="basket", sep=",")
as(tr1, "data.frame")

rules1 = apriori(tr1, parameter=list(supp=0.4, conf=0.4))
inspect(rules1)

########### Income data ######################

data(Income)
str(Income)
rules=apriori(Income, parameter=list(supp=0.4, conf=0.8))
inspect(rules)

############ Visualization ##########################

library(arulesViz)
plot(rules)
plot(rules,method="grouped")
plot(rules,method="graph")
plot(rules,method="paracoord")
