setwd("c:/data")
X1 <- c(1,1,3,5,7)
X2 <- c(1,2,4,5,5.5)
ex72 <- data.frame(X1,X2)
write.table(ex72, "ex7-2.txt", quote=F)

ex2=read.table("ex7-2.txt", header=T)
dist(ex2)
dist(ex2, method="manhattan")

clustering1 = hclust(dist(ex2,method="manhattan"), method="single")
clustering2 = hclust(dist(ex2,method="manhattan"), method="complete")
clustering3 = hclust(dist(ex2,method="manhattan"), method="average")
 
par(mfrow=c(1,3))
plot(clustering1)
plot(clustering2)
plot(clustering3)
dev.off()

dendrogram1 = as.dendrogram(clustering1)
plot(dendrogram1[[2]])

install.packages("cluster")
library(cluster)
ex2=read.table("ex7-2.txt", header=T)
dianaclustering = diana(ex2, metric="manhattan")
plot(dianaclustering)


ex2=read.table("ex7-2.txt", header=T)
ex2=as.matrix(ex2)
aveclustering = hclust(dist(ex2), method="average")
initialcent = tapply(ex2, list(rep(cutree(aveclustering,2), ncol(ex2)), col(ex2)),mean)
kmclustering = kmeans(ex2, initialcent, algorithm = "MacQueen")
kmclustering


dim(state.x77)
summary(state.x77)
statescale = data.frame(scale(state.x77, center=TRUE, scale=TRUE))
statescale = as.matrix(statescale)

sclustering1 = hclust(dist(statescale), method="single")
sclustering2 = hclust(dist(statescale), method="complete")
sclustering3 = hclust(dist(statescale), method="average")

library(cluster)
sdianaclustering = diana(statescale)

par(mfrow=c(2,3))
plot(sclustering1)
plot(sclustering2)
plot(sclustering3)
plot(sdianaclustering)


initial = tapply(statescale, list(rep(cutree(sclustering3, 4), ncol(statescale)), col(statescale)), mean)
skmclustering = kmeans(statescale, initial, algorithm = "MacQueen")
skmclustering

sscomp = data.frame()
nc = 1:40
set.seed(130)
for (k in nc) {
clus = kmeans(statescale, k)
sscomp = rbind(sscomp, c(k, clus$tot.withinss, clus$betweenss, clus$totss))
}
names(sscomp) = c("k", "SSwithin", "SSbetween", "SStotal")

library(ggplot2)
ggplot(sscomp, aes(x=k))+ geom_point(aes(y=SSwithin), shape=15, color="red")+
geom_line(aes(y=SSwithin), color="red")+ geom_point(aes(y=SSbetween), shape=24, color="blue")+ geom_line(aes(y=SSbetween), color="blue")+ 
xlab("k= # of Clusters")+ ylab("Within SS= Red square, Between SS= Blue triangle")

