
#customerID 는 row.name으로 사용하고 첫번째 행은 열이름으로 사용
mall = read.csv("./exdata/mall_customer.csv", header = T, row.names = 1)
head(mall)
# 기술통계량 확인
summary(mall)

mallf = mall[mall$Gender=="Female",2:4]
mallm = mall[mall$Gender!="Female",2:4]

zmallf = scale(mallf)
zmallf_euc = dist(zmallf, method = "euclidean")

hcf_w = hclust(zmallf_euc, method = "ward.D")
plot(hcf_w, hang=-1)

hcf_c = hclust(zmallf_euc, method = "complete")
plot(hcf_c, hang=-1)


kmcf = kmeans(zmallf, centers = 6)
kmcf

# 소속 군집 산점도
plot(zmallf, col=kmcf$cluster, pch=16)

pairs(zmallf, col=kmcf$cluster, pch=16, cex.labels = 1.5)





zmallm = scale(mallm)
zmallm_euc = dist(zmallm, method = "euclidean")

hcm_w = hclust(zmallm_euc, method = "ward.D")
plot(hcm_w, hang=-1)

hcm_c = hclust(zmallm_euc, method = "complete")
plot(hcm_c, hang=-1)


kmcm = kmeans(zmallm, centers = 6)
kmcm

# 소속 군집 산점도
plot(zmallm, col=kmcm$cluster, pch=16)

pairs(zmallm, col=kmcm$cluster, pch=16, cex.labels = 1.5)
