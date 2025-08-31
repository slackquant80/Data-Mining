ex2 = read.table("ex7-2.txt", header = T)

# 유클리디안.. 디폴트
dist(ex2)
dist(ex2, method = "manhattan")

## 계층적 군집분석(hierarchical cluster analysis)
clustering1 = hclust(dist(ex2, method = "manhattan"), method = "single")
clustering2 = hclust(dist(ex2, method = "manhattan"), method = "complete")
clustering3 = hclust(dist(ex2, method = "manhattan"), method = "average")

par(mfrow = c(1, 3))
plot(clustering1)
plot(clustering2)
plot(clustering3)
par(mfrow = c(1, 1))

# 나무형 그림의 확대
# 단일연결법에 의한 군집분석 결과 중 두 번째 부분을 출력
dendrogram1 = as.dendrogram(clustering1)
plot(dendrogram1[[2]])

## 분할분석
library(cluster)
ex2 = read.table("ex7-2.txt", header = T)
dianaclustering = diana(ex2, metric = "manhattan")
plot(dianaclustering)

## K-평균 군집분석
# 군집의 수를 2로 정하고, 앞서 살펴본 계층적 군집분석 결과를 이용해서 K-평균 군집분석을 수행한 결과

ex2 = read.table("ex7-2.txt", header = T)
ex2 = as.matrix(ex2)
aveclustering = hclust(dist(ex2), method = "average")
initialcent = tapply(ex2, list(rep(cutree(aveclustering, 2), ncol(ex2)), col(ex2)), mean)
kmclustering = kmeans(ex2, initialcent, algorithm = "MacQueen")
kmclustering
# 중심점은 (1.67, 2.33), (6.00, 5.25)
# 군집분석의 결과 : 'clustering vector'의 '1 1 1 2 2' --> 각 개체는 {1, 2, 3}, {4, 5}와 같이 군집이 이루어졌음
# 행렬로 바꾸어 줘야 함 -> as.matrix
# 다른 방법에 비해 효율적이나, 이상점이 있는 경우 바람직 하지 않은 결과 --> 이상점이 발견된 경우 제거하고 적용
# initialcent -> 초기값 가지고 있는 행렬
initialcent

cutree(aveclustering, 2)
rep(cutree(aveclustering, 2), ncol(ex2))
list(rep(cutree(aveclustering, 2), ncol(ex2)), col(ex2))
tapply(ex2, list(rep(cutree(aveclustering, 2), ncol(ex2)), col(ex2)), mean)
ex2

## 미국 주별 특성 데이터(state.x77) 군집분석
# state.x77 -> R에 내장되어 있는 데이터
# 모두 수치형 -> summary 함수로 간단하게 기술통계량 산출 가능
dim(state.x77)
summary(state.x77)
state.x77
# 변수 특징에 따라 수치적으로 범위가 매우 다양 -> 군집분석 특성상 단위에 영향... -> 표준화

statescale <- data.frame(scale(state.x77, center = TRUE, scale = TRUE))

sclustering1 = hclust(dist(statescale), method = "single")
sclustering2 = hclust(dist(statescale), method = "complete")
sclustering3 = hclust(dist(statescale), method = "average")

library(cluster)
sdianaclustering = diana(statescale)

plot(sclustering1)
plot(sclustering2)
plot(sclustering3)
plot(sdianaclustering)

## K-평균 군집분석
# 행렬로 바꿔주고 나서 적용해야 함!!!!!!
statescale <- as.matrix(statescale)
initial = tapply(statescale, list(rep(cutree(sclustering3, 4), ncol(statescale)),
                                  col(statescale)), mean)
skmclustering = kmeans(statescale, initial, algorithm = "MacQueen")
skmclustering

initial

list(rep(cutree(sclustering3, 4), ncol(statescale)), col(statescale))
cutree(sclustering3, 4)
rep(cutree(sclustering3, 4), ncol(statescale))

## 사전정보 없이 군집의 수를 1~40 증가시키면서 군집 내 변동과 군집 간 변동의 추이를 살펴보아
# 적절한 군집수를 파악하는 방법
library(ggplot2)
sscomp = data.frame()
nc = 1:40
set.seed(130)
for (k in nc) {
  clus = kmeans(statescale, k)
  sscomp = rbind(sscomp, c(k, clus$tot.withinss, clus$betweenss, clus$totss))
}
names(sscomp) = c("k", "SSwithin", "SSbetween", "SStotal")
ggplot(sscomp, aes(x = k)) + geom_point(aes(y = SSwithin), shape = 15,
                                        color = "black") +
  geom_line(aes(y = SSwithin), color = "black") +
  geom_point(aes(y = SSbetween), shape = 24, color = "blue") +
  geom_line(aes(y = SSbetween), color = "blue") + xlab("k = # of Clusters") +
  ylab("Within SS = Red square, Between SS = Blue triangle")
