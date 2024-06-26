###### ���������� ������ #############################################
library('cluster')
library('reshape2')
library('ggplot2')
library('grDevices')

# k-maens // ������ - 10
mtcars

df.stand <- as.data.frame(scale(mtcars))
(clus <- kmeans(df.stand, centers = 5))

#�������� �������� ������������ (�� ������� ��������� ������� 
#������� ���������������� ����������, ������������� ���������� - �������������� ��������).
n <- dim(df.stand)[[1]] 
euc.dist <- as.matrix(dist(df.stand))
dist = melt(euc.dist)
df.stand$cluster <- clus$cluster
pairs <- data.frame(dist = dist, 
                    ca = as.vector(outer(1:n, 1:n,
                                         function(a, b) df.stand[a, 'cluster'])),
                    cb = as.vector(outer(1:n, 1:n,
                                         function(a, b) df.stand[b, 'cluster'])))
dcast(pairs, ca ~ cb, value.var = 'dist.value', mean)

# ������������� �������� (���������������� � ���������� �������������)
# �������� � ����������� Wtotal - �������� ������� �������������
set.seed(13)
c(kmeans(df.stand, centers = 5, nstart = 2)$tot.withinss,
  kmeans(df.stand, centers = 5, nstart = 50)$tot.withinss) 

# ����� �����
k.max <- 31 # ������������ ����� ���������
wss <- sapply(1:k.max, function(k){
  kmeans(df.stand, k, nstart = 50)$tot.withinss
})

plot(1:k.max, wss, type = "b", pch = 19, frame = FALSE, 
      xlab = "����� ��������� K", 
      ylab = "����� ��������������� ����� ���������")

# ��������� ������ � ������� fviz_nbclust():
library(factoextra)
fviz_nbclust(df.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fit <- kmeans(df.stand, 4,nstart = 50)
df.stand$clusters <- factor(fit$cluster)

ggplot(mtcars, aes(vs,qsec, col = df.stand$clusters))+
  geom_point(size = 2)+
  theme_bw() 

cl <- fit
cl$cluster
cl$cluster[cl$cluster==1]
cl$cluster[cl$cluster==2]
cl$cluster[cl$cluster==3]
cl$cluster[cl$cluster==4]

plot(cl$centers[1,], col = 1, type="o", axes=FALSE, ylim=c(min(cl$centers),max(cl$centers)))
axis(1,at=1:12,lab=colnames(cl$centers))
axis(2)
points(cl$centers[2,], col=2)
points(cl$centers[3,], col=3)
points(cl$centers[4,], col=4)


lines(cl$centers[3,], col=3)
lines(cl$centers[2,], col=2)
lines(cl$centers[4,], col=4)

legends = paste("�������", 1:4, sep = "")
legend("top", legends, lty = "solid", pch=1, col = 1:4) 


# ����������, �� ������� �������� ����� ����������� ���������� �������
rez<-sapply(mtcars, function(x) summary(aov(x ~ df.stand$cluster))[[1]][1,5])
names(rez[rez<0.05])

# ������������� ������������� // ������ - 11
(fm<-read.table("clipboard", h=T, dec=",")) # ������ �� ������

fm1<-scale(fm) # �������������
hc <- hclust(dist(fm1),  "ward.D2" ) #  "complete"// "single" // "ave" // "cen" // "ward.D2"
plot(hc)
plot(hc, hang = -1)

(cars<-read.table("clipboard", h=T, dec=","))

hc <- hclust(dist(cars), "ward.D2" ) #  "complete"// "ward.D2"// "ave" // "cen" // "ward.D2"
plot(hc)
plot(hc, hang = -1)

library(pvclust)
set.seed(13)
#  �������� �������� � ������ BP- � AU- ������������ ��� �����
cars.pv <- pvclust(t(cars), nboot = 100, method.dist = "euclidean", 
                     method.hclust = "ward.D2", quiet = TRUE)
plot(cars.pv)  # ������������ � p-����������
pvrect(cars.pv) # ��������� ����������� ����������


hc <- hclust(dist(mtcars), "ward.D2" ) #  "complete"// "ward.D2"// "ave" // "cen" // "ward.D2"
plot(hc)
plot(hc, hang = -1)
rect.hclust(hc, 4) # ������� �������� ����� ���������, ������ ����� 2


cars.pv <- pvclust(t(df.stand), nboot = 1000, method.dist = "euclidean", 
                   method.hclust = "ward.D2", quiet = F)
plot(cars.pv)  # ������������ � p-����������
pvrect(cars.pv) # ��������� ����������� ����������

USArrests
