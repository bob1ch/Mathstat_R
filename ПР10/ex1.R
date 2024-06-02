install.packages("reshape2")
install.packages("factoextra")
library('cluster')
library('reshape2')
library('ggplot2')
library('grDevices')

data_num <- subset(iris, select = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width))
df.stand <- as.data.frame(scale(data_num)) # стандартизация
(clus <- kmeans(df.stand, centers = 3)) #потому что цветка 3

n <- dim(df.stand)[[1]]
euc.dist <- as.matrix(dist(df.stand))
dist = melt(euc.dist)
df.stand$cluster <- clus$cluster
pairs <- data.frame(dist = dist, 
                    ca = as.vector(outer(1:n, 1:n,
                                         function(a, b) df.stand[a, 'cluster'])),
                    cb = as.vector(outer(1:n, 1:n,
                                         function(a, b) df.stand[b, 'cluster'])))
dcast(pairs, ca ~ cb, value.var = 'dist.value', mean) #расстояние между кластерами (всё ок, см. диагональ)
#set.seed(13)
c(kmeans(df.stand, centers = 3, nstart = 1)$tot.withinss,
  kmeans(df.stand, centers = 3, nstart = 500)$tot.withinss) # кластеризация устойчива, т.к. критери оптимальности не поменялся

k.max <- 25 # максимальное число кластеров
wss <- sapply(1:k.max, function(k){
  kmeans(df.stand, k, nstart = 50)$tot.withinss
})

plot(1:k.max, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Число кластеров K", 
     ylab = "Общая внутригрупповая сумма квадратов")
#Используя априорные знания и график я опредляю локоть на числе 3

library(factoextra)
fviz_nbclust(df.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fit <- kmeans(df.stand, 3,nstart = 50)
fit
df.stand$clusters <- factor(fit$cluster)

column_names <- names(df.stand)[0:4]
column_names
for (i in 1:(length(column_names) - 1)) {
  for (j in (i + 1):length(column_names)) {
    
    plot <- ggplot(df.stand, aes_string(x = column_names[i], y = column_names[j], color = "clusters")) +
      geom_point(size = 2) +
      theme_bw() +
      labs(title = paste("Scatter plot of", column_names[i], "vs", column_names[j]),
           x = column_names[i],
           y = column_names[j],
           color = "Cluster")
    
    
    print(plot)
  }
}

cl <- fit
cl$cluster
cl$cluster[cl$cluster==1]
cl$cluster[cl$cluster==2]
cl$cluster[cl$cluster==3]

plot(cl$centers[1,], col = 1, type="o", axes=FALSE, ylim=c(min(cl$centers),max(cl$centers)))
axis(1,at=1:5,lab=colnames(cl$centers))
axis(2)
points(cl$centers[2,], col=2)
points(cl$centers[3,], col=3)

lines(cl$centers[3,], col=3)
lines(cl$centers[2,], col=2)

legends = paste("кластер", 1:3, sep = "")
legend("top", legends, lty = "solid", pch=1, col = 1:4) 
#Смотрим на cl$centers и в частности на Petal.Length

# переменные, по которым различия между выделенными кластерами значимы
rez<-sapply(data_num, function(x) summary(aov(x ~ df.stand$cluster))[[1]][1,5])
names(rez[rez<0.05])
