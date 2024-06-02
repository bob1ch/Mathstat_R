#1-------
data <- read.table('clipboard', dec=',', header = T)
var(data[1:2,])
for (i in c(1:(length(data[,1])-1)))
{
  for (j in c(i:length(data[,1])))
  {
    c1 = data[i,]
    c2 = data[j,]
    vec = cbind(c1, c2)[, 1]
    srednee = (vec - mean(vec))^2 / length(vec)
    print(vec)
    
  }
}
#2-------
install.packages('pvclust')
library(pvclust)

Ar.scale <- scale(USArrests)
hc <- hclust(dist(Ar.scale),  "ward.D2" ) #  "complete"// "single" // "ave" // "cen" // "ward.D2"
plot(hc)
plot(hc, hang = -1)
rect.hclust(hc, 4)

ars.pv <- pvclust(t(Ar.scale), nboot = 100, method.dist = "euclidean", 
                  method.hclust = "ward.D2", quiet = TRUE)
plot(ars.pv)  # дендрограмма с p-значениями
pvrect(ars.pv)

#свойства кластеров----
df.stand <- as.data.frame(scale(USArrests))
fit <- kmeans(df.stand, 13,nstart = 50)
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
for (i in c(1:13)){
  cl$cluster[cl$cluster==i]
}


plot(cl$centers[1,], col = 1, type="o", axes=FALSE, ylim=c(min(cl$centers),max(cl$centers)))
axis(1,at=1:4,lab=colnames(cl$centers))
axis(2)
for (i in c(2:13)){
  points(cl$centers[i,], col=i)
}

for (i in c(2:13)){
  lines(cl$centers[i,], col=i)
}


legends = paste("кластер", 1:13, sep = "")
legend("top", legends, lty = "solid", pch=1, col = 1:13) 
#Смотрим на cl$centers и в частности на Petal.Length

# переменные, по которым различия между выделенными кластерами значимы
rez<-sapply(USArrests, function(x) summary(aov(x ~ df.stand$cluster))[[1]][1,5])
names(rez[rez<0.05])
