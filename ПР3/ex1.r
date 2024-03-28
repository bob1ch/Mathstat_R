#------------1--------------
data <- read.table(file='clipboard', header=TRUE)
x <- c(data$t)
x[x == 0] = 0.0001
y1 <- data$x1
y2 <- data$x2
y3 <- data$x3
y4 <- data$x4
y5 <- data$x5
#x - время
#y - титр клеток
plot(x, y3) #зависимость y(x)
#аналогичный график для y1-5

#------------2--------------
Tukey_Transformation <- function(y_s, x){
  for (y in 1:length(y_s[1,])){
    test_data<-cbind(y_s[,y], x)
    l1<-seq(-5,-0.1, by =0.1)
    l2<-seq(0.1,5.0, by =0.1)
    c1<-numeric(50); c2<-numeric(50)
    for (i in 1:length(l1)) c1[i]<-abs(cor(test_data[,1], -test_data[,2]^l1[i]))
    for (i in 1:length(l2)) c2[i]<-abs(cor(test_data[,1], test_data[,2]^l2[i]))
    c3<-abs(cor(test_data[,1], log(test_data[,2])))
    
    plot(c(l1,0,l2), c(c1,c3,c2))
    abline(v=c(l1, l2)[which.max(c(c1,c3,c2))], col = 3)
    abline(h=c(c1,c3,c2)[which.max(c(c1,c3,c2))], col = 3)
    
    abline(v=1, col = 2, lty = 'dotted')
    abline(h=c2[which(l2 == 1)], col = 2, lty = 'dotted')
    
    #which(l2 == 1)
    #which.max(c(c1,c3,c2))
    
    max_corr = c(c1,c3,c2)[which.max(c(c1,c3,c2))] # max corr
    degree = c(l1, l2)[which.max(c(c1,c3,c2))]
    print('------------------')
    print(y)
    print('max_corr')
    print(max_corr)
    print('degree')
    print(degree)
  }
}
Tukey_Transformation(cbind(y1, y2, y3, y4, y5), x)

#------------3+4--------------
#давайте логарифмируем все, что захотим B)
#x_t = x_0 * e^rt
#ln(x_t) = ln(x_0) + rt * ln(e)
#ln(x_t) - ln(x_0) = rt
#r = (ln(x_t) - ln(x_0)) / t

x_0 = c(4.905275, 5.141664, 4.094345, 5.529429, 4.663439) #тут уже посчитаный логарифм
t = c(24,  48,  72,  96, 120, 144, 168, 192, 216)

x1 = log(c(245,   374,   545,   839,  1544,  2392,  3433,  6586, 10129))
x2 = log(c(270,   491,   693,  1163,  1788,  3460,  4704,  8526, 13198))
x3 = log(c(113,  186,  269,  447,  796, 1024, 2131, 3107, 4351))
x4 = log(c(371,   710,  1088,  1772,  2534,  4842,  6478, 10429, 19953))
x5 = log(c(201,  275,  451,  689, 1304, 2161, 3386, 5326, 8928))

r1 <- c(x1 - x_0[1]) / t
#r1 <- cbind(r1, c(x2 - x_0[2]) / t)
#r1 <- cbind(r1, c(x3 - x_0[3]) / t)
#r1 <- cbind(r1, c(x4 - x_0[4]) / t)
#r1 <- cbind(r1, c(x5 - x_0[5]) / t)

#строки выше закоментил, чтобы найти медиану для x1
t1 = log(2) / r1

median(r1)
median(t1)

#------------5--------------
fit1 <- lm(x1 ~ I(-t^-4.1))
fit2 <- lm(x1 ~ I(-log(t)^-0.02))

summary(fit1)
summary(fit2)
