#----------1---------
fn <- "D:/Study/Матметоды/ПР7/data/flats_moscow.txt" # code - код района
(h <- read.table(fn, header=TRUE))
data <- subset(h, select = - c(n, code))
data
m = lm(price ~ ., data=data)
summary(m)

#для какого-то набора данных
a <- data[5:15, ]
predict(m, a, interval='prediction')

#для средних
b <- colMeans(data)
b['walk'] = round(b['walk'])
b['brick'] = round(b['brick'])
b['floor'] = round(b['floor'])
b
predict(m, b, interval='prediction')

#----------2---------
library(lmtest)

df <- read.table("clipboard", dec='.', header=T)
df

m.y <- lm(урожай ~ температура+осадки, df) # зависимость урожая от температуры и осадков
summary(m.y)

dwtest(m.y) # Durbin-Watson test
bgtest(m.y, order = 1)
bg2<-bgtest(m.y, order = 1) # тест Бреуша-Годфри
coeftest(bg2)

x1 <- df[,2]
x2<- df[,3]
y <- df[,4]
relation <- lm(y~x1+x2)
a <- data.frame(x1 = c(35,28), x2 = c(25,26))
result1 <-  predict(relation,a, interval = "prediction")
print(result1)
###   fit      lwr      upr
###1 68.91805 65.31940 72.51669
###2 64.03530 60.33682 67.73378

r<- 0.899 # из регрессии теста Беруша-Годфри
(y.r<-df[2:11,4]-r*df[1:10,4])
x1.r <- df[2:11,2]-r*df[1:10,2]
x2.r <- df[2:11,3]-r*df[1:10,3]
m.yr<-lm(y.r~x1.r+x2.r)
summary(m.yr)
dwtest(m.yr) # Durbin-Watson test
