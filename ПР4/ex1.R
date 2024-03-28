#install.packages('sandwich')
#install.packages('lmtest')

library(sandwich) # оценка Var для гетероскедастичности
library(lmtest) # тест Бреуша-Пагана

fn <- "D:/Study/Матметоды/ПР4/data/flats_moscow.txt"
(h <- read.table(fn,header=TRUE))

plot(h$totsp,h$price)
h
h <- subset(h, select = - c(walk, brick, floor, code, n))

m <- lm(price~.,data=h)
summary(m)
#все переменные считаются значимыми
#модель значима
confint(m)
vcov(m) # ковариационная матрица коэффициентов регрессии - по OLS
vcovHC(m) #  состоятельная оценка ковариационной матрицы коэффициентов регрессии \\ подробнее в описании пакета sandwich. Наиболее актуальная форма “HC3”
coeftest(m,vcov=vcovHC(m))


m.resid <- resid(m)
par(mfrow = c(3, 2))
plot(h$totsp,m.resid)
abline(0, 0, col=4) 
plot(h$livesp,m.resid)
abline(0, 0, col=4) 
plot(h$kitsp,m.resid)
abline(0, 0, col=4) 
plot(h$dist,m.resid)
abline(0, 0, col=4) 
plot(h$metrdist,m.resid)
abline(0, 0, col=4) 

#totsp livesp dist

bptest(m)

(h1 <- h[order(h$totsp),])
(h2 <- h[order(h$livesp),])
(h3 <- h[order(h$dist),])
(h4 <- h[order(h$metrdist),])
(h5 <- h[order(h$kitsp),])
m1 <- lm(price~.,data=h1) 
m2 <- lm(price~.,data=h2) 
m3 <- lm(price~.,data=h3) 
m4 <- lm(price~.,data=h4) 
m5 <- lm(price~.,data=h5)

#H0 : гомоскедастичны
gqtest(m1,fraction=0.25)
gqtest(m2,fraction=0.25)
gqtest(m3,fraction=0.25)
gqtest(m4,fraction=0.25)
gqtest(m5,fraction=0.25)

#m3 и m4 гомоскедастичны, а это у нас dist, metrdist

#H0 : гомоскедастичны
bptest(m, ~ metrdist + I(metrdist^2), data = h)
bptest(m, ~ dist + I(dist^2), data = h)

#гомоскедастичен metrdist


# взвешенная регрессия
model1<- lm(price~ ., weights=I(1/totsp), data=h) # веса обратно пропорциональны переменной
summary(model1)
bptest(model1, data = h) # тест Бреуша-Пагана
bptest(model1, ~ totsp + I(totsp^2), data = h) # White's test
#Упала R, теперь остатки гомоскед

model2<- lm(price~ ., weights=I(1/livesp), data=h) # веса обратно пропорциональны переменной
summary(model2)
bptest(model2, data = h) # тест Бреуша-Пагана
bptest(model2, ~ livesp + I(livesp^2), data = h) # White's test
#тут ещё хуже с R, но теперь livesp гомоскед