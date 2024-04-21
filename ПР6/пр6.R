library(ggplot2)
library(lmtest)

fn <- "D:/Study/Матметоды/ПР6/data/flats_moscow.txt"
(h <- read.table(fn,header=TRUE))
h
p.msk <- subset(h, select = - c(code, n))

ggplot(p.msk, aes(totsp, price, col = factor(brick))) +
  geom_point(aes(pch = factor(brick))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap(~ factor(brick))

ggplot(p.msk, aes(totsp, price, col = factor(brick))) +
  geom_point(aes(pch = factor(brick))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap(~ factor(dist))

plot(p.msk$brick,p.msk$price)

m1 <- lm(price~., data = p.msk)
summary(m1)
# Модель значимая, все переменные также значимые
confint(m1)
vcov(m1) > 1

m1.resid <- resid(m1) # получаем остатки регрессии
plot(p.msk$totsp,m1.resid)
abline(0, 0, col=4) 
plot(p.msk$dist,m1.resid)
abline(0, 0, col=4) 
plot(p.msk$brick,m1.resid)
abline(0, 0, col=4) 
#Ну и там остальные конечно же, но перейдем к численным методам

bptest(m1,studentize=FALSE) # тест Бреуша-Пагана

#------------Пробуем бороться------------------------
#wt <- 1 / lm( abs (m1$residuals) ~ m1$fitted.values )$fitted.values^2
m2 <- lm(price~., weights=wt, data = p.msk)
summary(m2)
# Модель значимая, все переменные также значимые
confint(m2)
vcov(m2) > 1
bptest(m2,studentize=FALSE)


#----------2---------------------
df <- read.table("clipboard", dec='.', header=T)
df

model = lm(фев ~ янв, data=df)
summary(model)
