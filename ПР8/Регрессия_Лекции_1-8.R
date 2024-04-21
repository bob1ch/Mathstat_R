#######Повторение: Регрессионный анализ ###############
y <- c(109.14, 117.55, 106.76, 115.26, 117.13, 125.39, 121.03,114.03, 124.83, 113.92, 122.04, 109.41, 131.61, 103.93, 116.64, 117.06, 111.73, 120.41, 112.98, 101.20, 120.19,
       128.53, 120.14, 108.70, 130.77, 110.16, 129.07, 123.46,130.02, 130.31, 135.06, 129.17, 137.08, 107.62, 139.77,
       121.47, 130.95, 138.15, 114.31, 134.58, 135.86, 138.49,
       110.01, 127.80, 122.57, 136.99, 139.53, 127.34, 132.26,
       120.85, 124.99, 133.36, 142.46, 123.58, 145.05, 127.83,
       140.42, 149.64, 151.01, 135.69, 138.25, 127.24, 135.55,
       142.76, 146.67, 146.33, 137.00, 145.00, 143.98, 143.81,
       159.92, 160.97, 157.45, 145.68, 129.98, 137.45, 151.22, 136.10, 150.60, 148.79, 167.93, 160.85, 146.28, 145.97,
       135.59, 156.62, 153.12, 165.96, 160.94, 168.87, 167.64,
       154.64, 152.46, 149.03, 159.56, 149.31, 153.56, 170.87,
       163.52, 150.97)
x <- rep(seq(16, 65, 1), each = 2)
plot(x,y)
##### b=cov(x,y)/var(x); a=mean(y)-mean(x)b
cov(x,y)
var(x)
(b<-cov(x,y)/var(x))
(a<-mean(y)-mean(x)*b)
lines(x,a+b*x, col="blue")

fit<-lm(y ~ x)
summary(fit)
plot(fit$residuals)
abline(h=0, col='red')

##### матричная запись #############################
(X<-cbind(rep(1,100),x)) # матрица Х
(X1<-t(X)%*%X) # X'X
(X2<-solve(X1)) # обратная матрица для X'X 
X2%*%t(X)%*%y # вектор коэффициентов

##### lm () ######################
?mtcars
df<-mtcars[,c(1,3:6)]

summary(lm(df$mpg~df$disp+df$hp+df$wt)) #


fit <- lm(wt ~ ., data = df)
summary(fit)

fit <- lm(wt ~(.)*(.), data = df)
summary(fit)

###### Лекция 2. ########################################
###### Полная мультиколлинеарность ######################
library(dplyr) 
library('car') # VIF
library(ggplot2)
set.seed(42)

d<-tibble(y = rnorm(50), x1= rnorm(50),
              x3= rnorm(50), x2 = x1)

pairs(d)
fit<- lm( y ~ ., d)
summary(fit)

###### Частичная мультиколлинеарность ######################
?cars
ggplot(cars, aes(speed, dist)) +
  geom_point() +
#geom_smooth(method = 'lm', col = 'red') +
  geom_smooth() # заметна нелинейность

fit1<- lm( dist ~ ., cars)
summary(fit1)

cars <- mutate(cars, sp2 = speed^2, sp3 = speed^3)
pairs(cars)
cor(cars)  

fit2<- lm( dist ~ sp2, cars)
summary(fit2) # интерпретация коэффициентов, значимость регрессии, R^2

data(swiss)
head(swiss) # данные о населении нескольких франкоговорящих провинций Швейцарии (рождаемость, % населения в с.х, и т.д.) 
fit3 <- lm( Fertility ~ ., swiss)
summary(fit3)

pairs(swiss)
cor(swiss)
cor.test(~ Fertility + Examination, swiss)
?vif
vif(fit3)

fit4 <- lm( Fertility ~ ., select(swiss, - Examination))
summary(fit4)

###### Лекция 3. ########################################
###### Линейность взаимосвязи ######################
ggplot(mtcars, aes(x = hp, y = mpg))+
geom_point()

ggplot(mtcars, aes(x = hp^0.5, y = mpg))+
  geom_point()

ggplot(mtcars, aes(x = hp^-0.5, y = mpg))+
  geom_point()

ggplot(mtcars, aes(x = -hp^-0.5, y = mpg))+
  geom_point()

#### Изменение модуля корреляции ~ степень преобразования ##################
test_data<-cbind(mtcars$mpg, mtcars$hp)
l1<-seq(-5,-0.1, by =0.1)
l2<-seq(0.1,5.0, by =0.1)
c1<-numeric(50); c2<-numeric(50)
for (i in 1:length(l1)) c1[i]<-abs(cor(test_data[,1], -test_data[,2]^l1[i]))
for (i in 1:length(l2)) c2[i]<-abs(cor(test_data[,1], test_data[,2]^l2[i]))
c3<-abs(cor(test_data[,1], log(test_data[,2])))

plot(c(l1,0,l2), c(c1,c3,c2))
abline(v=l1[which.max(c(c1,c3,c2))], col = 3)
abline(h=c1[which.max(c(c1,c3,c2))], col = 3)

abline(v=1, col = 2, lty = 'dotted')
abline(h=c2[which(l2 == 1)], col = 2, lty = 'dotted')

which(l2 == 1)
which.max(c(c1,c3,c2))
c1[44] # max corr
l1[44] # степень оптимальная


### интерпретация ? #######################

fit <- lm(mpg ~ hp, mtcars)
summary(fit)

plot(fit3$residuals)
abline(h=0, col='red')
hist(fit3$residuals)
shapiro.test(fit1$residuals)

fit1 <- lm(mpg ~ I(-hp^-0.7), mtcars)
summary(fit1)

### Логарифмическая трансформация #######################
ggplot(mtcars, aes(x = log(hp), y = log(mpg)))+
  geom_point()

fit3 <- lm(log(mpg) ~ log(hp), mtcars)
summary(fit3)
shapiro.test(fit3$residuals)

log(1.01^seq(-5,5, by = 1))

###### Лекция 4. ########################################
####### Тесты на гетероскедастичность ###############
library(sandwich) # оценка Var для гетероскедастичности
library(lmtest) # тест Бреуша-Пагана

fn <- "data/flats_moscow.txt" # code - код района
(h <- read.table(fn, header=TRUE))

plot(h$totsp,h$price)

m1 <- lm(price~totsp,data=h)
summary(m1)
confint(m1) # д.и. коэффициентов регрессии
vcov(m1) # ковариационная матрица коэффициентов регрессии - по OLS
vcovHC(m1) #  состоятельная оценка ковариационной матрицы коэффициентов регрессии \\ подробнее в описании пакета sandwich. Наиболее актуальная форма “HC3”
coeftest(m1,vcov=vcovHC(m1)) # проверка гипотез с правильными оценками ошибок

m1.resid <- resid(m1) # получаем остатки регрессии
plot(h$totsp,m1.resid)
abline(0, 0, col=4) 

bptest(m1) # тест Бреуша-Пагана // H0 - гомоскедастичность
### Тест Голдфельда-Квандта в R #####
(h2 <- h[order(h$totsp),]) # сменим порядок строк в таблице h
m2 <- lm(price~totsp,data=h2) 
gqtest(m2,fraction=0.25) # проведем GQ тест выкинув посередине 25% наблюдений

#### Тест Уайта. White-test - частный случай теста Бреуша-Пагана (в его вспомогательной регресии любые объясняющие переменные)
# тест Уайта - исходные регрессоры, их квадраты и попарные произведения. 
# в R тест Бреуша-Пагана по умолчанию (без указания спецификации) берёт просто все регрессоры исходной модели.

bptest(m1, ~ totsp + I(totsp^2), data = h) # White's test

# взвешенная регрессия
model<- lm(price~totsp, weights=I(1/totsp), data=h) # веса обратно пропорциональны переменной
summary(model)
bptest(model, data = h) # тест Бреуша-Пагана
bptest(model, ~ totsp + I(totsp^2), data = h) # White's test

###### Лекция 5. ########################################
###    Тесты на автокорреляцию остатков из библиотеки lmtest ###############

data(mtcars) # Данные из американского журнала Motor Trend включают расход топлива
#и 10 аспектов дизайна и характеристик 32 автомобилей (модели 1973–74 годов).
mtcars
mod_car <- lm(mpg ~ disp+wt, data=mtcars) # зависимость расхода (Miles/(US) gallon) от объема двигателя и веса автомобиля
plot(mod_car$residuals, type="l")
abline(0, 0, col=4)

plot(mod_car$residuals[1:31], mod_car$residuals[2:32])
abline(0, 0, col=4)
abline(v=0, col=4)
mod_resid <- lm(mod_car$residuals[2:31]~ -1+mod_car$residuals[1:30]) # зависимость остатков e(t) от e(t-1)
summary(mod_resid)
abline(mod_resid, col="blue")

dwtest(mod_car) # Durbin-Watson test
bgtest(mod_car, order = 2) 
bgtest(mod_car, order = 1)

bg1<-bgtest(mod_car, order = 1) # тест Бреуша-Годфри
coeftest(bg1)
bg2<-bgtest(mod_car, order = 2) # тест Бреуша-Годфри
coeftest(bg2)

y.rt<-read.table("clipboard", dec=",", h=T) # Урожайность от осадков и температуры 
y.rt[,3:4]
m.y <- lm(урожай ~ температура+осадки, y.rt) # зависимость урожая от температуры и осадков
summary(m.y)
plot(m.y$residuals, type="l")
abline(0, 0, col=4)

plot(m.y$residuals[1:9], m.y$residuals[2:10])
abline(0, 0, col=4)
abline(v=0, col=4)
y_resid <- lm(m.y$residuals[2:10]~ -1+m.y$residuals[1:9]) # зависимость остатков e(t) от e(t-1)
summary(y_resid)
abline(y_resid, col="blue")

dwtest(m.y) # Durbin-Watson test
bgtest(m.y, order = 2) 
bgtest(m.y, order = 1)

bg2<-bgtest(m.y, order = 1) # тест Бреуша-Годфри
coeftest(bg2)
coeftest(m.y, vcov.=NeweyWest(m.y, lag=1, prewhite = F)) # гетероскедастичность и автокорреляция NeweyWest



t.Vl<-read.table("clipboard", dec=",", h=T) # средние температуры Владивостока в январе и феврале

m.Vl <- lm(t.Vl[,2] ~ t.Vl[,1]) # зависимость температуры в феврале от т. января
summary(m.Vl)
plot(m.Vl$residuals, type="l")
abline(0, 0, col=4)

plot(m.Vl$residuals[1:151], m.Vl$residuals[2:152])
abline(0, 0, col=4)
abline(v=0, col=4)
Vl_resid <- lm(m.Vl$residuals[2:152]~ -1+m.Vl$residuals[1:151]) # зависимость остатков e(t) от e(t-1)
summary(Vl_resid)
abline(Vl_resid, col="blue")

dwtest(m.Vl) # Durbin-Watson test
bgtest(m.Vl, order = 2) 
bgtest(m.Vl, order = 1)

(bg4<-bgtest(m.Vl, order = 4)) # тест Бреуша-Годфри
coeftest(bg4)

###### Лекция 6 ########################################
library (strucchange) # тест Чоу
library(car) # VIF
library(ggplot2)
library(sandwich) # оценка Var для гетероскедастичности
library(lmtest) # тест Бреуша-Пагана
####### Тест Чоу ###############
dt <- data.frame(x = c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8, 8, 9, 10, 10,
                       11, 12, 12, 13, 14, 15, 15, 16, 17, 18, 18, 19, 20, 20),
                 y = c(3, 5, 6, 10, 13, 15, 17, 14, 20, 23, 25, 27, 30, 30, 31,
                       33, 32, 32, 30, 32, 34, 34, 37, 35, 34, 36, 34, 37, 38, 36))
plot(dt)
sctest(dt$y ~ dt$x, type = "Chow", point = 10)

######### фиктивные переменные #########################
d.w<-read.table("clipboard", dec=",", h=T) # зарплата ~ пол, возраст, уровень образования

ggplot(d.w, aes(AGE, W, col = factor(EDU))) +
  geom_point(aes(pch = factor(EDU))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap(~ factor(SEX))

ggplot(d.w, aes(AGE, W, col = factor(SEX))) +
  geom_point(aes(pch = factor(SEX))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap( ~ factor(EDU))

d.w$SEX<-factor(d.w$SEX)
d.w$EDU<-factor(d.w$EDU)
m <- lm(W~., data = d.w)
summary(m)
vif(m)

m.resid <- resid(m) # получаем остатки регрессии
plot(d.w$EDU,m.resid)
abline(0, 0, col=4) 
plot(d.w$AGE,m.resid)
abline(0, 0, col=4) 
plot(d.w$SEX,m.resid)
abline(0, 0, col=4) 
bptest(m,studentize=FALSE) # тест Бреуша-Пагана

plot(m$residuals[1:118], m$residuals[2:119])
abline(0, 0, col=4)
abline(v=0, col=4)
mod_resid <- lm(m$residuals[2:119]~ -1+m$residuals[1:118]) # зависимость остатков e(t) от e(t-1)
summary(mod_resid)
abline(mod_resid, col="blue")

dwtest(m) # Durbin-Watson test
coeftest(m,vcov=vcovHC(m)) # Применяя правильную Var??HC(??^) (из пакета sandwich: HC3 which is thus the default), проверим гипотезы.


# квартиры: влияние качественных переменных
fn <- "data/flats_moscow.txt" # code - код района
(h <- read.table(fn, header=TRUE))
table(h$code)
p.msk<-filter(h,code == 7)

ggplot(p.msk, aes(totsp, price, col = factor(brick))) +
  geom_point(aes(pch = factor(brick))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap(~ factor(brick))

ggplot(p.msk, aes(totsp, price, col = factor(brick))) +
  geom_point(aes(pch = factor(brick))) +
  geom_smooth(method = 'lm', col = 'black')+
  facet_wrap(~ factor(dist))

plot(p.msk$brick,p.msk$price)
m1 <- lm(price~totsp+dist+brick, data = p.msk)
summary(m1)
confint(m1) # д.и. коэффициентов регрессии
vcov(m1) # оценка Var(betta)
m1.resid <- resid(m1) # получаем остатки регрессии
plot(p.msk$totsp,m1.resid)
abline(0, 0, col=4) 
plot(p.msk$dist,m1.resid)
abline(0, 0, col=4) 
plot(p.msk$brick,m1.resid)
abline(0, 0, col=4) 

bptest(m1,studentize=FALSE) # тест Бреуша-Пагана
bptest(m1, ~ totsp + brick + dist + totsp^2 + brick^2 + dist^2 + 
         totsp*brick + totsp*dist + brick*dist, data = p.msk) # White's test
bgtest(m1, order = 4)


coeftest(m1,vcov=vcovHC(m1)) # Применяя правильную Var??HC(??^) (из пакета sandwich: HC3 which is thus the default), проверим гипотезы.
vif(m1)

coeftest(m1, vcov.=NeweyWest(m1, lag=5, prewhite = F)) # гетероскедастичность и автокорреляция NeweyWest

# выбор оптимальной модели
pp<-p.msk[,-c(1,11)]
m2 <- lm(price~., data = pp)
summary(m2)
vif(m2)

optimal_fit <-  step(m2, direction = 'backward')
summary(optimal_fit)

###### Лекция 7 ########################################
library(lmtest) #  тест Бреуша-Годфри
#### Прогнозирование ##########
### 1a) температуры Владивостока в сентябре
t.Vl<-read.table("clipboard", dec=",") # температуры Владивостока в сентябре

m.Vl <- lm(t.Vl[,2] ~ t.Vl[,1]) # зависимость температуры от дня
summary(m.Vl)
sum(m.Vl$residuals^2)
sum(m.Vl$residuals^2)/28 # s^2
sqrt(sum(m.Vl$residuals^2)/28) # s

x.av<-mean(t.Vl[,1])
(delta.31<-sqrt(1+1/30+(31-x.av)^2/sum((x.av-t.Vl[,1])^2))*2.293)


(X<-cbind(rep(1,30),t.Vl[,1])) # матрица Х
(X1<-t(X)%*%X) # X'X
(X2<-solve(X1)) # обратная матрица для X'X 
X2%*%t(X)%*%t.Vl[,2] # вектор коэффициентов

X.new<-c(1,31)
sqrt((1+t(X.new)%*%X2%*%X.new))*2.293
m.Vl$coefficients%*%X.new # прогноз
17.23747-delta.31*qt(.975, df = 28)
17.23747+delta.31*qt(.975, df = 28)

x <- t.Vl[,1]
y <- t.Vl[,2]
relation <- lm(y~x)
a <- data.frame(x = 31)
result1 <-  predict(relation, a, interval = "prediction")
print(result1)

a <- data.frame(x = c(31:35))
(result1 <-  predict(relation,a, interval = "prediction"))

# 1b) Ретропрогноз сентябрьских температур во Владивостоке 
m.Vl <- lm(t.Vl[3:30,2] ~ t.Vl[3:30,1]) # зависимость температуры от дня
summary(m.Vl)

x <- t.Vl[3:30,1]
y <- t.Vl[3:30,2]
relation <- lm(y~x)
a <- data.frame(x = c(1:2, 31:32))
result1 <-  predict(relation, a, interval = "prediction")
cbind(result1,c(t.Vl[1:2,2], 19, 16)) # данные за 1 и 2 окт 2013 взяты из архивов pogoda.mail.ru

### 2a) Прогноз урожайности от осадков и температуры || устранение автокорреляции остатков
y.rt<-read.table("clipboard", dec=",", h=T) # Урожайность от осадков и температуры 
m.y <- lm(урожай ~ температура+осадки, y.rt) # зависимость урожая от температуры и осадков
summary(m.y)

dwtest(m.y) # Durbin-Watson test
bgtest(m.y, order = 1)

bg2<-bgtest(m.y, order = 1) # тест Бреуша-Годфри
coeftest(bg2)

x1 <- y.rt[,2]
x2<- y.rt[,3]
y <- y.rt[,4]
relation <- lm(y~x1+x2)
a <- data.frame(x1 = c(35,28), x2 = c(25,26))
result1 <-  predict(relation,a, interval = "prediction")
print(result1)
###   fit      lwr      upr
###1 68.91805 65.31940 72.51669
###2 64.03530 60.33682 67.73378

r<- 0.899 # из регрессии теста Беруша-Годфри
(y.r<-y.rt[2:11,4]-r*y.rt[1:10,4])
x1.r <- y.rt[2:11,2]-r*y.rt[1:10,2]
x2.r <- y.rt[2:11,3]-r*y.rt[1:10,3]
m.yr<-lm(y.r~x1.r+x2.r)
summary(m.yr)
dwtest(m.yr) # Durbin-Watson test

###### Лекция 8 ########################################
### Логистическая регрессия (биномиальная) ##############
load("students.rd") # Исследователь ищет, как переменные 
# gre (Graduate Record Exam scores — оценки во время обучения в вузе), 
# gpa (grade point average — средний балл) 
# rank -  престиж вуза (от 1 до 4; 1 - самый высокий престиж, а у вузов с рангом 4 — самый низкий.)
# влияют на поступление в аспирантуру. 
# Искомая переменная, поступил или не поступил (admit), является бинарной.

mydata<-students 
summary(mydata)
mydata$rank <- factor(mydata$rank) # превратить категориальную переменную в фактор
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

confint(mylogit) # доверительные интервалы рассчитанных коэффициентов
exp(coef(mylogit)) # Переход от логитов к шансам
exp(cbind(OR = coef(mylogit), confint(mylogit))) # шансы с доверительными интервалами

#Расчёт предсказанных вероятностей поступления для каждого значения престижности вуза (переменная rank), удерживая переменные gre и gpa на уровне их средних:
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
#Тест отношения правдоподобия («Likelihood ratio test»)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



