#install.packages("HoRM") Функция Хилдрета-Лу
library('HoRM')
library('lmtest')

fn <- "D:/Study/Матметоды/ПР5/data/flats_moscow.txt"
(h <- read.table(fn,header=TRUE))
h
h <- subset(h, select = - c(walk, brick, floor, code, n, totsp, livesp))
h

model <- lm(price ~  ., data=h)

plot(model$residuals, type="l")
abline(0, 0, col=4)
#ну остатки какие-то

plot(model$residuals[1:length(h$price)-1], model$residuals[2:length(h$price)])
abline(0, 0, col=4)
abline(v=0, col=4)
mod_resid <- lm(model$residuals[2:length(h$price)]~ -1+model$residuals[1:length(h$price)-1]) # зависимость остатков e(t) от e(t-1)
summary(mod_resid)
abline(mod_resid, col="blue")
#как будто бы немножко есть автокорреляция

dwtest(model) # Durbin-Watson test
#DW < du => положительная кореляция, отвергаем H0 => автокорреляция есть
bgtest(model, order = 2) #=> не отвергаем отсутствие автокор.
bgtest(model, order = 1) #автокорреляция есть

#https://rpubs.com/apricitea/handling-autocorrelation
r <- c(seq(-1,0.8, by= 0.1), seq(0.9,0.99, by= 0.01))
tab <- data.frame("rho" = r, "SSE" = sapply(r, function(i){deviance(hildreth.lu.func(i, model))}))
optrho <- which.min(round(tab, 4)[,2])
round(tab, 4)[optrho,]
#plot(tab)

model2 <- lm(price ~  dist, data=h)
tab <- data.frame("rho" = r, "SSE" = sapply(r, function(i){deviance(hildreth.lu.func(i, model2))}))
optrho <- which.min(round(tab, 4)[,2])
round(tab, 4)[optrho,]
#Returns the linear regression fit for a given level of rho using the Hildreth-Lu procedure
out.1 <- hildreth.lu(h$price, h$dist, 0)
out.1     
