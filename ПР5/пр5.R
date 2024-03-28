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

dwtest(mod_car) # Durbin-Watson test
#DW < du => положительная кореляция, отвергаем H0 => автокорреляция есть
bgtest(mod_car, order = 2) #=> не отвергаем отсутствие автокор.
bgtest(mod_car, order = 1) #автокорреляция есть
