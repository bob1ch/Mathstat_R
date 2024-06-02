####Код с предыдущего задания####
data <-read.table("clipboard", dec=".", h=T)
data <- subset(data, select = - c(date, mean))
summary(data)

mylogit <- glm(osadki ~ max + min, data = data, family = "binomial")
summary(mylogit)



confint(mylogit) # доверительные интервалы рассчитанных коэффициентов
exp(coef(mylogit)) # Переход от логитов к шансам
exp(cbind(OR = coef(mylogit), confint(mylogit))) # шансы с доверительными интервалами

newdata1 <- with(data, data.frame(max = mean(max), min = mean(min)))
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
newdata1
####1####
library(ROCR)
data$prob  <- predict(object = mylogit, type = "response") # предсказанные вероятности в переменной
pred_fit <- prediction(data$prob, data$osadki) 
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc") # здесь 0.693
str(auc)
perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2) # специфичность: предсказали 0, в данных - 0 
plot(add=T, perf4 , col = "green", lwd =2) # чувствительность: предсказали 1, в данных 1 
plot(add=T, perf5, lwd =2) # общая эффективность

legend(x = 0.55,y = 0.65, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

optim<-0.3 # подобрать значение
abline(v= optim, lwd = 2, lty = 'dotted', col = 'black') 


#install.packages('pROC')
#install.packages('ROCR')
library(pROC)

test_prob = predict(mylogit, newdata = data, type = "response")
test_roc = roc(data$osadki ~ test_prob, plot = TRUE, print.auc = TRUE)

data$prob  <- predict(object = mylogit, type = "response")

nrow(subset(data,osadki == 1&prob>=optim))/nrow(subset(data,osadki == 1)) # правильно +
nrow(subset(data,osadki == 0 & prob<optim))/nrow(subset(data,osadki == 0)) # правильно -
ttt<-data$prob[data$osadki==0]
length(ttt[ttt>=optim])/length(ttt)
length(ttt[ttt<optim])/length(ttt)
ttt1<-data$prob[data$osadki==1]
length(ttt1[ttt1>=optim])/length(ttt1)
length(ttt1[ttt1<optim])/length(ttt1)

####2####
require(foreign)
require(nnet)
#install.packages('foreign')
#install.packages('nnet')
croc_data <-read.table("clipboard", dec=".", h=T)

croc_data$FOOD <- factor(croc_data$FOOD)
croc_data

croc_data$FOOD2 <- relevel(croc_data$FOOD, ref = 1)
croc_logit <- multinom(FOOD2 ~ LENGTH, data = croc_data)
summary(croc_logit)


Probs <- fitted(croc_logit)
pred <- apply(Probs, 1, function(x) colnames(Probs)[which(x == max(x))])
(table(Факт = croc_data$FOOD, Прогноз = pred))
Acc <- mean(pred == croc_data$FOOD2)
paste("Точность=", round(100*Acc, 2), "%", sep = "")

z <- summary(croc_logit)$coefficients/summary(croc_logit)$standard.errors
# p-значения на основе теста Вальда 
(1 - pnorm(abs(z), 0, 1))*2

summary(croc_logit)
croc_data
