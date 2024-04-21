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
