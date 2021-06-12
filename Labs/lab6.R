install.packages("gam")
library(ISLR)
library(leaps)
library(gam)
set.seed(100)
train <- sample(1: nrow(College), nrow(College)*0.7)
test <- -train
fit <- regsubsets(Outstate ~ ., data = College, subset = train, method = 'forward')
fit.summary <- summary(fit)
fit.summary

coef(fit, id = 6)

gam.mod <- gam(Outstate ~ Private + s(Room.Board, 5) + s(Terminal, 5) + s(perc.alumni, 5) + s(Expend, 5) + s(Grad.Rate, 5), data = College, subset = train)
par(mfrow = c(2,3))
plot(gam.mod, se = TRUE, col = 'blue')

preds <- predict(gam.mod, College[test, ])
RSS <- sum((College[test, ]$Outstate - preds)^2) # based on equation (3.16)
TSS <- sum((College[test, ]$Outstate - mean(College[test, ]$Outstate)) ^ 2)
1 - (RSS / TSS)

summary(gam.mod)
