library(MASS)
?Boston
attach(Boston)
plot(medv ~ lstat)
fit <- list(
  simple = lm(medv ~ lstat),
  multiple = lm(medv ~ lstat + age),
  full = lm(medv ~ ., data = Boston),
  interact = lm(medv ~ lstat * age),
  poly5 = lm(medv ~ poly(lstat, 5)),
  poly6 = lm(medv ~ poly(lstat, 6))
)
# methods for 'lm' objects: reporting, modeling, plotting
summary(fit$simple)
confint(fit$simple)
predict(fit$simple, data.frame(lstat = c(5, 10, 15)), interval = 'confidence')
abline(fit$simple, col = 'red')
op <- par(mfrow = c(2, 2))
plot(fit$full)
par(op)
update(fit$full, ~ . - age - indus)
summary(fit$poly5)
summary(fit$poly6)
plot(medv ~ lstat)
points(lstat, fitted(fit$poly5), col = 'red', pch = 20)
points(lstat, fitted(fit$poly6), col = 'blue', pch = 20)

library(ISLR)
?Carseats
attach(Carseats)
# Qualitative predictors
contrasts(ShelveLoc)
fit_carsets <- lm(Sales ~ . + Income:Advertising + Age:Price, data = Carseats)
summary(fit_carsets)
