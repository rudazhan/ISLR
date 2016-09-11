library(ISLR)
library(boot)

scatter.smooth(Auto$horsepower, Auto$mpg)

# LOOCV (leave-one-out cross validation)
glm_fit <- glm(mpg ~ horsepower, data = Auto)
loo <- cv.glm(Auto, glm_fit)
loo$delta
## Faster loocv prediction error.
loocv <- function(fit) {
  h <- lm.influence(fit)$hat
  mean((residuals(fit) / (1-h))^2)
}
loocv_poly <- function(d) {
  loocv(glm(mpg ~ poly(horsepower, d), data = Auto))
}
degrees <- 1:5
cv_error <- vapply(degrees, loocv_poly, double(1))
plot(degrees, cv_error, type = 'b')

# K-fold CV
cv_ploy <- function(d) {
  poly_fit <- glm(mpg ~ poly(horsepower, d), data = Auto)
  cv.glm(Auto, poly_fit, K = 10)$delta[[1]]
}
lines(degrees, vapply(degrees, cv_ploy, double(1)), type = 'b', col = 'red')

# Bootstrap
alpha <- function(x, y) {
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x, y)
  (vy - cxy) / (vx + vy - 2*cxy)
}
alpha_fn <- function(data, index) {
  with(data[index, ], alpha(X, Y))
}
set.seed(1)
boot_out <- boot(Portfolio, alpha_fn, R = 1000)
plot(boot_out)
