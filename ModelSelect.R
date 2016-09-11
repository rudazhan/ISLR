library(ISLR)
summary(Hitters)
sapply(Hitters, purrr::compose(sum, is.na))
Hitters <- na.omit(Hitters)

# Best subset selection
library(leaps)
p <- ncol(Hitters) - 1
subset_all <- regsubsets(Salary ~ ., data = Hitters, nvmax = p)
fit_subsets <- summary(subset_all)
## R-squared and adjusted R-squared
plot(fit_subsets$rsq, type = 'b', col = 2, ylab = NA)
lines(fit_subsets$adjr2, type = 'b', col = 4)
abline(v = 10, col = 'gray50')
legend('topleft', legend = c('R-squared', 'Adjusted R-squared'),
       lty = 1, col = c(2, 4), bty = 'n')
## Marlow's Cp (AIC) and BIC
plot(fit_subsets$cp, type = 'b', col = 'red', ylab = NA)
par(new = TRUE)
plot(fit_subsets$bic, type = 'b', col = 'blue', axes = NULL)
axis(side = 4)
legend('topleft', inset = .05, legend = c('Cp', 'BIC'), col = c('red', 'blue'),
       lty = 1, bty = 'n')
## Plot method for class 'regsubsets'
plot(subset_all, scale = 'Cp')
coef(subset_all, 10)

# Forward selection
subset_fwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = p, method = 'forward')
summary(subset_fwd)
plot(subset_fwd, scale = 'Cp')

# Validation
set.seed(1)
n <- nrow(Hitters)
n_train <- round(2/3 * n, digits = -1)
train <- sample(seq(n), n_train, replace = FALSE)
Hitters_val <- Hitters[-train, ]
subset_fwd <- regsubsets(Salary ~ ., data = Hitters[train, ],
                         nvmax = p, method = 'forward')
predict.regsubsets <- function(subsets, id, data, formula = NULL) {
  if (is.null(formula)) {
    tryCatch(formula <- as.formula(subsets$call[[2]]),
             error = function(e) stop(simpleError('no full model formula'))
             )
  }
  design <- model.matrix(formula, data)
  coef_id <- coef(subsets, id = id)
  design[, names(coef_id)] %*% coef_id
}
mse <- function(y, pred) mean((y - pred) ^ 2)
rmse <- purrr::compose(sqrt, mse)
val_error <- purrr::map_dbl(seq(p), ~ mse(Hitters_val$Salary,
                                          predict(subset_fwd, ., Hitters_val)))

# K-fold cross validation
set.seed(11)
cv_regsubsets <- function(data, formula, K) {
  y <- all.vars(formula)[[1]]
  p <- ifelse(all.vars(formula)[[2]] == ".",
              ncol(data) - 1, length(all.vars(formula)) - 1)
  folds <- sample(rep(seq(K), length = nrow(data)))
  val_error <- function(val_fold) {
    train <- data[folds != val_fold, ]
    test <- data[folds == val_fold, ]
    subsets <- leaps::regsubsets(formula, data = train,
                                 nvmax = p, method = 'forward')
    purrr::map_dbl(seq(p), ~mse(test[[y]], predict(subsets, ., test, formula)))
  }
  rowMeans(vapply(seq(K), val_error, double(p)))
}
cv_error <- cv_regsubsets(Hitters, Salary ~ ., K = 10)
## Compare Validation and CV
plot(sqrt(subset_fwd$rss[-1] / n_train), type = 'b', ylab = 'Root MSE')
points(sqrt(val_error), col = 2, type = 'b')
points(sqrt(cv_error), col = 3, type = 'b')
legend('topleft', inset = c(0.05, 0), lty = 1, bty = 'n',
       legend = c('Training', 'Validation', '10-fold CV'), col = 1:3)

# Regularization
library(glmnet)
x <- model.matrix(Salary ~ . + 0, data = Hitters)
y <- Hitters$Salary
## Ridge
fit_ridge <- glmnet(x, y, alpha = 0)
plot(fit_ridge, xvar = 'lambda', label = TRUE)
cv_ridge <- cv.glmnet(x, y, alpha = 0)
plot(cv_ridge)
## Lasso
fit_lasso <- glmnet(x, y)
plot(fit_lasso, xvar = 'lambda', label = TRUE)
plot(fit_lasso, xvar = 'dev', label = TRUE)
cv_lasso <- cv.glmnet(x, y)
plot(cv_lasso)
coef(cv_lasso)
