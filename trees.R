library(ISLR)
library(tree)
attach(Carseats)
hist(Sales)
summary(Sales)

# Classification tree
Carseats$High <- factor(Sales > 8, levels = c(TRUE, FALSE), 
                        labels = c('Yes', 'No'))
fit_tree <- tree(High ~ . - Sales, data = Carseats)
summary(fit_tree)
class(fit_tree)
plot(fit_tree)
text(fit_tree, pretty = 0)
fit_tree
# Validation
set.seed(1011)
part <- function(train, test) {
  factor(sample(train + test) <= train, 
         levels = c(TRUE, FALSE), labels = c('Train', 'Test'))
}
car_seats <- split(Carseats, part(250, 150))
fit_tree <- tree(High ~ . - Sales, data = car_seats$Train)
plot(fit_tree)
text(fit_tree, pretty = 0)
misclass_table <- function(fitted, newdata) {
  y_name <- all.vars(terms(fitted))[[1]]
  pred <- predict(fitted, newdata, type = 'class')
  table(pred, newdata[[y_name]])
}
misclass_table(fit_tree, car_seats$Test)
# Cross-Validation
cv_carseats <- cv.tree(fit_tree, FUN = prune.misclass)
# The plot generalizes rather badly.
plot(cv_carseats)
fit_pruned <- prune.misclass(fit_tree, best = 5)
plot(fit_pruned)
text(fit_pruned, pretty = 0)
misclass_table(fit_pruned, car_seats$Test)

# Random Forest
library(randomForest)
library(MASS)
set.seed(101)
train <- sample(seq_len(nrow(Boston)), 300)
rf_boston <- randomForest(medv ~ ., data = Boston, subset = train)
# Tuning the size of candidate set of splite variables.
# (RF never overfits with increasing number of trees.)
boston = list(
  train = Boston[train, ],
  test = Boston[-train, ]
)
tune_rf <- function(m) {
  cat(m, '\t')
  ntree <- 400L
  fit <- randomForest(medv ~ ., data = boston$train, mtry = m, ntree = ntree)
  pred <- predict(fit, boston$test)
  c(mtry = m, oob = fit$mse[ntree], test = mean((boston$test$medv - pred) ^ 2))
}
error_mtry <- t(vapply(1:13, tune_rf, double(3)))
matplot(error_mtry[, 1], error_mtry[, 2:3], type = 'b', pch = 19, col = c(2, 4),
        xlab = 'mtry', ylab = 'MSE')
legend('topleft', inset = 0.05, c('Out-of-bag error', 'Test error'), 
       bty = 'n', pch = 19, col = c(2, 4))

# Gradient Boosting
library(gbm)
boost_boston <- gbm(medv ~ ., data = boston$train, distribution = 'gaussian', 
                    n.trees = 1e4, shrinkage = 1e-2, interaction.depth = 4)
summary(boost_boston)
# Partial dependence plots
plot(boost_boston, i = 'lstat')
plot(boost_boston, i = 'rm')
# Effect of number of iteration
n_trees <- seq(100, 1e4, by = 100)
predmat <- predict(boost_boston, newdata = boston$test, n.trees = n_trees)
err_ntrees <- colMeans((predmat - boston$test$medv) ^ 2)
plot(n_trees, err_ntrees)
# GBM usually out-performs RF, given proper parameter tuning.
# (n.trees, shrinkage, interaction.depth)
rf_min_error <- apply(error_mtry, 2, min)
abline(h = rf_min_error[2:3], col = c(2, 4))
text(x = 8000, y = rf_min_error[2:3], adj = c(0, -.1), col = c(2, 4),
    labels = paste('RF', names(rf_min_error[2:3]), 'error'))
