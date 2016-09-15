library(ISLR)
attach(Wage)

plot(wage ~ age, pch = 20, col = scales::alpha('black', 0.2))

# (Orthogonal) Polynomial linear regression
# Tests of each coefficients are uncorrelated, thus can be done in one fit.
fit <- lm(wage ~ poly(age, 4))
summary(fit)
complete_age <- seq(min(age), max(age))
preds <- predict(fit, newdata = list(age = complete_age, se.fit = T)
lines(complete_age, preds$fit, col = 'red', lwd = 2)
se_bands <- with(preds, cbind(fit - 2 * se.fit, fit + 2 * se.fit))
matlines(complete_age, se_bands, col = 'red', lty = 2)
# ANOVA test works for general nested sequence of models.
# (In this two-variable model, 4th order term is not statistically significant.)
fit2 <- list(
  lm(wage ~ education),
  lm(wage ~ education + age),
  lm(wage ~ education + poly(age, 2)),
  lm(wage ~ education + poly(age, 3)),
  lm(wage ~ education + poly(age, 4))
)
do.call(anova, fit2)

# Polynomial logistic regression
high_wage <- I(wage > 250)
fit_logit <- glm(high_wage ~ poly(age, 3), family = binomial)
summary(fit_logit)
pred_logit <- predict(fit_logit, list(age = complete_age), se.fit = T)
v3 <- with(pred_logit, fit + cbind(y = 0, lower = -2*se.fit, upper = 2*se.fit))
link_logistic <- function(y) exp(y) / (1 + exp(y))
ylim <- c(0, 0.1)
matplot(complete_age, link_logistic(v3), col = 'green', type = 'l', 
        lwd = c(2, 1, 1), lty = c(1, 2, 2), ylim = ylim)
# rug plot in base R!
points(jitter(age), high_wage * ylim[[2]], pch = '|', cex = 0.5)

# Splines
library(splines)
plot(wage ~ age, pch = 20, col = scales::alpha('black', 0.2))
# B-spline and natural spline.
knots <- c(25, 40, 60)
abline(v= knots, col = 'darkgreen', lty = 2)
pred_spline <- function(type) {
  sp <- get(type, getNamespace('splines'))
  fit <- lm(wage ~ sp(age, knots = knots))
  predict(fit, list(age = complete_age))
}
lines(complete_age, pred_spline('bs'), col = 'darkgreen', lwd = 2)
lines(complete_age, pred_spline('ns'), col = 'darkred', lwd = 2)
# Smoothing spline
# Cross validation automatically chooses the equivalent degree of freedom.
fit_smooth <- smooth.spline(wage ~ age, cv = TRUE)
fit_smooth$df
lines(fit_smooth, col = 'purple', lwd = 2)

# Generalized additive model
library(gam)
op <- par(mfrow = c(1, 3))
fit_gam <- list(
  gam(wage ~ s(age, df = 4) + s(year, df = 4) + education),
  gam(high_wage ~ s(age, df = 4) + year + education, family = binomial),
  gam(high_wage ~ s(age, df = 4) + s(year, df = 4) + education, family = binomial)
)
# Chi-squared ANOVA test
do.call(anova, fit_gam[2:3])
# 'gam' has plot() methods for 'gam', 'lm' and 'glm'.
plot(fit_gam[[1]])
plot(fit_gam[[2]])
plot(fit_gam[[3]])
fit_lm <- lm(wage ~ ns(age, df = 4) + ns(year, df = 4) + education)
plot.gam(fit_lm, se = T)

