## Q1
# Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
n <- length(x)

fit <- lm(y ~ x)

sigma <- sqrt(sum(resid(fit)^2) / (n - 2)) # sd around the regression line; residual sd
ssx <- sum((x - mean(x))^2)
slope.standard.error <- sigma / sqrt(ssx)
slope <- fit$coefficients[2]
t.slope <- slope / slope.standard.error

2 * pt(abs(t.slope - 0), df = n-2, lower.tail = F) 

summary(fit)$coefficients['x','Pr(>|t|)'] # same answer
anova(fit)['x','Pr(>F)'] # same answer

## Q2 
# what's the residual sd?

sigma

r <- resid(fit)
sqrt(sum((r - mean(r))^2)/(length(r)-2)) # same answer; mean = 0

summary(fit)$sigma # same result

## Q3
# mtcars data set, lm: weight (predictor), mpg (outcome). 
# lower endpoint of a 95% confidence interval for the expected mpg at the average weight

fit <- lm(mpg ~ wt, data = mtcars)
predict(fit, newdata = data.frame(wt = mean(mtcars$wt)), interval = 'confidence')[2]

## Q5
# Consider again the mtcars data set and a linear regression model with mpg 
# as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. 
# Construct a 95% prediction interval for its mpg. What is the upper endpoint?

predict(fit, newdata = data.frame(wt = 3000 / 1000), interval = 'prediction')[3]

## Q6
# Consider again the mtcars data set and a linear regression model with mpg 
# as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. 
# Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight.
# Give the lower endpoint.

fit <- lm(mpg ~ I(wt/2), data = mtcars)

cf <- summary(fit)$coefficients[2,]
cf[1] - qt(0.975, df = fit$df) * cf[2]

## Q7
# If my X from a linear regression is measured in centimeters and I convert it to meters 
# what would happen to the slope coefficient?

lm(y ~ x)$coefficients[2]; lm(y ~ I(x / 100))$coefficients[2]

## Q8
# I have an outcome, Y, and a predictor, X and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1. What would be the consequence to the subsequent slope and intercept if I were to refit the model with a new regressor, X+c for some constant, c?

lm(y ~ x)$coefficients; lm(y ~ I(x + 1))$coefficients

## Q9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor.
# About what is the ratio of the the sum of the squared errors, ∑ni=1(Yi−Y^i)2 when comparing
# a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?

sum(lm(mpg ~ wt, data = mtcars)$residuals ^ 2) / sum(lm(mpg ~ 1, data = mtcars)$residuals ^ 2)

## Q10
# Do the residuals always have to sum to 0 in linear regression?

sum(lm(mpg ~ wt, data = mtcars)$residuals); sum(lm(mpg ~ wt-1, data = mtcars)$residuals)

  