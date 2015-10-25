## Q1
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes 
# number of cylinders as a factor variable and weight as confounder. 
# Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.

data(mtcars)
summary(lm(mpg ~ as.factor(cyl) + wt , data = mtcars))$coefficients[3,1]

lm(mpg ~ as.factor(cyl) + wt , data = mtcars)$coefficients[3]

## Q2
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes 
# number of cylinders as a factor variable and weight as a possible confounding variable. 
# Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. 
# Here, adjusted means including the weight variable as a term in the regression model and 
# unadjusted means the model without weight included. What can be said about the effect 
# comparing 8 and 4 cylinders after looking at models with and without weight included?.

lm(mpg ~ as.factor(cyl) + wt , data = mtcars)$coefficients[3] # holding wt constant
lm(mpg ~ as.factor(cyl) , data = mtcars)$coefficients[3] # disregarding wt

## Q3
# Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders 
# as a factor variable and weight as confounder. Now fit a second model with mpg as the outcome model 
# that considers the interaction between number of cylinders (as a factor variable) and weight. 
# Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 
# 0.05 as a type I error rate significance benchmark.

anova(
  lm(mpg ~ as.factor(cyl) + wt , data = mtcars),
  lm(mpg ~ as.factor(cyl) + wt + as.factor(cyl) * wt, data = mtcars)
)$`Pr(>F)`[2]

## Q4
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders 
# as a factor variable and weight inlcuded in the model as
# lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# How is the wt coefficient interpretted?

# 1 ton = 2240 lbs. 1 short (or US) ton = 2000 lbs.

# The estimated expected change in MPG per one ton increase in weight for for a specific number of cylinders (4, 6, 8).

## Q5
# Consider the following data set
# x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
# y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

max(hatvalues(lm(y~x)))

## Q6
# Consider the following data set
# x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
# y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y~x)
max.pos <- which.max(hatvalues(fit))
dfbetas(fit)[max.pos,2]