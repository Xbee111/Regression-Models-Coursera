unknown <- 7

## Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
# what value minimize f(u) = sum(w*(x - u)^2)

sum(x * w) / sum(w) # x0 for the derrivative of f(u) = sum(w*(x - u)^2)

# checking all the answers
ans <- c(0.0025,0.1471,0.300,1.077)
errors <- sapply(ans, function(u) sum(w*(x - u)^2))
ans[which.min(errors)]

## Q2
# calculate the slope of lm through the point (0,0)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

sum(x * y) / sum(x^2) # x0 for derriviate of f(b) = sum((y - x*b)^2)

lm(y ~ x -1)$coeff # same result;  -1 to remove the intercept

## Q3
# weight - predictor, mpg - outcome. whats the slope of lm?
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg

slope <- cor(x,y) * sd(y)/sd(x)
slope

lm(y ~ x)$coeff[2] # same result

## Q4
ysd <- unknown
xsd <- ysd / 2; correlation <- 0.5

slope <- correlation * ysd / xsd
slope

## Q5
correlation <- 0.4; intercept <- 0; x <- 1.5
slope <- correlation

slope * x

## Q6
# normalize vector and give 1st value
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
((x - mean(x))/sd(x))[1]

## Q7
# calculate intercept of lm
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

slope <- cor(x,y) * sd(y)/sd(x)
intercept <- mean(y) - slope * mean(x)
intercept
# lm(y~x)$coeff[1]  # same results

## Q8
xmean <- 0; ymean <- 0
slope <- unknown

intercept <- ymean - slope * xmean
intercept

## Q9
# what value minimizes squared distance from that value
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

## Q10
# what's the: slopexy / slopeyx 

# cor(x,y) = cor(y,x)
# slopexy / slopeyx = (cor(x,y) * sd(y)/sd(x) ) / ( (cor(y,x) * sd(x)/sd(y) ) ) = 
# (sdy / sdx) / ( sdx / sdy ) = (sdy / sdx) * (sdy / sdx) = 
# var(y) / var(x)

