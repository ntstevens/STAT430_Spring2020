## Candy Crush F-test Example

## Change working directory
setwd(dir = "/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2020/R Stuff/")

## Read in the data
candy <- read.csv(file = "candycrush.csv", header = T)

## Plot the data
boxplot(time ~ booster, data = candy, xaxt = "n", ylab = "Length of Game Play (minutes)", xlab = "Condition")
axis(side = 1, at = c(1,2,3), labels = c("Lollipop Hammer", "Jelly Fish", "Color Bomb"))

## Perform the test by fitting a linear regression model
model <- lm(time ~ factor(booster), data = candy)
summary(model)
anova(model)

## Check model assumptions 
par(mfrow=c(1,3))
hist(model$residuals, xlab = "Residials", main = "Histogram of Residuals")
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
plot(x = model$fitted.values, y = model$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")