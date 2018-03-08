##############################
## Problem set 2, Problem 1 ##
##############################

# install.packages(c('ggplot2', 'dplyr'))
library(ggplot2)
library(dplyr)

# a)
# Simulate 100 draws from a normal distribution with parameters
# mu = 10, sigma^2 = 4
# rnorm accepts a parameter sd (standard deviation, i.e. the square root of the variance).
# To draw from N(10, 4) we call rnorm(n, mean = 10, sd = 2) and NOT rnorm(n, mean = 10, sd = 4)


x <- rnorm(n = 20, mean = 10, sd = 2)
ggplot(data = data.frame(x), aes(x = x)) + geom_histogram()


# b)

x <- rnorm(n = 10000, mean = 10, sd = 2)
ggplot(data = data.frame(x), aes(x = x)) + geom_histogram()

# c)

n <- 10000
R <- 1000

x <- rnorm(n * R, mean = 10, sd = 2)

simData <- data.frame(x = x, r = rep(1:R, each = n))
# simData

groupedBySimIndex <- group_by(simData, r)
groupedBySimIndex
estimatedMeans <- summarise(groupedBySimIndex, estMean = mean(x))
estimatedMeans

summary(estimatedMeans$estMean)

ggplot(data = estimatedMeans, aes(x = estMean)) + geom_histogram() + xlim(c(9, 11))
