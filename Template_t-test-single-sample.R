library(dplyr)

# Read the population data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

n <- 50
sampleTaxes <- sample(pop$taxes, size = 50, replace = TRUE)

# A) Test the hypothesis H_0: mu = 1000 vs the two-sided alternative H_1: mu != 1000

muH0 <- 1000

## 1A) Compute the test statistic using sampleTaxes

testStatistic <- sqrt(n) * (mean(sampleTaxes) - muH0)/sd(sampleTaxes)

## 2A) Compute the critical values for a 5% error probability
## from the distribution of the test statistic 
## under the null hypothesis

criticalLower <- - qt(0.975, df = n - 1)
criticalUpper <- qt(0.975, df = n - 1)
  

## 3A) Compare the test statistic with the critical values
## and decide if you reject the null hypothesis

abs(testStatistic) > criticalUpper

## 4A) Calculate the p-value of the test
2 *(1 - pt(abs(testStatistic), df = n - 1))
  
## 5A) Perform the test using the function t.test
t.test(sampleTaxes, mu = muH0)

  
# B) Test the null hypothesis H_0: mu = 1000 vs the alternative H_1: mu > 1000

## 1B) Compute the test statistic

## The test statistic here is the same as in 1A).

## 2B) Compute the critical value at a 5% error probability

qt(0.95, df = n - 1)

## 3B) Compute the p-value of the test

1 - pt(testStatistic, df = n - 1)

## 4B) Compare the test statistic with the critical values and decide if you
## reject H_0.

testStatistic > qt(0.95, df = n - 1)

## 5B) Perform the test using the function t.test

t.test(sampleTaxes, mu = muH0, alternative = 'greater')

## C) Examine the behaviour of the (two.sided) test in a simulation

R <- 1000
n <- 50

samples <- sample(pop$taxes, size = R * n, replace = TRUE)
samplesData <- data.frame(x = samples, sampleIndex = rep(1:R, each = n))

rejectH0 <- function(x) {
  testResult <- t.test(x, mu = muH0, alternative = 'two.sided')
  return(testResult$p.value < 0.05)
}

samplesSummary <- 
  samplesData %>% 
  group_by(sampleIndex) %>%
  summarise(rejectedH0 = rejectH0(x))

table(samplesSummary$rejectedH0)



