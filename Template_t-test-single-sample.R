# Read the population data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

n <- 50
sampleTaxes <- sample(pop$taxes, size = 50, replace = TRUE)

# A) Test the hypothesis H_0: mu = 1000 vs the alternative H_1: mu != 1000

## 1A) Compute the test statistic using sampleTaxes


## 2A) Compute the critical values for a 5% error probability
## from the distribution of the test statistic 
## under the null hypothesis

criticalLower <- 
criticalUpper <- 


## 3A) Compare the test statistic with the critical values
## and decide if you reject the null hypothesis

## Calculate the p-value of the test


# B) Test the null hypothesis H_0: mu = 1000 vs the alternative H_1: mu > 1000

## 1B) Compute the test statistic

## 2B) Compute the critical value at a 5% error probability

## 3B) Compute the p-value of the test
