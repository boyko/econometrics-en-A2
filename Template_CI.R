# install.packages(c('dplyr', 'ggplot2'))


library(ggplot2)
library(dplyr)

pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')


## Draw a sample of persons' taxes with replacement (n = 50)

sampleTaxes <- sample(pop$taxes, size = 50, replace = TRUE)

## Compute a 95\% normal CI for the population mean with the sample in sampleTaxes

lowerCIlimit <- mean(sampleTaxes) - sqrt(var(pop$taxes)) * qnorm(0.975) / sqrt(50)
upperCIlimit <- mean(sampleTaxes) + sqrt(var(pop$taxes)) * qnorm(0.975) / sqrt(50)

## Compute a 95\% t CI for the population mean with the sample in sampleTaxes

lowerTCIlimit <- mean(sampleTaxes) - sqrt(var(sampleTaxes)) * qt(0.975, df = 50 - 1) / sqrt(50)
upperTCIlimit <- mean(sampleTaxes) + sqrt(var(sampleTaxes)) * qt(0.975, df = 50 - 1) / sqrt(50)

## Select 1000 samples of size 50 with replacement from the taxes column in pop.

R <- 1000
n <- 50

samples <- sample(pop$taxes, size = R * n, replace = TRUE)

## create a data frame with the sample values and a sample index
samplesData <- data.frame(x = samples, sampleIndex = rep(1:R, each = n))

groupedSamplesData <- group_by(samplesData, sampleIndex)
samplesSummary <- summarise(groupedSamplesData,
   sampleMean = mean(x),
   sampleStdDev = sqrt(var(x))
)

## Compute 95\% t-confidence interval for each sample
## Compute 95\% normal confidence intervals for each sample
## Compute an logical variable that is TRUE if the CI contains
## the populaiton mean and FALSE if not.

samplesSummary <- within(samplesSummary, {
  ## Lower normal CI limit
  normalCIlower <- sampleMean - sqrt(var(pop$taxes))*qnorm(0.975) / sqrt(n)
  ## Upper normal CI limit
  normalCIupper <- sampleMean + sqrt(var(pop$taxes))*qnorm(0.975) / sqrt(n)
  ## Indicator 
  
  ## Lower t CI limit
  tCIlower <- sampleMean - sampleStdDev * qt(0.975, df = n - 1) / sqrt(n)
  ## Upper t CI limit
  tCIupper <- sampleMean + sampleStdDev * qt(0.975, df = n - 1) / sqrt(n)
   
  inCI <- mean(pop$taxes) < tCIupper & mean(pop$taxes) > tCIlower
})

table(samplesSummary$inCI)

## Examine the distributional assumption of the t confidence intervals
## using a qq-plot

ggplot(data = data.frame(y  = sampleTaxes)) + stat_qq(aes(sample = y))

## Visualise the first 20 confidence intervals

ggplot(data = samplesSummary, aes(x = sampleIndex, y = sampleMean)) +  # Select the data 
  ## Draw the sample means as points
  geom_point() +
  # Draw a horizontal line at the position of the population mean
  geom_hline(yintercept = mean(pop$taxes)) +
  ## Draw error bars to represent the confidence intervals  
  geom_errorbar(aes(ymin = tCIlower, ymax = tCIupper))


