# install.packages(c('dplyr', 'ggplot2'))


library(ggplot2)
library(dplyr)

pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')


## Draw a sample of persons' taxes with replacement (n = 50)

sampleTaxes <- sample(pop$taxes, size = ???, replace = TRUE)

## Compute a 95\% normal CI for the sample in sampleTaxes

## Compute a 95\% t CI for the sample in sampleTaxes


## Compute a 95\% t-confidence interval for the population mean

lowerCIlimit <- 
upperCIlimit <- 


## Select 1000 samples of size 50 with replacement from the taxes column in pop.

R <- 1000
n <- 

samples <- sample(???, size = ???, replace = TRUE)

## create a data frame with the sample values and a sample index
samplesData <- data.frame(x = , sampleIndex = )

groupedSamplesData <- group_by(samplesData, ???)
samplesSummary <- summarise(???,
   sampleMean = ???,
   sampleStdDev = ???,
)

## Compute 95\% t-confidence interval for each sample
## Compute 95\% normal confidence intervals for each sample
## Compute an logical variable that is TRUE if the CI contains
## the populaiton mean and FALSE if not.

samplesSummary <- within(samplesSummary, {
  ## Lower normal CI limit
  
  ## Upper normal CI limit
  
  ## Indicator 
  
  ## Lower t CI limit
  
  ## Upper t CI limit
  
})

## Compute the average length of the normal CI intervals in the simulation.

## Compute the average length of the t CI intervals in the simulation.

## Visualise the first 20 confidence intervals

ggplot(data = ???, aes(x = ???)) +  # Select the data 
  ## Draw the sample means as points
  geom_point(aes(y = ???)) +
  # Draw a horizontal line at the position of the population mean
  geom_hline(yintercept = ???) +
  ## Draw error bars to represent the confidence intervals  
  geom_errorbar(aes(ymin = ???, ymax = ???)) +


