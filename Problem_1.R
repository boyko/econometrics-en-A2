# install.packages(c('dplyr', 'ggplot2'))

library(ggplot2)
library(dplyr)

# a) Download and read the data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

# Check the structure of the resulting data frame
str(pop)
summary(pop)

ATA
# b) Summarise 'sex' using a bar chart

ggplot(data = pop, aes(x = sex)) + geom_bar()

# c) Summarise 'taxes' using a histogram

ggplot(data = pop, aes(x = taxes)) + geom_histogram(bins=50)

# d) ATA (average tax amount).

ATA <- mean(pop$taxes)

# e) Draw a sample of 100 persona (with replacement)

sampleTaxes <- sample(pop$taxes, size = 100, replace = TRUE)
str(sampleTaxes)

# Calculate the sample mean
estATA1 <- mean(sampleTaxes)
estATA1

# f) Set R to the number of samples and n to the size of each sample
R <- 2000
n <- 500

simTaxes <- sample(pop$taxes, n*R, replace = TRUE)

samplesData <- data.frame(
  taxes = simTaxes, 
  r = rep(1:R, each = n)
)
groupedSamples <- group_by(samplesData, r)
estATAbySample <- summarise(groupedSamples, estATA = mean(taxes))

# g) Histogram: distribution of the sample mean

ggplot(data = estATAbySample, aes(x = estATA)) + 
  geom_histogram(bins=40) + # Draws the histograms
  xlim(c(5000, 15000)) # Limits the range of the x-axis from 5000 to 15000

# In the plot above we fix the range of the x-axis in order to better observe 
# the changing location and shape of the distribution without the disturbance
# of changing scales.

# h) Change the value of n above and re-run the code to see the distribution change

# i)

# Estimate the standard deviation of "taxes" using the single sample selected in e)
estStd1 <- sd(sampleTaxes)

# Calculate the 2.5% and 97.5% quantiles of the standard normal distribution
# and for the t-distribution with n - 1 degrees of freedom
# for use in the calculations of the confidence intervals

q1 <- qnorm(0.025)
q2 <- qnorm(0.975)
q1
q2

tq1 <- qt(0.025, df = 99)
tq2 <- qt(0.975, df = 99)
tq1
tq2

# Confidence interval using the normal distribution
c(estATA1 + q1*estStd1 /sqrt(100), estATA1 + q2*estStd/sqrt(100))

# Confidence interval using the t distribution
c(estATA1 + tq1 *estStd1 /sqrt(100), estATA1 + tq2*estStd/sqrt(100))

# j)

# First compute the estimates for the mean and for the standard deviation
# in each sample

estATAbySample <- summarise(groupedSamples,
                            estATA = mean(taxes),
                            estStd = sd(taxes))

# Then calculate upper and lower confidence interval bounds
# and the corresponding indicator variables.

samplesData <- within(estATAbySample, {
  # Add 5 new columns in the summarised data
  
  ## Lower limit of the confidence intervals
  normalConfIntLower <- estATA + q1 * estStd / sqrt(n)
  
  # Upper limit of the confidence intervals
  normalConfIntUpper <- estATA + q2 * estStd / sqrt(n)
  
  # Upper and lower limits of the confidence intervals using the t distribution
  tConfIntLower <- estATA + qt(0.025, df = n - 1) * estStd/sqrt(n)
  tConfIntUpper <- estATA + qt(0.975, df = n - 1) * estStd/sqrt(n)
  
  # Lengths of the confidence intervals
  normalConfIntLength <- normalConfIntUpper - normalConfIntLower
  tConfIntLength <- tConfIntUpper - tConfIntLower
  
  # (TRUE/FALSE) indicator variables for confidence intervals
  # based on the normal and t distributions
  # TRUE: the confidence interval covers ATA (the population mean)
  # FALSE: the confidence interval _does not_ cover ATA
  
  inNormalConfInt <- ATA >= normalConfIntLower & ATA <= normalConfIntUpper
  inTConfInt <- ATA >= tConfIntLower & ATA <= tConfIntUpper
})

# k) Display the first 50 confidence intervals for the mean (normal confidence intervals)
ggplot(data = samplesData[1:50, ], aes(x = r)) + 
  geom_errorbar(aes(ymin = normalConfIntLower, ymax = normalConfIntUpper)) +
  geom_point(aes(y = estATA)) +
  geom_hline(yintercept = ATA, linetype = 2, color = 'red') +
  ggtitle('Estimated (normal) confidence intervals for ATA') +
  xlab('Sample') +
  coord_flip()

# Get a summary of the samplesData and check the lengths of the normal and t conf. intervals
# as well as their coverage rates.

# l)
summary(samplesData)


# m)
t.test(sampleTaxes, mu = ATA)
testStatistic <- sqrt(100)*(estATA1 - ATA)/estStd1

# Distribution under the Null hypothesis is t-distribution with 100 - 1 = 99 degrees of freedom 
criticalLower <- qt(0.025, df = 99)
criticalUpper <- qt(0.975, df = 99)
criticalLower
criticalUpper

# Calculate the p-value "by hand"
# Note: pt 
2*(1 - pt(abs(testStatistic), df = 99))

testStatistic

# n)
# Run the test for every sample

# Examine the object returned by t.test
str(t.test(sampleTaxes, mu = ATA))

# Define a function for the test result
testH0 <- function(x) {
  testResult <- t.test(x, mu = ATA)
  return(testResult$p.value < 0.05)
}

# Use summary to run the function on each sample
samplesDataTest <- summarise(
  groupedSamples,
  falseRejection = testH0(taxes)
)

# Look at the to find the count of false rejections
summary(samplesDataTest)
