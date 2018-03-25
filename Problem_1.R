# install.packages(c('dplyr', 'ggplot2'))
rm(list = ls())
library(ggplot2)
library(dplyr)

# a) Download and read the data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

# Check the structure of the resulting data frame
# 1) number of observations (rows)
# 2) number of variables (columns)
# 3) Class of the variables: numeric, factor, character, etc.

str(pop)

# Check the summary of the variables in the data.frame
# 1) Look whether the variables are summarised appropriately
#    Categorical variables (e.g. sex) are summarised by a frequency table,
#    i.e. a table of counts (how many observations have sex=Female, how many obs have sex=Male, etc.)
# 2) Look for extreme/unusual values
summary(pop)

# b) Summarise 'sex' using a bar chart

ggplot(data = pop, aes(x = sex)) + geom_bar()

# c) Summarise 'taxes' using a histogram

ggplot(data = pop, aes(x = taxes)) + geom_histogram(bins=50)


# d) Population mean: average tax amount paid by person

populationMean <- mean(pop$taxes)

# e) Draw a sample of 100 persons (with replacement)

sampleTaxes <- sample(pop$taxes, size = 100, replace = TRUE)
str(sampleTaxes)

# Calculate the sample mean which we use as an estimate of the population mean
# Run the code several times to see how the estimated mean changes after each
# run as a different sample get selected.

sampleMean1 <- mean(sampleTaxes)
sampleMean1

# f) Set R to the number of samples and n to the size of each sample
# We draw a large number (R) of samples in order to visualise the distribution
# of the sample mean

R <- 2000 # number of samples
n <- 1000 # sample size

# First draw a very large sample from the population with n*R elements
simTaxes <- sample(pop$taxes, size = n*R, replace = TRUE)

# Arrange the elements from the large sample in a table
# and create an indicator variable called r that identifies
# the samples

samplesData <- data.frame(
  taxes = simTaxes, 
  r = rep(1:R, each = n)
)

# We want to calculate the sample mean for each sample 
# To do this we apply two functions from the dplyr package
# group_by and summarise.
# You can think about the following 
# as the equivalent to SELECT mean(some_column) FROM data.frame GROUP_BY some_other_column;

# First we group the data by the samples index r
groupedSamples <- group_by(samplesData, r)

# Next we use the grouped data from the previous step
# to calculate the sample means
sampleMeansBySample <- summarise(groupedSamples, sampleMean = mean(taxes))

# g) Histogram: distribution of the sample mean

# Next we draw a histogram of the sample means
# In the plot above we fix the range of the x-axis (xlim) in order to better observe 
# the changing location and shape of the distribution without the disturbance
# of changing scales.

# The central limit theorem states that 
# the limiting distribution of the sample mean
# is a normal distribution with mean = populationMean and standard deviation equal
# to the population standard deviation (here of the variable 'taxes')
# devided by the square root of the sample size n

# Draw a histogram of the sample means. Note that 'count' on the y-axis
# refers to a number of SAMPLES and NOT to a number of persons 
# as was the case in the histogram from part c).

ggplot(data = sampleMeansBySample, aes(x = sampleMean)) + 
  geom_histogram(bins=50) # Draws the histograms
  
# Re-run the simulation by changing the sample size n
# to different values and observe the change of location
# of the distribution.
# For low values of n it resembles the distribution of taxes in the population (skewed)
# for high values of n it appears centered around some value


# To see more clearly where the center of the distribution is
# we will superimpose a vertical line at the population mean
# and to see the shape of the distribution
# we superimpose a plot of a normal density function
# with mean equal to the population mean and standard deviation
# equal to the standad deviation of taxes in the population devided
# by the square root of n

# First we compute the standard deviation
# of taxes in the population
populationStdDev <- sd(pop$taxes)


# To be able to see the plotted density function
# we rescale the height of the bars so that their
# area integrates to 1.

ggplot(data = sampleMeansBySample, aes(x = sampleMean)) + 
  geom_histogram(aes(y=..density..), bins=50) + # Draws the histograms with rescaled bar heights
  xlim(c(5000, 15000)) + # Fix the range of the x-axis
  stat_function(fun = dnorm, 
                args = list(mean = populationMean, sd = populationStdDev/sqrt(n)), 
                color = 'red2') + # Plot the density function
  geom_vline(xintercept = populationMean, color = 'orange1', linetype = 2) # Draw the vertical line at the population mean

# In order to see how close the distribution of the sample means


# h) Change the value of n above and re-run the code to see the distribution change

# i) 

# Estimate the standard deviation of "taxes" using the single sample selected in e)
sampleStdDev1 <- sd(sampleTaxes)

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
c(sampleMean1 + q1*sampleStdDev1 /sqrt(100), sampleMean1 + q2*sampleStdDev1/sqrt(100))

# Confidence interval using the t distribution
c(sampleMean1 + tq1*sampleStdDev1/sqrt(100), sampleMean1 + tq2*sampleStdDev1/sqrt(100))

# j)

# First compute the estimates for the mean and for the standard deviation
# in each sample
# This is the same as we did above, only this time we compute
# the mean AND the standard deviation in each sample.
# NOTE that we do not need to create a new grouped
# data set because groupedSamples already exists

sampleMeansBySample <- summarise(groupedSamples,
                            sampleMean = mean(taxes),
                            sampleStdDev = sd(taxes)
                            )

# Then calculate upper and lower confidence interval bounds
# and the corresponding indicator variables.

samplesData <- within(sampleMeansBySample, {
  # Add 5 new columns in the summarised data
  
  ## Lower limit of the confidence intervals
  normalConfIntLower <- sampleMean + q1 * sampleStdDev / sqrt(n)
  
  # Upper limit of the confidence intervals
  normalConfIntUpper <- sampleMean + q2 * sampleStdDev / sqrt(n)
  
  # Upper and lower limits of the confidence intervals using the t distribution
  tConfIntLower <- sampleMean + qt(0.025, df = n - 1) * sampleStdDev/sqrt(n)
  tConfIntUpper <- sampleMean + qt(0.975, df = n - 1) * sampleStdDev/sqrt(n)
  
  # Lengths of the confidence intervals
  normalConfIntLength <- normalConfIntUpper - normalConfIntLower
  tConfIntLength <- tConfIntUpper - tConfIntLower
  
  # (TRUE/FALSE) indicator variables for confidence intervals
  # based on the normal and t distributions
  # TRUE: the confidence interval covers the population mean
  # FALSE: the confidence interval _does not_ cover the population mean
  
  # Normal confidence intervals
  inNormalConfInt <- populationMean >= normalConfIntLower & populationMean <= normalConfIntUpper
  
  # t confidence intervals
  inTConfInt <- populationMean >= tConfIntLower & populationMean <= tConfIntUpper
})

# k) Display the first 50 confidence intervals for the mean (normal confidence intervals)

ggplot(data = samplesData[1:50, ], aes(x = r)) +  # Select the data 
  # geom_errorbar draws the lines that represent the CI
  geom_errorbar(aes(ymin = normalConfIntLower, ymax = normalConfIntUpper)) +
  # geom_point draws the sample means
  geom_point(aes(y = sampleMean)) +
  # geom_hline draws the dashed line at the population mean
  geom_hline(yintercept = populationMean, linetype = 2, color = 'red') +
  # Give the plot a descriptive title
  ggtitle('Estimated (normal) confidence intervals for the population mean') +
  # Give the x-axis a descriptive title
  xlab('Sample') +
  # Flip the x and y-axis so that the lines appear horizontal instead of vertical
  coord_flip()

# l) 
# Get a summary of the samplesData and check the lengths of the normal and t conf. intervals
# as well as their coverage rates.

summary(samplesData)


# m)
t.test(sampleTaxes, mu = populationMean)
testStatistic <- sqrt(100)*(sampleMean1 - populationMean)/sampleStdDev1

# Distribution under the Null hypothesis is t-distribution with 100 - 1 = 99 degrees of freedom 
criticalLower <- qt(0.025, df = 99)
criticalUpper <- qt(0.975, df = 99)
criticalLower
criticalUpper

# Compare the test statistic with the critical values
testStatistic

# Calculate the p-value "by hand"
# Note: pt(x, df = D) computes the t distribution function with 'D' degrees of freedom
# at point 'x'. This is the probability P(T <= x) where T follows a t-distribution with 'D'
# degrees of freedom

2*(1 - pt(abs(testStatistic), df = 99))



# n)
# Run the test for every sample

# Examine the object returned by t.test
str(t.test(sampleTaxes, mu = populationMean))

# Define a function for the test result
# The function returns TRUE if the test rejects the
# null hypothesis at a 5% error probability

testH0 <- function(x) {
  testResult <- t.test(x, mu = populationMean)
  return(testResult$p.value < 0.05)
}

# Use summary to run the function on each sample
# Here we _know_ that the null hypothesis is true!
# so each rejection is a _false_ rejection!

samplesDataTest <- summarise(
  groupedSamples,
  falseRejection = testH0(taxes)
)

# Look at the to find the count of false rejections
# Calculate the false rejection rate by dividing the 
# count and the total number of samples R ()
summary(samplesDataTest)
