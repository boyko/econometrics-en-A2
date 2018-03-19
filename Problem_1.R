# install.packages(c('dplyr', 'ggplot2'))


library(ggplot2)
library(dplyr)

# a) Download and read the data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

# Check the structure of the resulting data frame
str(pop)
summary(pop)


# b) Summarise 'sex' using a bar chart

ggplot(data = pop, aes(x = sex)) + geom_bar()

# c) Summarise 'taxes' using a histogram

ggplot(data = pop, aes(x = taxes)) + geom_histogram(bins=50)

# d) ATA

ATA <- mean(pop$taxes)

# e) Draw a sample

sampleTaxes <- sample(pop$taxes, 100, replace = TRUE)
str(sampleTaxes)

estATA1 <- mean(sampleTaxes)
estATA1

# f) Estimate ATA from the sample
R <- 1000
n <- 1000

simTaxes <- sample(pop$taxes, n*R, replace = TRUE)
samplesData <- data.frame(taxes = simTaxes, r = rep(1:R, each = n))
groupedSamples <- group_by(samplesData, r)
estATAbySample <- summarise(groupedSamples, estATA = mean(taxes))

# g) 

ggplot(data = estATAbySample, aes(x = estATA)) + geom_histogram()


# h) Change the value of n above and re-run the code to see the distribution change

# i)
# Estimate the standard deviation 

s <- sd(sampleTaxes)

# The 95\% confidence interval is given by:

c(estATA1 - 1.96*s/sqrt(100), estATA1 + 1.96*s/sqrt(100))

# j)
estATAbySample <- summarise(groupedSamples, 
                            estATA = mean(taxes), 
                            s = sd(taxes))
samplesData <- within(estATAbySample, {
  confIntLower <- estATA - 1.96*s/sqrt(n)
  confIntUpper <- estATA + 1.96*s/sqrt(n)
  trueATA <- ATA
  inConfidenceInterval <- trueATA >= confIntLower & trueATA <= confIntUpper
})

table(samplesData$inConfidenceInterval)
