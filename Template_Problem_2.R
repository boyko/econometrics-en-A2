library(ggplot2)
library(dplyr)

# a) Download and read the data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

# a) Calculate the population mean for male and female

popTaxesMale <- 
popTaxesFemale <- 

popMeanMale <- mean(???)
popMeanFemale <- mean(???)

# b) Boxplots for the distribution of taxes by sex
ggplot(data = pop, aes(y = ???, x = ???)) + 
  geom_boxplot() + 
  ylim(c(-2000, 30000))


# c) # Select a sample

N <- nrow(pop)

## This time we need to sample rows from a table, not single values

sampledRows1 <- sample(1:N, size = 100, replace = TRUE)
sample1 <- pop[sampledRows1, ]

#d)

## Test hypothesis 1
t.test(taxes ~ sex, data = sample1)
t.test(taxes ~ sex, data = sample1)

## Test hypothesis 2

t.test(taxes ~ sex, data = sample1, alternative = )

## Test hypothesis 3

t.test(taxes ~ sex, data = sample1, alternative = )

## Test hypothesis 3

# e)

## R: number of samples
R <- 2000

## sample size
n <- 500

sampleRows <- sample(1:N, size = n * R, replace = TRUE)
samplesData <- pop[sampleRows, ]

samplesData <- within(samplesData, {
  samplenIndex <- rep(1:R, each = n)
})

rejectH0 <- function(taxes, sex) {
  testResult <- t.test(taxes ~ sex, alternative = 'two.sided')
  return(testResult$p.value < 0.05)
}

samplesSummary <- 
  samplesData %>%
  group_by(sampleIndex) %>%
  summarise(
    rejectedH0 = rejectH0(taxes, sex)
  )

table(samplesSummary$rejectedH0)
