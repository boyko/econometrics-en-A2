# install.packages(c('dplyr', 'ggplot2'))

library(ggplot2)
library(dplyr)

# a) Download and read the data
pop <- read.csv('http://econometrics2018.s3-website.eu-central-1.amazonaws.com/data/cex16pop.csv')

# a) Calculate the population mean for male and female

popTaxesMale <- pop$taxes[pop$sex == 'Male']
popTaxesFemale <- pop$taxes[pop$sex == 'Female']

popMeanMale <- mean(popTaxesMale)
popMeanFemale <- mean(popTaxesFemale)

popMeanMale
popMeanFemale

# b) Boxplots for the distribution of taxes by sex
ggplot(data = pop, aes(y = taxes, x = sex)) + 
  geom_boxplot() + 
  ylim(c(-2000, 30000))


# c)
# Select a sample

# Population size
N <- nrow(pop)

sampledRows1 <- sample(1:N, size = 100, replace = TRUE)
sample1 <- pop[sampledRows1, ]

#d)

## Test hypothesis 1
t.test(taxes ~ sex, data = sample1)
t.test(taxes ~ sex, data = sample1, var.equal = TRUE)
## Test hypothesis 2

t.test(taxes ~ sex, data = sample1, alternative = 'less')

## Test hypothesis 3

t.test(taxes ~ sex, data = sample1, alternative = 'greater')

## Test hypothesis 3

# e)

## R: number of samples
R <- 2000

## sample size
n <- 500

sampleRows <- sample(1:N, size = n * R, replace = TRUE)
samplesData <- pop[sampleRows, ]

samplesData <- within(samplesData, {
  r <- rep(1:R, each = n)
})

groupedSamples <- group_by(samplesData, r)

testH0 <- function(taxes, sex) {
  testResult <- t.test(taxes ~ sex)
  return(testResult$p.value < 0.05)
}

testResultsBySample <- summarise(groupedSamples,
                          rejection = testH0(taxes, sex)
                          )
summary(testResultsBySample)
