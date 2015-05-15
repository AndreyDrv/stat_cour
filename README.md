---
title: "Statistical Inference Course Notes"
author: "Andrey G"
output:
  pdf_document:
    toc: yes
    toc_depth: 3
  html_document:
    highlight: pygments
    theme: spacelab
    toc: yes
  word_document: default
---


# w2

```
################## binomial distr
choose(8, 7) * .5 ^ 8 + choose(8,8) * .5 ^ 8
#or
pbinom(6, size = 8, prob = .5, lower.tail = FALSE)

##################

################## normal distr
#Ex1: Assume the number of daily ad clicks for companies is approximately 
#normally distributed with a mean of 1,020 clicks per day, 
#and a standard deviation of 50 clicks per day.
#What is a probability of 1,160 clicks?

pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
# or
pnorm(2.8, lower.tail = FALSE) # (1160-1020)/50 = 2.8


#Ex2: Assume that the number of daily ad clicks for this company is 
#approximately normally distributed with a mean of 1020 and a standard 
#deviation of 50. What number of daily ad clicks would represent the one
#where 75% have fewer clicks?

# 1020 + 50 = 1sigma (68 %)

qnorm(0.75, mean = 1020, sd = 50)

##################


################## poisson distr
#Ex: if the number of people that show up to a bus stop is Poisson with
#a mean of 2.5 people per hour. We watch the bus stop for four hours.
#What's the probability that three or four, three or fewer people show up 
#the whole time?

ppois(3, lambda = 2.5 * 4)


################## law of large numbers
#Ex: coin flip

n <- 1000
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n) 
plot(means) #mean of the large population is close to M(X)

##################


################## confidence intervals
#Ex: give confidence intervals for the average height of sons
#Galtons data

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1,1) * qnorm(0.975) * sd(x)/sqrt(length(x))) / 12

#######

#Ex2: you were running for political office and your campaign advisor told you
#that in a random sample of 100 likely voters, 56 intended to vote for you.
#Can you relax?

# 1. quick (intuitive) calculation
#1/sqrt(100)= 0.1   - 95% interval of (0.46, 0.66)

# 2. binomial interval
0.56 + c(-1,1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
#or
binom.test(56, 100)$conf.int

# answer: nope, more adv needed

#########
#Ex3: Simulation
# 20 coin flips
# 1000 simulations

n <- 20 # 20 coin flips
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000 # 1000 simulations
coverage <- sapply(pvals, function(p) {     #loop
  phats <- rbinom(nosim, prob = p, size = n)/n #foreach success prob 
                                                #generate 1000 sets of 10 coin flips
  ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n) #lower limit of confidence
  ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n) #upper -//- for each case
  mean(ll < p & ul > p) #calculate the proportion of times that they can
                        #cover that true value of p that I used to simulate the data
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + 
  geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

#result: not good. Need more coin flips in a row: n <- 20 change to n <- 100

#########
#Ex4:a nuclear pump that failed 5 times out of 94.32 days, 
#given 95% confidence interval for the failure rate per day

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1,1) * qnorm(0.975) * sqrt(lambda/t), 3)
#or
poisson.test(x, T = 94.32)$conf

#result: variance estimation: confidence interval

#########
#Ex5: Small lambda simulations

lambdavals <- seq(0.005, 0.10, by = .01); 
nosim <- 1000;
t <- 100
# calculate coverage using Poisson intervals
coverage <- sapply(lambdavals, function(lambda){
  # calculate Poisson rates
  lhats <- rpois(nosim, lambda = lambda * t) / t
  # lower bound of 95% CI
  ll <- lhats - qnorm(.975) * sqrt(lhats / t)
  # upper bound of 95% CI
  ul <- lhats + qnorm(.975) * sqrt(lhats / t)
  # calculate percent of intervals that contain lambda
  mean(ll < lambda & ul > lambda)
})
# plot CI results vs 95%
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

#result: bad on small lambda values: should not use asymtotic interval for small
#         values of lambda (not enough coverage).
#todo: extend monitoring time to t <- 100 to t <- 1000 (number of days)
```
