#investigate the exponential distribution in R and compare it with 
#the Central Limit Theorem

#Illustrate via simulation and associated explanatory text 
#the properties of the distribution of the mean of 40 exponentials:
#1. Show the sample mean and compare it to the theoretical mean 
#of the distribution.
#2. Show how variable the sample is (via variance) and compare 
#it to the theoretical variance of the distribution.
#3. Show that the distribution is approximately normal.





#rexp(n, lambda) #exponential distribution can be simulated
                #lambda is the rate parameter = 0.2.
#The mean of exponential distribution is 1/lambda

compare <- function()
{
  lambda <- 0.2
  sigma <- 1/lambda
  nosim <- 1000
  #cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
  
  data_1 <- NULL
  data_2 <- NULL
  
  data_1 = rexp(nosim, lambda)
  for (i in 1 : nosim) data_2 = c(data_2, mean(rexp(40, lambda)))
  
  dat <- data.frame(x=data_2)
  
  #1. means: theoretical distr, distr of 1000, distr(CLT) 40x1000 
  means <- c(1/lambda, mean(data_1), mean(data_2))
  ##TODO: plot graph here - histogram of data_2 + mean + theoretical mean
  sigmas_theoretical <- c(-1,1) * sigma + means[1]
  sigmas_practical   <- c(-1,1) * sigma + means[3]
  
  g1 <- ggplot(dat, aes(x=x)) +
    geom_histogram(binwidth=.5, colour="black", fill="white") +
    geom_vline(aes(xintercept=means[1]), color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=means[3]), color="blue", linetype="solid", size=1)
  
  
  #2. variances
  variance_fixed = sigma/sqrt(1000)
  ##TODO: plot graph here - histogram of data_2 + mean (+3 sigma lines) 
                                   #+ theoretical mean (+3 sigma lines)
  g2 <- ggplot(dat, aes(x=x)) +
    geom_histogram(binwidth=.5, colour="black", fill="white") +
    geom_vline(aes(xintercept=means[1]), color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=means[3]), color="blue", linetype="solid", size=1) +
    geom_vline(aes(xintercept=sigmas_theoretical), color="blue", linetype="solid", size=.5) +
    geom_vline(aes(xintercept=sigmas_practical), color="red", linetype="dashed", size=.5)  
  
  #3. show distribution is normal
  ##TODO: plot data_2 and line above

  g3 <- ggplot(dat, aes(x=x)) + 
    geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") + 
    geom_density(alpha=.2, fill="#FF6666")
  
  
#  g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
#  g <- g + stat_function(fun = dnorm, size = 2)
#  g + facet_grid(. ~ size)


  
  
  
}
