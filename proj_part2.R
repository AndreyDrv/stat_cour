###########################################################################
# The Effect of Vitamin C on Tooth Growth in Guinea Pigs
###########################################################################
#
#The response is the length of odontoblasts (teeth) in each of 10 guinea 
#pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) with each 
#of two delivery methods (orange juice or ascorbic acid).
#
#A data frame with 60 observations on 3 variables.
#[,1]  len	numeric	Tooth length
#[,2]	supp	factor	Supplement type (VC or OJ).
#[,3]	dose	numeric	Dose in milligrams.


analyze <- function(){
  d <- data(ToothGrowth)

  ###############################################
  #1. Summary of the data
  #-perform an exploratory data analysis of at least a single plot 
  #or table highlighting basic features of the data
  
  summary(d)
  #Supplementary:
  #50% (30 observations) of Orange Juice supplementary
  #50% (30 observations) of ascorbic acid supplementary
  #Dose:
  #Mean of doze of a supplementary is 1.167 mg
  #Max doze of a supplementary is 2.000 mg
  #Min doze of a supplementary is 0.500 mg
  #Length:
  #Mean of tooth length is 18.81 cm
  #Max tooth length is 33.90 cm
  #Min tooth length is 4.20 cm

  ggplot(d, aes(x=len)) +
    geom_histogram(binwidth=.5, colour="black", fill="white") +
    geom_vline(aes(xintercept=(mean(d[,1]))), color="blue", linetype="solid", size=1) +
    geom_vline(aes(xintercept=(c(-1,1) * sd(d[,1]) + mean(d[,1]))), color="blue", linetype="dashed", size=.5)
  #rem?
  #68% of values are between the dotted lines
  
  ggplot(d, aes(x=len)) + 
    geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") + 
    geom_density(alpha=.2, fill="#FF6666")
  #we can say the data is slightly normalized. As the shape is more looks
  #like gaussian. But there is not enough data (only 60 observations) to say it
  #is fully normalized.
  
  ggplot(d, aes(x=len, fill=supp)) + geom_density(alpha=.3)
  oj_mean <- mean(d[which(d$supp=="OJ"),][,1])
  vc_mean <- mean(d[which(d$supp=="VC"),][,1])
  #comparing the tooth length by supplementary type we can see that with OJ
  #has better results as it has more mean
  
  ###############################################
  #2. Use confidence intervals and/or hypothesis tests to compare 
  #tooth growth by supp and dose. (Only use the techniques from class, 
  #even if there's other approaches worth considering)
  #-Perform some relevant confidence intervals and/or tests
  x1 <- d[which(d$supp=="VC"),][,1]
  x2 <- d[which(d$supp=="OJ"),][,1]
  (mean(x1) + c(-1, 1) * qnorm(.975) * sd(x1) / sqrt(length(x1)))  
  #confidence interval for VC
  (mean(x2) + c(-1, 1) * qnorm(.975) * sd(x2) / sqrt(length(x2)))  
  #confidence interval for OJ
  (mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x)))
  #confidence interval for both
  
  ###############################################
  #3. State your conclusions and the assumptions needed for your conclusions
  #-Were the results of the tests and/or intervals interpreted in the context 
  #of the problem correctly? 
  #-describe the assumptions needed for their conclusions?
  
  #hypotesis #1: OJ has better influence on tooth grow length
  #hypotesis #2: The influece depends on doze  
  #hypotesis #3: tooth growth by supp and dose
  
  
}
