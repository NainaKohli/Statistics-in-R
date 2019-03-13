# Central Limit Theorem # 
# In a survey of a company, mean salary of employees is 6066 dollars with SD of 2875 dollars. Consider the sample of 40 employees and find the probability their mean salary will be less than 6000 dollars? #
pnorm(6000, mean = 6066, sd = 2875)

# Probability of seeing a sample mean of 19,800 or lower if true population mean was 20,000 #
pnorm(19800, mean = 20000, sd = 800)

# The average of sample mean from 20 observations is 18,000. Now what is the probability of seeing the average of 18,000 or lower , if the true population mean was 20,000#
pnorm(18000, mean = 20000, sd = 894.42)

# Hypothesis #
# A cricket selection committee has a selection meeting and is confused on one player over the selection in the team. This particular player has a career batting average  of 40  with a standard deviation of 10. But in his recent 30 innings he has a batting average of 38. The selection committee has a selection criteria where the average should not be less than 40. Now they want to confirm on which statistics they want to relay on to make the selection? #
pnorm(38, mean = 40, sd = 1.82)

# A International cargo company delivers product from Mumbai to Dubai Port in 50 hrs. with a standard deviation of 12 hrs. is normally distributed #
# The Company has appointed you as a Supply Chain Analyst, and you observed the last 35 shipments and found the average delivery time between Mumbai and Dubai is 53 hrs. with a standard deviation of 2.5 hours.#
1 - pnorm(53, mean = 50, sd = 2.02)

# Hypothesis testing : When the sample size is less than 30 ( t-test) #
x <- c(158,160,154,176,162,175,159,179,155,172,162,160,156,161,165)
t.test(x, mu = 164, alternative = "less")

# Population standard deviation not known #

# 2 sample t test with equal variance #
sample1 <- c(5,9,4,8,7,11,10,8,6,7)
sample2 <- c(4,7,3,7,5,9,8,6,4,6)
t.test(sample1,sample2, var.equal=TRUE, paired=FALSE)

# 2 sample t test with unequal variance #
t.test(sample1,sample2, var.equal=FALSE, paired=FALSE)

# paired Sample t test #
y1 <- c(1,0,2,3,2,1,3,2,0,2)
y2 <- c(3,2,3,4,2,2,4,3,1,3)
t.test(y1,y2,paired=TRUE)
# Population Distribution Not Normal #
dbinom(15,18,0.3)

# ANOVA #
Delhi <- c(980,776,923,1498,999)	
Mumbai <-c(1123,1357,1152,921,959)	
Bangalore <-c(1084,1025,1114,1182,1022)
x<- data.frame(cbind(Delhi,Mumbai,Bangalore))
stacked.group <- stack(x)
stacked.group
anova_result <- aov(values~ind, data = stacked.group)
anova_result
summary(anova_result)

# Two way ANOVA #
df <- c(133,107,114,150,85,130,86,130,112,125,84,114,138,155,78,138,119,146,158,137,94,123,108,148,93,120,127,150,106,127)
discount <- c(rep(0.1,5),rep(0.2,5),rep(0.1,5),rep(0.2,5),rep(0.1,5),rep(0.2,5))
city <- c(rep("Delhi",10),rep("Mumbai",10),rep("Bangalore",10))  
results <-lm(df~discount*city)
anova(results)

# Chi Square Test #
# Test for homogeniety #
table1 = matrix(c(98,40,52,10),ncol=1)
colnames(table1)=c("Sales transaction")
rownames(table1)=c("Honda City","Honda Amaze","Honda Accord","Honda CR-V")
table1
chisq.test(table1)

# Test of indepedence #
table2=matrix(c(74,29,17,52,21,34,48,12,39),ncol=3)
colnames(table2)=c("Urban","SemiUrban", "Rural")
rownames(table2)=c("Honda City","Honda Amaze","Honda CRV")
table2
chisq.test(table2) 

# Goodess of fit #
chisq.test(c(84,16), p=c(0.9,0.1))
