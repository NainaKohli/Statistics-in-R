# Binomial Distribution #
# If you Toss a coin 5 times, find the probability of getting exactly 2 heads #
dbinom(2, size = 5, prob=0.5)

# If you toss the same coin 5 times, find the probability of getting upto 2 heads #
prob <- sum(pbinom(2, size=5, prob=0.5) )
prob 
# If you toss the coin 5 times, find the probability of getting more than 2 heads #
1 - prob

# An E-commerce company delivers 3.4% defective goods to its company. A sample of 10 deliveries is taken , What is the probability that the sample contains exactly 2 defective parts #
dbinom(2, size = 10, prob=0.034)

# An E-commerce company delivers 3.4% defective goods to its company . A sample of 30 deliveries is taken , What is the probability that the sample contains upto 2 defective parts #
prob1 <- sum(pbinom(2, size=30, prob=0.034) )
prob1

# Hyper Geometric Distribution #
# A small voting district has 101 female voters and 95 male voters. A random sample of 10 voters is drawn. What is the probability exactly 7 of the voters will be female? #
dhyper(7, 101,95, 10, log = FALSE)


# NegativeBinomial Distribution #
# An Indian oil company conducts a geological study that indicates that an exploratory oil well should have a 20% chance of striking oil. What is the probability that the first strike comes on the third well drilled? #
dnbinom(2, 1, 0.2)

# Geometric Distribution #
# In a country 10% of the people evade legitimate taxes. What is the probability  that a tax official will need to raid at most 20 people, before finding a tax evader ( first tax evader) #
dgeom(x= 20, prob = 0.1, log = FALSE)

# Poisson Distribution #
# On an average, 12 people visit a restaurant in one hour, what is the probability that 15 people may visit in next one hour. #
dpois(15, lambda=12) 

# Normal Distribution #
# A company has 500 employees, salary of whom is normally distributed, with an  average of Rs.40,000 and Standard deviation of  Rs.6000. Suppose you pick a random employee from the 500 employees,  what are chances he/she earns less than Rs.30,000 #
pnorm(30000, mean = 40000, sd = 6000)

# Normal distribution of same problem mentioned above for chances he/she earns more than Rs.50,000 #
1-pnorm(30000, mean = 40000, sd = 6000)

# Normal distribution of same problem mentioned above for chances he/she earns exact Rs.50,000 #
dnorm(50000,mean = 40000, sd = 6000)

# The average score of Virat Kohli is 53 and the Standard Deviation is 40, assume the run scored by Kohli is normally distributed . Find out the probabilities of Kohli hitting 100 or more than 100 run in the next inning. #
1- pnorm(100, mean = 53, sd = 40)
