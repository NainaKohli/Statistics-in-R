# Mean, Median, Mode #
# Dataset #
data_series <- c(19,4,33,2,51,32,2,41,18,2,4,1)
# Mean #
data_series_mean <- mean(data_series)
data_series_mean
# Median #
data_series_median <- median(data_series)
data_series_median
# Mode #
# There is no inbuilt function to calculate mode Hence, ite user function to calculate mode #
y <- table(data_series)
y
names(y)[which(y ==max(y))]

# Frequency Distribution #
class <- c("20-30","30-40", "40-50","50-60","60-70","70-80")
class
freq <- c(5,8,9,10,6,2)
freq
rel_freq <- freq/sum(freq)
rel_freq
cum_freq <- cumsum(rel_freq)
freq_table <- cbind(class, freq,rel_freq,cum_freq)
freq_table



# Ogives Table #
df <- c(12, 13, 17, 21, 24, 24, 26, 27, 27, 30, 32, 35, 37, 38, 41, 43, 44, 46, 53, 58
)
range(df)
breaks <- seq(10,60, by = 10)
breaks
df.cut <- cut(df, breaks, right = FALSE)
df.cut
frequency <- table(df.cut)
frequency
cbind(frequency)
# Relative Frequency #
df.relfreq <- frequency/sum(frequency)
cbind(frequency,df.relfreq)

# Cumulative Frequency #
df.cumfreq <- cumsum(frequency)
df.cumfreq

# Cumulative Relative Frequency #
df.cumrelfreq <- cumsum(df.relfreq)
df.cumrelfreq

cbind(frequency,df.relfreq,df.cumfreq,df.cumrelfreq)
# ogives graph #
library(agricolae)
library(spData)
library(spDataLarge)
h <- graph.freq(df,plot=FALSE)
h
points <- ogive.freq(h,col="red",frame=FALSE, xlab="class Midpoints",
                     ylab="cumulative frequency", main="ogive")
plot(points,type="o",pch=9,las=1,bty="l")

# histogram #
hist <- hist(df)

# Line Chart #
library(plotly)
library(ggplot2)
year <- c(1985:2006)
year
Inflation_Rate <- c(3.56,1.86, 3.65,	4.14,	4.82,	5.4,	4.21,	3.01,	2.99,	2.56,	2.83,	2.95,	2.29,	1.56,	2.21,	3.36,	2.85,	1.59,	2.27,	2.68,	3.39,	3.24)
Inflation_Rate
data <- data.frame(year, Inflation_Rate)
data
p <- plot_ly(data, x = ~year, y = ~Inflation_Rate, type = 'scatter', mode = 'lines')
p

# Scatter Plot #
vol_per_day <- c(23,26,29,33,38,42,50,55,60)
cost_per_day <- c(125,140,146,160,167,170,188,195,200)
data_scatter <- data.frame(vol_per_day, cost_per_day)

scatter_plot <- plot_ly(data = data_scatter,type = "scatter",mode = "markers", x = ~vol_per_day, y = ~cost_per_day)
scatter_plot

# Range #
v1 <- c(2, 3, 4, 6, 9, 3, 7, 16, 21)
range(v1)
v2 <- c(8, 11, 5, 9, 7, 6, 19, 58, 45, 90, 4001)
range(v2)


# Box Plot #
data_box <- c(23,1,1,3,38,44,33,33,46,107,8,3,4,9,139,66,53,49,22,127,62,2,13,1,40,0,5,48,136,82,
              2,6,78,123,0,31,19,99,86,0,115,68,100,61,68,14,115,2,31,102,2,11,43,58,22,22,31,0,26,77,
              37,15,7,6,0,23,128,38,1,106,183,66,108,133,21,66,12,15,18,77,31,80,23,20,117,3,0,86,35,112,
              37,107,16,7,9,55,94,22,0,81,2,35,9,24,59,1,12,34,8,100,2,87,28,22,54,2,0,63,64,105,
              118,37,8,0,28,10,18,11,68,18,82,0,57,31,2,91,9,107,54,27,10,30,79,16,2,31,54,25)
boxplot <- plot_ly(y = ~data_box, type = "box")
boxplot

# variance and standard deviation #
data1 <- c(12,13,17,21,24, 24, 26,27, 27, 30, 32, 35, 37, 38, 41, 43, 44, 46,53,60)
data2 <- c(34, 14, 31, 59, 11, 50, 27, 33, 53, 34, 13, 13, 42, 29, 33, 42, 34, 33, 44, 21)

# variance and Standard deviation of the delivery time taken by delivery boy 1 #
var(data1)
sd(data1)

# variance and Standard deviation of the delivery time taken by delivery boy 2 #
var(data2)
sd(data2)


# Emperical Rule for the delivery time taken by delivery boy 1 #
mean_data1 <- mean(data1)
sd_data1 <- sd(data1)

# Calculate the lower and upper bound using meand and standard deviation #
lower.bounds = mean_data1 - 1:3*sd_data1
lower.bounds
upper.bounds = mean_data1 + 1:3*sd_data1
upper.bounds

#  calculate the proportion of observations between each pair of the upper and lower bounds #
one.sd = mean(data1 > lower.bounds[1] & data1 < upper.bounds[1])
one.sd
two.sd = mean(data1 > lower.bounds[2] & data1 < upper.bounds[2])
two.sd
three.sd = mean(data1 > lower.bounds[3] & data1 < upper.bounds[3])
three.sd

# Skewness #
data <- c(23,1,1,3,38,44,33,33,46,107,8,3,4,9,139,66,53,49,22,127,62,2,13,1,40,0,5,48,136,82,
             2,6,78,123,0,31,19,99,86,0,115,68,100,61,68,14,115,2,31,102,2,11,43,58,22,22,31,0,26,77,
             37,15,7,6,0,23,128,38,1,106,183,66,108,133,21,66,12,15,18,77,31,80,23,20,117,3,0,86,35,112,
             37,107,16,7,9,55,94,22,0,81,2,35,9,24,59,1,12,34,8,100,2,87,28,22,54,2,0,63,64,105,
             118,37,8,0,28,10,18,11,68,18,82,0,57,31,2,91,9,107,54,27,10,30,79,16,2,31,54,25)
skewness(data)

# Kurtosis #
kurtosis(data)

