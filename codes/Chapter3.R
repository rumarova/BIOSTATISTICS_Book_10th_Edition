
library(dplyr)
library(ggplot2)


#========================================#
##    Import Data
#========================================#

df <- read.csv("data/Chapter2.csv")


#========================================#
##    Exercise 2.3.11
#========================================#

###   (a) 
# construct the following distributions: frequency, relative frequency, cumulative
# frequency, and cumulative relative frequency; 

# cut into intervals of equal length
tmp1 = as.data.frame(cut_interval(df$Ex_2.3.11, length = 0.1))
colnames(tmp1) = "Class"
tmp1$Ratio = df$Ex_2.3.11

# create table & count frequency
t = as.data.frame(table(tmp1$Class))
colnames(t) = c("Class_Interval", "Frequency")
t$Cumulative_Freq = cumsum(t$Frequency)
t$Relative_Freq = t$Frequency/length(tmp1)
t$Cum_Relat_Freq = cumsum(t$Relative_Freq)

# construct graphs: histogram, frequency polygon, and stem-and-leaf plot.
ggplot(tmp1, aes(Class)) + 
  geom_bar() +
  labs(x= "S/R Ratio", y ="Frequency") + 
  theme_minimal()

ggplot(tmp1, aes(Ratio)) + 
  geom_freqpoly() +
  labs(x= "S/R Ratio", y ="Frequency") + 
  scale_x_continuous(breaks = seq(0,1.25,0.1)) +
  theme_minimal()

stem(tmp1$Ratio)

###   (b) 
# Describe these data with respect to symmetry and skewness as discussed in Exercise 2.3.1, part h.

# skewed to the right
summary(tmp1$Ratio)

###   (c) 
# The investigators defined as poor metabolizers of mephenytoin any subject with an S/ R
# mephenytoin ratio greater than .9. How many and what percentage of the subjects were poor
# metabolizers?
table(tmp1$Ratio > 0.9)
table(tmp1$Ratio > 0.9)[[2]]/216

###   (d) 
# How many and what percentage of the subjects had ratios less than .7? Between .3 and .6999
# inclusive? Greater than .4999?
table(tmp1$Ratio < 0.7)
table(tmp1$Ratio > 0.4999)
table(tmp1$Ratio >= 0.3 & tmp1$Ratio <= 0.6999)


#========================================#
##    Review Exercise 15
#========================================#

###   (a) Compute mean, median, variance, standard deviation, and coefficient of variation.
tmp2 <- na.omit(df$Ex_Review_15)
summary(tmp2)
mean(tmp2)
median(tmp2)
var(tmp2)
sd(tmp2)
sd(tmp2)/mean(tmp2)


###   (b) Construct a stem-and-leaf display.
stem(tmp2)


###   (c) Construct a box-and-whisker plot.
tmp2 <- as.data.frame(tmp2)

ggplot(tmp2, aes(tmp2)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(x= "GFR") + 
  theme_minimal()

###   (d) What percentage of the measurements is within one standard deviation of the mean? Two
# standard deviations? Three standard deviations?
m = mean(tmp2$tmp2)
s = sd(tmp2$tmp2)

s1 = ifelse(between(tmp2$tmp2, m-s, m+s), TRUE, FALSE)
length(s1[s1 == TRUE])/length(s1)

s2 = ifelse(between(tmp2$tmp2, m-2*s, m+2*s), TRUE, FALSE)
length(s2[s2 == TRUE])/length(s2)

s3 = ifelse(between(tmp2$tmp2, m-3*s, m+3*s), TRUE, FALSE)
length(s3[s3 == TRUE])/length(s3)

rm(tmp2, t, tmp1, m, s, s1, s2, s3)


#========================================#
##    Review Exercise 29
#========================================#

tmp3 <- na.omit(df$Ex_Review_29)


###   (a) For these data compute the following descriptive measures: mean, median, mode, variance,
# standard deviation, range, first quartile, third quartile, and IQR.

summary(tmp3)
mean(tmp3)
median(tmp3)
var(tmp3)
sd(tmp3)
sd(tmp3)/mean(tmp3)
range(tmp3)
IQR(tmp3)
quantile(tmp3)


###  (b) Construct the following graphs for the data: histogram, frequency polygon, stem-and-leaf plot,
# and boxplot.

# cut into intervals of equal length
tmp3 = as.data.frame(cut_interval(na.omit(df$Ex_Review_29), length = 5))
colnames(tmp3) = "Class"
tmp3$Frequency = na.omit(df$Ex_Review_29)


# construct graphs: histogram, frequency polygon, and stem-and-leaf plot.
ggplot(tmp3, aes(Class)) + 
  geom_bar() +
  labs(x= "Nutritional Status", y ="Frequency") + 
  theme_minimal()

ggplot(tmp3, aes(Frequency)) + 
  geom_freqpoly() +
  labs(x= "Nutritional Status", y ="Frequency") + 
  scale_x_continuous(breaks = seq(45,135,5)) +
  theme_minimal()

stem(tmp3$Frequency)

ggplot(tmp3, aes(Frequency)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(x= "Nutritional Status") + 
  theme_minimal()


###  (d) What proportion of the measurements are within one standard deviation of the mean? Two
# standard deviations of the mean? Three standard deviations of the mean?
m = mean(tmp3$Frequency)
s = sd(tmp3$Frequency)

s1 = ifelse(between(tmp3$Frequency, m-s, m+s), TRUE, FALSE)
length(s1[s1 == TRUE])/length(s1)

s2 = ifelse(between(tmp3$Frequency, m-2*s, m+2*s), TRUE, FALSE)
length(s2[s2 == TRUE])/length(s2)

s3 = ifelse(between(tmp3$Frequency, m-3*s, m+3*s), TRUE, FALSE)
length(s3[s3 == TRUE])/length(s3)


###  (e) What proportion of the measurements are less than 100?
table(tmp3$Frequency < 100)[[2]]/nrow(tmp3)

###  (f) What proportion of the measurements are less than 50?
table(tmp3$Frequency < 50)[[2]]/nrow(tmp3)
