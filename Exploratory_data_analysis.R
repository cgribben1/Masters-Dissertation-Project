################################################################################ Loading packages ################################################################################ 

library(boot)
library(pscl)
library(bestglm)
library(MASS)
library(leaps)
library(dplyr)
library(caret)
library(ggpubr)
library(ggplot2)
library(vcd)
library(pscl)

################################################################################ Loading and formatting the data ################################################################################ 

chic <- read.csv("Chicago_data_absolute_final.csv")

# Convert categorical variables to factors
chic$district = factor(chic$district)
chic$date = factor(chic$date)
chic$hour_of_day = factor(chic$hour_of_day)
chic$day_of_week = factor(chic$day_of_week)
chic$month = factor(chic$month)
chic$year = factor(chic$year)

# Create the independent variables
hour <- unlist(data.frame(chic$hour_of_day))
district <- unlist(data.frame(chic$district))
month <- unlist(data.frame(chic$month))
weekday <- unlist(data.frame(chic$day_of_week))
date <- unlist(data.frame(chic$date))

# Create the dependent variable
count <- unlist(data.frame(chic$crime_count))

################################################################################ Mean and Variance ################################################################################ 

mean(chic$crime_count)
var(chic$crime_count)

################################################################################ Black Pop % vs Crime Rate correlation ################################################################################ 

blackpop = c(0.218,0.698,0.950,0.626,0.947,0.979,0.974,0.216,0.120,0.339,0.857,0.186,0.080,0.940,0.012,0.039,0.093, 0.068, 0.112, 0.613, 0.185, 0.178)
crimerates = c(3.6086077,3.118829828,4.185046853,2.835824398,3.668772515,3.886152728,5.161697457,1.707684347,1.867857943,2.209699135,5.515949144,2.356591512,2.078496322,4.552810387,1.023000261,1.240735343,2.254577456,1.367510683,1.176305612,1.988669917,1.303733746,1.79043969)
arrestrates = c(1.101734601,0.90852796,1.16342128,0.669447704,0.966637991,1.063715723,1.50370756,0.417935668,0.557720595,0.723582261,2.400942191,0.602233536,0.457142129,1.947610078,0.201031703,0.28392183,0.585136832,0.320505414,0.299203541,1.96192E-05,0.319771976,0.22530453)

cor.test(blackpop,crimerates)
cor.test(blackpop,arrestrates)
dem <- read.csv("demographics_regression.csv")
cor.test(dem$Crime_rate,dem$black..,method="pearson")
cor.test(dem$Crime_rate,dem$black..,method="spearman")

################################################################################ Q-Q Plot and Deviance-Residual Plot ################################################################################ 

### Q-Q Plot
qqnorm(chic$crime_count)
qqline(chic$crime_count)



### Deviance-Residual Plot (using Poisson Model defined in model-construction stage)
deviance_residuals <- residuals(poissonmodel, type = "deviance")
fitted_values <- poissonmodel$fitted.values

ggplot(data.frame(deviance_residuals, fitted_values), aes(x = fitted_values, y = deviance_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Deviance-Residual Plot of Poisson Model", subtitle = "Model constructed using predictors 'district', 'hour_of_day', 'month', and 'day_of_week'",
       x = "Fitted Values",
       y = "Deviance Residuals")

################################################################################ Making histograms with distributions overlayed ################################################################################ 

hist(chic$crime_count, freq = FALSE, main = "Comparison of Crime Count distribution with Poisson and Negative Binomial PMF", xlab = "Crime Count Value (per Half Day)",breaks=400,xlim=range(0:60),ylim=range(0,0.1))

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(chic$crime_count)
x <- seq(0, max(chic$crime_count))
points(x, (dpois(x, lambda)), type = "l", col = "blue")

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(chic$crime_count)
x <- seq(0, max(chic$crime_count))
points(x, (dnbinom(x, size = var(chic$crime_count)/mean(chic$crime_count), mu = lambda)), type = "l", col = "red")

legend( x="topright", 
        legend=c("Poisson","Negative Binomial"), 
        col=c("blue","red"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA), merge=FALSE )



chic <- read.csv("Chicago_data_absolute_final_hourly.csv")

hist(chic$crime_count, freq = FALSE, main = "Comparison of Crime Count distribution with Poisson and Negative Binomial PMF", xlab = "Crime Count Value (per Hour)",breaks=400,xlim=range(0:12),ylim=range(0,2))

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(chic$crime_count)
x <- seq(0, max(chic$crime_count))
points(x, (dpois(x, lambda)), type = "l", col = "blue")

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(chic$crime_count)
x <- seq(0, max(chic$crime_count))
points(x, (dnbinom(x, size = var(chic$crime_count)/mean(chic$crime_count), mu = lambda)), type = "l", col = "red")

legend( x="topright", 
        legend=c("Poisson","Negative Binomial"), 
        col=c("blue","red"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA), merge=FALSE )

################################################################################ Checking for multicollinearity between month and date ################################################################################ 

cramer_v <- assocstats(table(chic$date, chic$month))$cramer

print(cramer_v)

################################################################################ Generating EDA Graphs ################################################################################ 

a <- read.csv("crimecountbytypeanddayofyear.csv")
b <- read.csv("crimecountbytypeandhourofday.csv")
c <- read.csv("crimecountbydayofyearbeforefixinganomalies.csv")
d <- read.csv("crimecountbydayofyearafterfixinganomalies.csv")
e <- read.csv("crimecountbyhourofday.csv")
f <- read.csv("daylengthbydayofyear.csv")
g <- read.csv("demographics.csv")
h <- read.csv("crimecountbydistrict.csv")
i <- read.csv("crimeratebyyear.csv")
j <- read.csv("crimeratebydayofweek.csv")
k <- read.csv("crimeratebymonth.csv")



ggplot(data = a, aes(x = day_of_year)) +
  geom_line(aes(y = Theft, color = "Theft"), size = 0.75) +
  geom_line(aes(y = Narcotics, color = "Narcotics"), size = 0.75) +
  geom_line(aes(y = Motor.Vehicle.Theft, color = "Motor.Vehicle.Theft"), size = 0.75) +
  geom_line(aes(y = Battery, color = "Battery"), size = 0.75) +
  geom_line(aes(y = Assault, color = "Assault"), size = 0.75) +
  geom_line(aes(y = Prostitution, color = "Prostitution"), size = 0.75) +
  geom_line(aes(y = Robbery, color = "Robbery"), size = 0.75) +
  geom_line(aes(y = Gambling, color = "Gambling"), size = 0.75) +
  labs(x = "Day of Year", y = "Crime Count", title = "Crime Count per Day of Year for each Crime Type", subtitle = "Given crime counts taken over all years",
       color = "Crime Type") +
  scale_color_manual(values = c("Theft" = "purple", "Narcotics" = "brown", "Motor.Vehicle.Theft" = "cornsilk4", "Battery" = "violetred", "Assault" = "grey", "Prostitution" = "chartreuse4", "Robbery" = "darkolivegreen3", "Gambling" = "gold3")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(20, 365, by = 40))



ggplot(data = b, aes(x = Dates)) +
  geom_line(aes(y = Theft, color = "Theft"), size = 0.75) +
  geom_line(aes(y = Narcotics, color = "Narcotics"), size = 0.75) +
  geom_line(aes(y = Motor.Vehicle.Theft, color = "Motor.Vehicle.Theft"), size = 0.75) +
  geom_line(aes(y = Battery, color = "Battery"), size = 0.75) +
  geom_line(aes(y = Assault, color = "Assault"), size = 0.75) +
  geom_line(aes(y = Prostitution, color = "Prostitution"), size = 0.75) +
  geom_line(aes(y = Robbery, color = "Robbery"), size = 0.75) +
  geom_line(aes(y = Gambling, color = "Gambling"), size = 0.75) +
  labs(x = "Hour of Day", y = "Crime Count", title = "Crime Count per Hour of Day for each Crime Type", subtitle = "Given crime counts taken over all years",
       color = "Crime Type") +
  scale_color_manual(values = c("Theft" = "purple", "Narcotics" = "brown", "Motor.Vehicle.Theft" = "cornsilk4", "Battery" = "violetred", "Assault" = "grey", "Prostitution" = "chartreuse4", "Robbery" = "darkolivegreen3", "Gambling" = "gold3")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, 1))



ggplot(data = e, aes(x = hour_of_day)) +
  geom_line(aes(y = Count..year., color = "Theft"), size = 1.5) +
  labs(x = "Hour of Day", y = "Crime Count", title = "Crime Count per Hour of Day", subtitle = "Given crime counts taken over all years") +
  scale_color_manual(values = c("Theft" = "#424242")) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 23, 1))



ggplot(c, aes(x = seq(1,366), y = Count..year.)) +
  geom_point() +
  labs(x = "Day of Year", y = "Crime Count", title = "Crime Count per Day of Year (before correction of anomalies)", subtitle = "Given crime counts taken over all years") +
  scale_x_continuous(breaks = seq(0, 366, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2000))



ggplot(d, aes(x = seq(1,366), y = Count..year.)) +
  geom_point() +
  labs(x = "Day of Year", y = "Crime Count", title = "Crime Count per Day of Year (after correction of anomalies)", subtitle = "Given crime counts taken over all years") +
  scale_x_continuous(breaks = seq(0, 366, 10)) +
  scale_y_continuous(breaks = seq(0, 15000, 2000))



ggplot(g, aes(x = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25), y = black.., shape = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25))) +
  geom_point(aes(y = Crime_rate), size = 3) +
  labs(x = "Variable C", y = "Variable A/B", title = "Scatter Plot of A and B by Variable C") +
  scale_shape_manual(values = c("X" = 16, "Y" = 17, "Z" = 18)) +
  theme_minimal()



ggplot(data.frame(x = 100 * g$black.., y = g$Crime_rate), aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(x = "Black Population %", y = "Crime Rate", title = "Crime Rate against Black Population Percentage", subtitle = "Crime rate measured as reported crimes per capita over all recorded years") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 4, 0.5))



ggplot(data = data.frame(x = dem$district, y = dem$Crime_rate), aes(x = x, y = y)) +
  geom_point() +
  labs(x = "District", y = "Crime Rate", title = "Crime Rate against District", subtitle = "Crime rate measured as reported crimes per capita over all recorded years") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25))


ggplot(data = i, aes(x = year)) +
  geom_line(aes(y = crimes_per_day, color = "line"), size = 1.5) +
  labs(x = "Year", y = "Crime Rate", title = "Crime Rate against Year", subtitle = "Crime rate measured as reported crimes per day over all recorded years") +
  scale_color_manual(values = c("line" = "#424242")) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(2001, 2016, 1)) +
  scale_y_continuous(breaks = seq(0, 1000, 100), limits = c(0, 1000))



ggplot(data = k, aes(x = month, y = crime_rate_per_month, fill = "line")) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = crime_rate_per_month - 1.96 * se, ymax = crime_rate_per_month + 1.96 * se), width = 0.2, position = position_dodge(0.6), color = "cornflowerblue") +
  labs(x = "Month", y = "Crime Rate", title = "Crime Rate for each Month", subtitle = "Crime rate measured as reported crimes per month per year",
       fill = "Crime Type") +
  scale_fill_manual(values = c("line" = "#424242")) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(0, 28000, 2000), limits = c(0, 28000))



ggplot(data = j, aes(x = c(1, 2, 3, 4, 5, 6, 7), y = crime_rate, fill = "line")) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = crime_rate - 1.96 * se, ymax = crime_rate + 1.96 * se), width = 0.2, position = position_dodge(0.6), color = "cornflowerblue") +
  labs(x = "Day of Week", y = "Crime Rate", title = "Crime Rate for each Day of Week", subtitle = "Crime rate measured as reported crimes per day of week per year",
       fill = "Crime Type") +
  scale_fill_manual(values = c("line" = "#424242")) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 40000, 2000), limits = c(0, 40000))
