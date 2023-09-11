################################################################################ Creating Simulation Functions ################################################################################

# Creating data frame of all time points in a year
timepoints <- chic[chic$year == 2015, ][-c(2,7)]



# Getting regular forecast
get_model_forecast <- function(model){
  
  myvec <- c()
  
  for (i in seq(1,length(timepoints$month))){
    myvec <- append(myvec, predict(model, newdata = data.frame(hour = factor(timepoints$hour_of_day[i]), district = factor(timepoints$district[i]), month = factor(timepoints$month[i]), weekday = factor(timepoints$day_of_week[i])), type = "response"))
  }

  return(myvec)
}



# Getting biased forecast
get_biased_forecast <- function(model, alpha){
  
  myvec <- c()
  timepoints$forecast <- get_model_forecast(model)
  
  for (i in seq(1,length(timepoints$month))){
    district_forecasts <- c()
    district_list <- timepoints[timepoints$date == timepoints$date[i] & timepoints$hour_of_day == timepoints$hour_of_day[i],]
    for (y in 1:nrow(district_list)){
      district_forecasts <- append(district_forecasts, district_list$forecast[y])
    }
    # Modulating forecast to emulate overcompensation
    myvec <- append(myvec, (timepoints$forecast[i] + (timepoints$forecast[i] - mean(district_forecasts))*alpha))
  }
  for (i in seq(length(myvec))){
    if (myvec[i] < 0){
      myvec[i] <- 0.1
    }
  }
  return(myvec)
}



# Getting biased forecast, with confirmation bias included
get_biased_forecast_with_conf_bias <- function(model, alpha, previous_dataset, current_dataset, beta){
  
  myvec <- c()
  timepoints$forecast <- get_model_forecast(model)
  
  percent_diffs <- c()
  
  if (length(previous_dataset$district) == length(chic_five$district)){
    for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)){
      percent_diffs <- append(percent_diffs, (sum(current_dataset[current_dataset$district == unique(current_dataset$district)[i] & as.numeric(current_dataset$year) == max(as.numeric(current_dataset$year)),]$crime_count) - original_average_crime_count_by_district$average_original_crime_count[i])/original_average_crime_count_by_district$average_original_crime_count[i])
    }
  }
  else {
    for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)){
      percent_diffs <- append(percent_diffs, (sum(current_dataset[current_dataset$district == unique(current_dataset$district)[i] & as.numeric(current_dataset$year) == max(as.numeric(current_dataset$year)),]$crime_count) - sum(previous_dataset[previous_dataset$district == unique(previous_dataset$district)[i] & as.numeric(previous_dataset$year) == max(as.numeric(previous_dataset$year)),]$crime_count))/sum(previous_dataset[previous_dataset$district == unique(previous_dataset$district)[i] & as.numeric(previous_dataset$year) == max(as.numeric(previous_dataset$year)),]$crime_count))
    }
  }
  
  # Calculating z-scores
  for (i in seq(1,length(timepoints$month))){
    district_forecasts <- c()
    district_list <- timepoints[timepoints$date == timepoints$date[i] & timepoints$hour_of_day == timepoints$hour_of_day[i],]
    for (y in 1:nrow(district_list)){
      district_forecasts <- append(district_forecasts, district_list$forecast[y])
    }
  # Modulating forecast to emulate overcompensation
    myvec <- append(myvec, (timepoints$forecast[i] + (timepoints$forecast[i] - mean(district_forecasts))*alpha) + (timepoints$forecast[i]*(percent_diffs[which(unique(timepoints$district) == timepoints$district[i])])*beta))
  }
  for (i in seq(length(myvec))){
    if (myvec[i] < 0){
      myvec[i] <- 0.1
    }
  }
  return(myvec)
}



# Getting biased new crime count
get_new_crime_count <- function(model, alpha, data){
  
  myvec <- c()
  forecast <- get_biased_forecast(model, alpha)
  
  # Sampling from Poisson distribution
  for (i in seq(1,length(forecast))){
    myvec <- append(myvec, rpois(1, forecast[i]))
  }
  
  return(myvec)
}



# Getting biased new crime count, with confirmation bias included
get_new_crime_count_with_conf_bias <- function(model, alpha, previous_dataset, current_dataset, beta){
  
  myvec <- c()
  forecast <- get_biased_forecast_with_conf_bias(model, alpha, previous_dataset, current_dataset, beta)
  
  # Sampling from NegBin distribution
  for (i in seq(1,length(forecast))){
    myvec <- append(myvec, rpois(1, forecast[i]))
  }
  
  return(myvec)
}



# Function to add new crime data to training dataset
update_dataset <- function(new_crime_data, current_dataset){
  
  extra_cols <- data.frame(year = rep(max(as.numeric(levels(current_dataset$year))[current_dataset$year]), times = 16060))
  extra_cols$year <- factor(extra_cols$year)
  timepoints_plus_new_crimes <- cbind(timepoints, new_crime_data, extra_cols)
  colnames(timepoints_plus_new_crimes)[6] ="crime_count"
  timepoints_plus_new_crimes$crime_count <- as.integer(timepoints_plus_new_crimes$crime_count)
  timepoints_plus_new_crimes <- timepoints_plus_new_crimes[, c(1,7,2,3,4,5,6)]
  
  return(rbind(current_dataset,timepoints_plus_new_crimes))
}



# Function to add new crime data to training dataset, with effective two-year rolling window
update_dataset_rolling_two <- function(new_crime_data, current_dataset){
  
  if (length(current_dataset$district) == 80388){
    current_dataset$weights <- c(rep(0.1, times = 80388))
  }
  
  generation <- (length(current_dataset$district) - 80388)/16060
  doubling <- c(0.1, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4, 12.8)
  
  extra_cols <- data.frame(year = rep(max(as.numeric(levels(current_dataset$year))[current_dataset$year]) + 1, times = 16060))
  extra_cols$weights <- c(rep(doubling[generation + 2], times = 16060))
  extra_cols$year <- factor(extra_cols$year)
  timepoints_plus_new_crimes <- cbind(timepoints, new_crime_data, extra_cols)
  colnames(timepoints_plus_new_crimes)[6] ="crime_count"
  timepoints_plus_new_crimes$crime_count <- as.integer(timepoints_plus_new_crimes$crime_count)
  timepoints_plus_new_crimes <- timepoints_plus_new_crimes[, c(1,7,2,3,4,5,6,8)]
  new_dataset <- rbind(current_dataset,timepoints_plus_new_crimes)
  # new_dataset <- subset(new_dataset, as.numeric(new_dataset$year) != min(as.numeric(new_dataset$year)))
  
  return(new_dataset)
}



# Function to add new crime data to training dataset and retrain model
update_model_without_weights <- function(updated_dataset){
  
  updated_dataset$district = factor(updated_dataset$district)
  updated_dataset$date = factor(updated_dataset$date)
  updated_dataset$hour_of_day = factor(updated_dataset$hour_of_day)
  updated_dataset$day_of_week = factor(updated_dataset$day_of_week)
  updated_dataset$month = factor(updated_dataset$month)
  updated_dataset$year = factor(updated_dataset$year)
  
  # Create the independent variables
  hour <- unlist(data.frame(updated_dataset$hour_of_day))
  district <- unlist(data.frame(updated_dataset$district))
  month <- unlist(data.frame(updated_dataset$month))
  weekday <- unlist(data.frame(updated_dataset$day_of_week))
  date <- unlist(data.frame(updated_dataset$date))
  
  # Create the dependent variable
  count <- unlist(data.frame(updated_dataset$crime_count))
  
  negbinmodel_new <- glm.nb(count ~ hour + district + month + weekday, data = updated_dataset)
  
  return(negbinmodel_new)
}



# Function to add new crime data to training dataset and retrain model
update_model <- function(updated_dataset){
  
  updated_dataset$district = factor(updated_dataset$district)
  updated_dataset$date = factor(updated_dataset$date)
  updated_dataset$hour_of_day = factor(updated_dataset$hour_of_day)
  updated_dataset$day_of_week = factor(updated_dataset$day_of_week)
  updated_dataset$month = factor(updated_dataset$month)
  updated_dataset$year = factor(updated_dataset$year)
  
  # Create the independent variables
  hour <- unlist(data.frame(updated_dataset$hour_of_day))
  district <- unlist(data.frame(updated_dataset$district))
  month <- unlist(data.frame(updated_dataset$month))
  weekday <- unlist(data.frame(updated_dataset$day_of_week))
  date <- unlist(data.frame(updated_dataset$date))
  
  # Create the dependent variable
  count <- unlist(data.frame(updated_dataset$crime_count))
  
  negbinmodel_new <- glm.nb(count ~ hour + district + month + weekday, data = updated_dataset, weights = weights)
  
  return(negbinmodel_new)
}

################################################################################ Justification for use of Crime Count ################################################################################ 

# Crime rate vs Population size
plot <- ggplot(data = dem, aes(x = population, y = Crime_rate)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "grey") + 
  labs(title = "District Crime Rate against Population Size", subtitle = "Crime rate measured as reported crimes per capita over all recorded years",
       x = "Population Size",
       y = "Crime Rate") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 250000, 25000)) +
  scale_y_continuous(breaks = seq(0, 4, 0.5))
print(plot)


# Crime count vs Population size
plot <- ggplot(data = dem, aes(x = population, y = Crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(title = "District Crime Count against Population Size", subtitle = "Given crime counts taken over all years",
       x = "Population Size",
       y = "Crime Count") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 250000, 25000)) +
  scale_y_continuous(breaks = seq(0, 300000, 25000))
print(plot)


################################################################################ T-test of Crime Count and Forecasted Crime Counts ################################################################################ 

chic <- read.csv("Chicago_data_grouped_by_year.csv")

t.test(next_year$forecast, chic$Mean.crime_count.,alternative = "two.sided", var.equal = TRUE)


################################################################################ Showing Poisson is suitable for sampling ################################################################################ 

hi <- chic[chic$month == 5 & chic$district == 5 & chic$hour_of_day == 1,]$crime_count

hist(hi, freq = FALSE, breaks = 30, main = "Crime Count distribution for given Month, Half-day, and District with Poisson and Negative Binomial PMF", xlab = "Crime Count Value (per Half-day)", xlim=range(0:30),ylim=range(0,0.15))

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(hi)
x <- seq(0, max(hi))
points(x, (dpois(x, lambda)), type = "l", col = "blue")

# Overlay the Poisson PMF with the sample mean as lambda
lambda <- mean(hi)
x <- seq(0, max(hi))
points(x, (dnbinom(x, size = var(hi)/mean(hi), mu = lambda)), type = "l", col = "red")

legend( x="topright", 
        legend=c("Poisson","Negative Binomial"), 
        col=c("blue","red"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA), merge=FALSE )


################################################################################ Checking District Crime Counts Follow a Normal Distribution for Z-scores ################################################################################ 

districts <- read.csv("Chicago_data_grouped_by_district.csv")$Sum.crime_count.

mean_data <- mean(districts)
sd_data <- sd(districts)
x <- seq(min(districts), max(districts), length.out = 100)
pmf <- dnorm(x, mean_data, sd_data)

hist(districts, breaks = 5, probability = TRUE, main = "Histogram of Crime Count Forecast Distribution by District Overlayed with Normal Distribution PMF",
     xlab = "Crime Count Value",xlim=range(50000:300000),ylim=range(0,0.000009))
lines(x, pmf, col = "blue")
legend("topright", legend = c("Crime Count Forecast Density", "Normal Distribution PMF"),
       col = c("black", "blue"), lty = c(NA, 1), pch=c(7,NA))


################################################################################ Plotting Original Crimes vs Null Simulation ################################################################################ 

original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average_five_years.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

null_crime_count <- get_new_crime_count(negbinmodel_five, 0)
null_crime_count_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], null_crime_count = null_crime_count)
null_crime_count_by_district <- as.data.frame(null_crime_count_and_district %>% group_by(district) %>% summarise(total_count = sum(null_crime_count)))



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "Original", linetype = "Original")) +
  geom_line(data = null_crime_count_by_district, aes(x = district, y = total_count, color = "Null Simulation", linetype = "Null Simulation")) +
  labs(title = "District Average Crime Count per Year for Original Dataset vs Null Simulation Year", subtitle = "Five years of original dataset (2012 - 2016), null condition of 'α' = 0",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(values = c("Original" = "black", "Null Simulation" = "blue"), 
                     labels = c("Null Simulation", "Original")) +
  scale_linetype_manual(values = c("Original" = "solid", "Null Simulation" = "dashed"),
                        labels = c("Null Simulation", "Original")) +
  guides(color = guide_legend(title = ""),
         linetype = guide_legend(title = "")) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(null_crime_count_by_district$total_count), by = 2000))
print(plot)


# Carrying out a t-test
t.test(original_average_crime_count_by_district$average_original_crime_count, null_crime_count_by_district$total_count)

################################################################################ Checking Simulated Data is Similar to Original Data ################################################################################ 

hist(chic$crime_count, freq = FALSE, main = "Histogram of Original Dataset Crime Count Distribution", xlab = "Crime Count Value",breaks=400,xlim=range(0:60),ylim=range(0,0.06))
hist(round(next_year$forecast), freq = FALSE, main = "Histogram of Forecasted Crime Count Distribution", xlab = "Crime Count Value", breaks=50,xlim=range(0:60),ylim=range(0,0.1))
hist(null_crime_count, freq = FALSE, main = "Histogram of Simulated 'Next Year' Crime Count Distribution", xlab = "Crime Count Value",breaks=50,xlim=range(0:60),ylim=range(0,0.06))
min(next_year$forecast)

freq_table_chic <- table(chic$crime_count)
chic_mode <- as.numeric(names(freq_table_chic[freq_table_chic == max(freq_table_chic)]))
mean(chic$crime_count)
chic_mode
median(chic$crime_count)

freq_table_next_year <- table(null_crime_count)
next_year_mode <- as.numeric(names(freq_table_next_year[freq_table_next_year == max(freq_table_next_year)]))
mean(null_crime_count)
next_year_mode
median(null_crime_count)

# t-test of means (kind of made up the original means)
t.test(c(15.543, 15.635, 15.574, 15.060, 15.647), c(15.502, 15.585, 15.534, 15.566, 15.607))



################################################################################ Running Simulations, Just Compensation, three levels of alpha ################################################################################ 

new_crime_count_alpha_0.05 <- get_new_crime_count(negbinmodel_five, 0.05, chic_five)

new_crime_count_alpha_0.1 <- get_new_crime_count(negbinmodel_five, 0.1, chic_five)

new_crime_count_alpha_0.2 <- get_new_crime_count(negbinmodel_five, 0.2, chic_five)

################################################################################ Running Simulations, no rolling window, Just Compensation ################################################################################ 

new_crime_count_gen_I <- get_new_crime_count(negbinmodel, 0.2, chic)

updated_dataset_gen_I <- update_dataset(new_crime_count_gen_I, chic)

updated_model_gen_I <- update_model_without_weights(updated_dataset_gen_I)



new_crime_count_gen_II <- get_new_crime_count(updated_model_gen_I, 0.2, updated_dataset_gen_I)

updated_dataset_gen_II <- update_dataset(new_crime_count_gen_I, updated_dataset_gen_I)

updated_model_gen_II <- update_model_without_weights(updated_dataset_gen_II)



new_crime_count_gen_III <- get_new_crime_count(updated_model_gen_II, 0.2, updated_dataset_gen_II)

updated_dataset_gen_III <- update_dataset(new_crime_count_gen_II, updated_dataset_gen_II)

updated_model_gen_III <- update_model_without_weights(updated_dataset_gen_III)



new_crime_count_gen_IV <- get_new_crime_count(updated_model_gen_III, 0.2, updated_dataset_gen_III)

updated_dataset_gen_IV <- update_dataset(new_crime_count_gen_III, updated_dataset_gen_III)

updated_model_gen_IV <- update_model_without_weights(updated_dataset_gen_IV)



new_crime_count_gen_V <- get_new_crime_count(updated_model_gen_IV, 0.2, updated_dataset_gen_IV)

updated_dataset_gen_V <- update_dataset(new_crime_count_gen_IV, updated_dataset_gen_IV)

updated_model_gen_V <- update_model_without_weights(updated_dataset_gen_V)



new_crime_count_gen_VI <- get_new_crime_count(updated_model_gen_V, 0.2, updated_dataset_gen_V)

updated_dataset_gen_VI <- update_dataset(new_crime_count_gen_V, updated_dataset_gen_V)

updated_model_gen_VI <- update_model_without_weights(updated_dataset_gen_VI)



new_crime_count_gen_VII <- get_new_crime_count(updated_model_gen_VI, 0.2, updated_dataset_gen_VI)

updated_dataset_gen_VII <- update_dataset(new_crime_count_gen_VI, updated_dataset_gen_VI)

updated_model_gen_VII <- update_model_without_weights(updated_dataset_gen_VII)




new_crime_count_gen_VIII <- get_new_crime_count(updated_model_gen_VII, 0.2, updated_dataset_gen_VII)

################################################################################ Running Simulations, rolling window, Just Compensation ################################################################################ 

new_crime_count_two_year_gen_I <- get_new_crime_count(negbinmodel_five, 0.1, chic_five)

updated_dataset_two_year_gen_I <- update_dataset_rolling_two(new_crime_count_two_year_gen_I, chic_five)

updated_model_two_year_gen_I <- update_model(updated_dataset_two_year_gen_I)



new_crime_count_two_year_gen_II <- get_new_crime_count(updated_model_two_year_gen_I, 0.1, updated_dataset_two_year_gen_I)

updated_dataset_two_year_gen_II <- update_dataset_rolling_two(new_crime_count_two_year_gen_II, updated_dataset_two_year_gen_I)

updated_model_two_year_gen_II <- update_model(updated_dataset_two_year_gen_II)



new_crime_count_two_year_gen_III <- get_new_crime_count(updated_model_two_year_gen_II, 0.1, updated_dataset_two_year_gen_II)

updated_dataset_two_year_gen_III <- update_dataset_rolling_two(new_crime_count_two_year_gen_III, updated_dataset_two_year_gen_II)

updated_model_two_year_gen_III <- update_model(updated_dataset_two_year_gen_III)



new_crime_count_two_year_gen_IV <- get_new_crime_count(updated_model_two_year_gen_III, 0.1, updated_dataset_two_year_gen_III)

updated_dataset_two_year_gen_IV <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV, updated_dataset_two_year_gen_III)

updated_model_two_year_gen_IV <- update_model(updated_dataset_two_year_gen_IV)



new_crime_count_two_year_gen_V <- get_new_crime_count(updated_model_two_year_gen_IV, 0.1, updated_dataset_two_year_gen_IV)

updated_dataset_two_year_gen_V <- update_dataset_rolling_two(new_crime_count_two_year_gen_V, updated_dataset_two_year_gen_IV)

updated_model_two_year_gen_V <- update_model(updated_dataset_two_year_gen_V)



new_crime_count_two_year_gen_VI <- get_new_crime_count(updated_model_two_year_gen_V, 0.1, updated_dataset_two_year_gen_V)

updated_dataset_two_year_gen_VI <- update_dataset_rolling_two(new_crime_count_two_year_gen_VI, updated_dataset_two_year_gen_V)

updated_model_two_year_gen_VI <- update_model(updated_dataset_two_year_gen_VI)



new_crime_count_two_year_gen_VII <- get_new_crime_count(updated_model_two_year_gen_VI, 0.1, updated_dataset_two_year_gen_VI)

updated_dataset_two_year_gen_VII <- update_dataset_rolling_two(new_crime_count_two_year_gen_VII, updated_dataset_two_year_gen_VI)

updated_model_two_year_gen_VII <- update_model(updated_dataset_two_year_gen_VII)



new_crime_count_two_year_gen_VIII <- get_new_crime_count(updated_model_two_year_gen_VII, 0.1, updated_dataset_two_year_gen_VII)

################################################################################ Running Simulations, rolling window, Confirmation Bias, beta = 0.25, 0.5, and 1 ################################################################################ 

### Beta = 0.25

new_crime_count_two_year_gen_I_conf <- get_new_crime_count(negbinmodel_five, 0.1, chic_five)

updated_dataset_two_year_gen_I_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_conf, chic_five)

updated_model_two_year_gen_I_conf <- update_model(updated_dataset_two_year_gen_I_conf)



new_crime_count_two_year_gen_II_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_I_conf, 0.1, chic_five, updated_dataset_two_year_gen_I_conf, 0.25)

updated_dataset_two_year_gen_II_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_conf, updated_dataset_two_year_gen_I_conf)

updated_model_two_year_gen_II_conf <- update_model(updated_dataset_two_year_gen_II_conf)



new_crime_count_two_year_gen_III_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_II_conf, 0.1, updated_dataset_two_year_gen_I_conf, updated_dataset_two_year_gen_II_conf, 0.25)

updated_dataset_two_year_gen_III_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_conf, updated_dataset_two_year_gen_II_conf)

updated_model_two_year_gen_III_conf <- update_model(updated_dataset_two_year_gen_III_conf)



new_crime_count_two_year_gen_IV_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_III_conf, 0.1, updated_dataset_two_year_gen_II_conf, updated_dataset_two_year_gen_III_conf, 0.25)

updated_dataset_two_year_gen_IV_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_conf, updated_dataset_two_year_gen_III_conf)

updated_model_two_year_gen_IV_conf <- update_model(updated_dataset_two_year_gen_IV_conf)



new_crime_count_two_year_gen_V_conf_beta_0.25 <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_IV_conf, 0.1, updated_dataset_two_year_gen_III_conf, updated_dataset_two_year_gen_IV_conf, 0.25)



### Beta = 0.5

new_crime_count_two_year_gen_I_conf <- get_new_crime_count(negbinmodel_five, 0.1, chic_five)

updated_dataset_two_year_gen_I_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_conf, chic_five)

updated_model_two_year_gen_I_conf <- update_model(updated_dataset_two_year_gen_I_conf)



new_crime_count_two_year_gen_II_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_I_conf, 0.1, chic_five, updated_dataset_two_year_gen_I_conf, 0.5)

updated_dataset_two_year_gen_II_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_conf, updated_dataset_two_year_gen_I_conf)

updated_model_two_year_gen_II_conf <- update_model(updated_dataset_two_year_gen_II_conf)



new_crime_count_two_year_gen_III_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_II_conf, 0.1, updated_dataset_two_year_gen_I_conf, updated_dataset_two_year_gen_II_conf, 0.5)

updated_dataset_two_year_gen_III_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_conf, updated_dataset_two_year_gen_II_conf)

updated_model_two_year_gen_III_conf <- update_model(updated_dataset_two_year_gen_III_conf)



new_crime_count_two_year_gen_IV_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_III_conf, 0.1, updated_dataset_two_year_gen_II_conf, updated_dataset_two_year_gen_III_conf, 0.5)

updated_dataset_two_year_gen_IV_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_conf, updated_dataset_two_year_gen_III_conf)

updated_model_two_year_gen_IV_conf <- update_model(updated_dataset_two_year_gen_IV_conf)



new_crime_count_two_year_gen_V_conf_beta_0.5 <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_IV_conf, 0.1, updated_dataset_two_year_gen_III_conf, updated_dataset_two_year_gen_IV_conf, 0.5)



### Beta = 1

new_crime_count_two_year_gen_I_conf <- get_new_crime_count(negbinmodel_five, 0.1, chic_five)

updated_dataset_two_year_gen_I_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_conf, chic_five)

updated_model_two_year_gen_I_conf <- update_model(updated_dataset_two_year_gen_I_conf)



new_crime_count_two_year_gen_II_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_I_conf, 0.1, chic_five, updated_dataset_two_year_gen_I_conf, 1)

updated_dataset_two_year_gen_II_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_conf, updated_dataset_two_year_gen_I_conf)

updated_model_two_year_gen_II_conf <- update_model(updated_dataset_two_year_gen_II_conf)



new_crime_count_two_year_gen_III_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_II_conf, 0.1, updated_dataset_two_year_gen_I_conf, updated_dataset_two_year_gen_II_conf, 1)

updated_dataset_two_year_gen_III_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_conf, updated_dataset_two_year_gen_II_conf)

updated_model_two_year_gen_III_conf <- update_model(updated_dataset_two_year_gen_III_conf)



new_crime_count_two_year_gen_IV_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_III_conf, 0.1, updated_dataset_two_year_gen_II_conf, updated_dataset_two_year_gen_III_conf, 1)

updated_dataset_two_year_gen_IV_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_conf, updated_dataset_two_year_gen_III_conf)

updated_model_two_year_gen_IV_conf <- update_model(updated_dataset_two_year_gen_IV_conf)



new_crime_count_two_year_gen_V_conf_beta_1 <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_IV_conf, 0.1, updated_dataset_two_year_gen_III_conf, updated_dataset_two_year_gen_IV_conf, 1)


################################################################################ Running Simulations, rolling window, Confirmation Bias, eight years ################################################################################ 

new_crime_count_two_year_gen_I_conf <- get_new_crime_count(negbinmodel_five, 0.2, chic_five)

updated_dataset_two_year_gen_I_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_conf, chic_five)

updated_model_two_year_gen_I_conf <- update_model(updated_dataset_two_year_gen_I_conf)



new_crime_count_two_year_gen_II_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_I_conf, 0.2, chic_five, updated_dataset_two_year_gen_I_conf, 1)

updated_dataset_two_year_gen_II_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_conf, updated_dataset_two_year_gen_I_conf)

updated_model_two_year_gen_II_conf <- update_model(updated_dataset_two_year_gen_II_conf)



new_crime_count_two_year_gen_III_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_II_conf, 0.2, updated_dataset_two_year_gen_I_conf, updated_dataset_two_year_gen_II_conf, 1)

updated_dataset_two_year_gen_III_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_conf, updated_dataset_two_year_gen_II_conf)

updated_model_two_year_gen_III_conf <- update_model(updated_dataset_two_year_gen_III_conf)



new_crime_count_two_year_gen_IV_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_III_conf, 0.2, updated_dataset_two_year_gen_II_conf, updated_dataset_two_year_gen_III_conf, 1)

updated_dataset_two_year_gen_IV_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_conf, updated_dataset_two_year_gen_III_conf)

updated_model_two_year_gen_IV_conf <- update_model(updated_dataset_two_year_gen_IV_conf)



new_crime_count_two_year_gen_V_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_IV_conf, 0.2, updated_dataset_two_year_gen_III_conf, updated_dataset_two_year_gen_IV_conf, 1)

updated_dataset_two_year_gen_V_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_V_conf, updated_dataset_two_year_gen_IV_conf)

updated_model_two_year_gen_V_conf <- update_model(updated_dataset_two_year_gen_V_conf)



new_crime_count_two_year_gen_VI_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_V_conf, 0.2, updated_dataset_two_year_gen_IV_conf, updated_dataset_two_year_gen_V_conf, 1)

updated_dataset_two_year_gen_VI_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_VI_conf, updated_dataset_two_year_gen_V_conf)

updated_model_two_year_gen_VI_conf <- update_model(updated_dataset_two_year_gen_VI_conf)



new_crime_count_two_year_gen_VII_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_VI_conf, 0.2, updated_dataset_two_year_gen_V_conf, updated_dataset_two_year_gen_VI_conf, 1)

updated_dataset_two_year_gen_VII_conf <- update_dataset_rolling_two(new_crime_count_two_year_gen_VII_conf, updated_dataset_two_year_gen_VI_conf)

updated_model_two_year_gen_VII_conf <- update_model(updated_dataset_two_year_gen_VII_conf)



new_crime_count_two_year_gen_VIII_conf <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_VII_conf, 0.2, updated_dataset_two_year_gen_VI_conf, updated_dataset_two_year_gen_VII_conf, 1)

################################################################################ Plotting Just Compensation, three levels of alpha ################################################################################ 

new_crime_count_alpha_0.05_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_alpha_0.05)
new_crime_count_alpha_0.1_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_alpha_0.1)
new_crime_count_alpha_0.2_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_alpha_0.2)

new_crime_count_alpha_0.05_by_district <- as.data.frame(new_crime_count_alpha_0.05_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_alpha_0.1_by_district <- as.data.frame(new_crime_count_alpha_0.1_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_alpha_0.2_by_district <- as.data.frame(new_crime_count_alpha_0.2_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))

original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average_five_years.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_alpha_0.05_by_district, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_alpha_0.1_by_district, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_alpha_0.2_by_district, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  labs(title = "District Average Crime Count per Year for Original Dataset vs Simulated Crime Counts for Three Levels of 'α'", subtitle = "Five years of original dataset (2012 - 2016), showing simulated year for 'α' = 0.05, 0.1, and 0.2",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "chartreuse3", "orange", "red2"),
    labels = c("Original", "α = 0.05", "α = 0.1", "α = 0.2"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_alpha_0.2_by_district$total_count), by = 0.02))
print(plot)

################################################################################ Plotting Just Compensation, eight years, no rolling window ################################################################################ 

original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

new_crime_count_gen_I_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_I)
new_crime_count_gen_II_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_II)
new_crime_count_gen_III_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_III)
new_crime_count_gen_IV_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_IV)
new_crime_count_gen_V_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_V)
new_crime_count_gen_VI_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_VI)
new_crime_count_gen_VII_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_VII)
new_crime_count_gen_VIII_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_gen_VIII)

new_crime_count_gen_I_by_district <- as.data.frame(new_crime_count_gen_I_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_II_by_district <- as.data.frame(new_crime_count_gen_II_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_III_by_district <- as.data.frame(new_crime_count_gen_III_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_IV_by_district <- as.data.frame(new_crime_count_gen_IV_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_V_by_district <- as.data.frame(new_crime_count_gen_V_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_VI_by_district <- as.data.frame(new_crime_count_gen_VI_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_VII_by_district <- as.data.frame(new_crime_count_gen_VII_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_gen_VIII_by_district <- as.data.frame(new_crime_count_gen_VIII_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))

plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_gen_II_by_district, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_gen_IV_by_district, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_gen_VI_by_district, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  geom_line(data = new_crime_count_gen_VIII_by_district, aes(x = district, y = total_count, color = "purple"), linetype = "dashed") +
  labs(title = "District Average Crime Count per Year for Original Dataset vs Eight Generations of Simulated Crime Counts", subtitle = "Full original dataset, full model, no rolling window or weightings, showing every other simulated year for 'α' = 0.2",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "green", "orange", "purple", "red2"),
    labels = c("Original", "Gen II", "Gen IV", "Gen VI", "Gen VIII"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_gen_VIII_by_district$total_count), by = 2000))
print(plot)

################################################################################ Plotting Just Compensation, eight years, rolling window ################################################################################ 

dem <- read.csv("demographics_regression.csv")
district_pops <- dem$population
original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average_five_years.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

new_crime_count_two_year_gen_I_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_I)
new_crime_count_two_year_gen_II_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II)
new_crime_count_two_year_gen_III_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_III)
new_crime_count_two_year_gen_IV_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_IV)
new_crime_count_two_year_gen_V_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V)
new_crime_count_two_year_gen_VI_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VI)
new_crime_count_two_year_gen_VII_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VII)
new_crime_count_two_year_gen_VIII_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII)

new_crime_count_two_year_gen_I_by_district <- as.data.frame(new_crime_count_two_year_gen_I_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_II_by_district <- as.data.frame(new_crime_count_two_year_gen_II_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_III_by_district <- as.data.frame(new_crime_count_two_year_gen_III_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_IV_by_district <- as.data.frame(new_crime_count_two_year_gen_IV_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district <- as.data.frame(new_crime_count_two_year_gen_V_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VI_by_district <- as.data.frame(new_crime_count_two_year_gen_VI_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VII_by_district <- as.data.frame(new_crime_count_two_year_gen_VII_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VIII_by_district <- as.data.frame(new_crime_count_two_year_gen_VIII_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_II_by_district, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_IV_by_district, aes(x = district, y = total_count, color = "blue"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_VI_by_district, aes(x = district, y = total_count, color = "brown"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_VIII_by_district, aes(x = district, y = total_count, color = "cyan"), linetype = "dashed") +
  labs(title = "District Average Crime Count per Year for Original Dataset vs Eight Generations of Simulated Crime Counts", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window model, showing every other simulated year for 'α' = 0.2",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "orange", "red2", "purple", "chartreuse3"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_VIII_by_district$total_count), by = 2000))
print(plot)

################################################################################ Plotting Confirmation bias, three levels of beta (for alpha = 0.1) ################################################################################ 

new_crime_count_two_year_gen_V_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V)
new_crime_count_two_year_gen_V_and_district_conf_beta_0.25 <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf_beta_0.25)
new_crime_count_two_year_gen_V_and_district_conf_beta_0.5 <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf_beta_0.5)
new_crime_count_two_year_gen_V_and_district_conf_beta_1 <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf_beta_1)

new_crime_count_two_year_gen_V_by_district <- as.data.frame(new_crime_count_two_year_gen_V_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district_conf_beta_0.25 <- as.data.frame(new_crime_count_two_year_gen_V_and_district_conf_beta_0.25 %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district_conf_beta_0.5 <- as.data.frame(new_crime_count_two_year_gen_V_and_district_conf_beta_0.5 %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district_conf_beta_1 <- as.data.frame(new_crime_count_two_year_gen_V_and_district_conf_beta_1 %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district, aes(x = district, y = total_count, color = "grey"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district_conf_beta_0.25, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district_conf_beta_0.5, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district_conf_beta_1, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  
  labs(title = "District Crime Counts of the Same Simulated Year (Gen V) for Three Levels of 'β', with 'β = 0' as Baseline", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window model, showing Gen V for 'β' = 0.25, 0.5, and 1, for 'α' = 0.1",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("grey", "chartreuse3", "black", "orange", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dotted", "dashed", "solid", "dotted")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "solid", "dotted")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_V_by_district_conf_beta_1$total_count), by = 2000))
print(plot)


################################################################################ Plotting Confirmation Bias vs Just Compensation, Gens I, V, and VIII (for alpha = 0.1) ################################################################################ 

original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average_five_years.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

new_crime_count_two_year_gen_II_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II)
new_crime_count_two_year_gen_V_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V)
new_crime_count_two_year_gen_VIII_and_district <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII)

new_crime_count_two_year_gen_II_by_district <- as.data.frame(new_crime_count_two_year_gen_II_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district <- as.data.frame(new_crime_count_two_year_gen_V_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VIII_by_district <- as.data.frame(new_crime_count_two_year_gen_VIII_and_district %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))

new_crime_count_two_year_gen_II_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II_conf)
new_crime_count_two_year_gen_V_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf)
new_crime_count_two_year_gen_VIII_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII_conf)

new_crime_count_two_year_gen_II_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_II_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_V_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VIII_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_VIII_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_II_by_district, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_VIII_by_district, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_II_by_district_conf, aes(x = district, y = total_count, color = "blue"), linetype = "dotted") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district_conf, aes(x = district, y = total_count, color = "purple"), linetype = "dotted") +
  geom_line(data = new_crime_count_two_year_gen_VIII_by_district_conf, aes(x = district, y = total_count, color = "brown"), linetype = "dotted") +
  labs(title = "Comparison of Three Simulated Years, Overcompensation-only vs Overcompensation Plus Confirmation Bias", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window model, showing Gens II, V, and VIII for 'α' = 0.1 and 'β' = 0.5",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "chartreuse3", "red2", "chartreuse3", "orange", "orange", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I", "Gen VI", "Gen III"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dotted", "dashed", "dotted", "dashed", "dotted", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dotted", "dashed", "dotted", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_VIII_by_district_conf$total_count), by = 2000))
print(plot)

################################################################################ Plotting Confirmation bias, eight Years ################################################################################ 

dem <- read.csv("demographics_regression.csv")
district_pops <- dem$population
original_average_crime_count_by_district <- read.csv("Original_crimes_per_district_average_five_years.csv")
original_average_crime_count_by_district$average_original_crime_count <- round(original_average_crime_count_by_district$average_original_crime_count)

new_crime_count_two_year_gen_I_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_I_conf)
new_crime_count_two_year_gen_II_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II_conf)
new_crime_count_two_year_gen_III_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_III_conf)
new_crime_count_two_year_gen_IV_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_IV_conf)
new_crime_count_two_year_gen_V_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf)
new_crime_count_two_year_gen_VI_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VI_conf)
new_crime_count_two_year_gen_VII_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VII_conf)
new_crime_count_two_year_gen_VIII_and_district_conf <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII_conf)

new_crime_count_two_year_gen_I_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_I_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_II_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_II_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_III_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_III_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_IV_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_IV_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_V_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_V_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VI_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_VI_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VII_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_VII_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_two_year_gen_VIII_by_district_conf <- as.data.frame(new_crime_count_two_year_gen_VIII_and_district_conf %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = average_original_crime_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_II_by_district_conf, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_IV_by_district_conf, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_VI_by_district_conf, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_VIII_by_district_conf, aes(x = district, y = total_count, color = "purple"), linetype = "dashed") +
  labs(title = "District Average Crime Count per Year for Original Dataset vs Eight Generations of Confirmation Bias Simulations", subtitle = "Five years of original dataset (2012 - 2016), effective two-year rolling window model, showing every other simulated year for 'α' = 0.05 and 'β' = 0.25",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "green", "orange", "purple", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_VIII_by_district_conf$total_count), by = 2000))
print(plot)



### Plotting Per Capita

original_average_crime_count_by_district$per_capita <- original_average_crime_count_by_district$average_original_crime_count / district_pops
new_crime_count_two_year_gen_I_by_district_conf$per_capita <- new_crime_count_two_year_gen_I_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_II_by_district_conf$per_capita <- new_crime_count_two_year_gen_II_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_III_by_district_conf$per_capita <- new_crime_count_two_year_gen_III_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_IV_by_district_conf$per_capita <- new_crime_count_two_year_gen_IV_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_V_by_district_conf$per_capita <- new_crime_count_two_year_gen_V_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_VI_by_district_conf$per_capita <- new_crime_count_two_year_gen_VI_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_VII_by_district_conf$per_capita <- new_crime_count_two_year_gen_VII_by_district_conf$total_count / district_pops
new_crime_count_two_year_gen_VIII_by_district_conf$per_capita <- new_crime_count_two_year_gen_VIII_by_district_conf$total_count / district_pops



plot <- ggplot() +
  geom_line(data = original_average_crime_count_by_district, aes(x = district, y = per_capita, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_two_year_gen_I_by_district_conf, aes(x = district, y = per_capita, color = "green"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_II_by_district_conf, aes(x = district, y = per_capita, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_III_by_district_conf, aes(x = district, y = per_capita, color = "red"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_IV_by_district_conf, aes(x = district, y = per_capita, color = "blue"), linetype = "dashed") +
  geom_line(data = new_crime_count_two_year_gen_V_by_district_conf, aes(x = district, y = per_capita, color = "purple"), linetype = "dotted") +
  geom_line(data = new_crime_count_two_year_gen_VI_by_district_conf, aes(x = district, y = per_capita, color = "brown"), linetype = "dotted") +
  geom_line(data = new_crime_count_two_year_gen_VII_by_district_conf, aes(x = district, y = per_capita, color = "pink"), linetype = "dotted") +
  geom_line(data = new_crime_count_two_year_gen_VIII_by_district_conf, aes(x = district, y = per_capita, color = "cyan"), linetype = "dotted") +
  labs(title = "Annual Average District Crime Count per Capita of Original Dataset vs Eight Generations of Simulated Crime Counts per Capita where 'α' = 0.2, Effective Two-year Rolling Window",
       x = "District",
       y = "Crime Count per Capita per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "purple", "orange", "purple", "chartreuse3", "orange", "red2", "chartreuse3", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I", "Gen VI", "Gen III", "Gen V", "Gen VII"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dotted", "dashed", "dotted", "dashed", "dotted", "dashed", "dotted", "dotted"
    )))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dotted", "dashed", "dotted", "dashed", "dotted", "dashed"
                              )))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_VIII_by_district$per_capita), by = 0.02))
print(plot)


################################################################################ Proof of Concept Simulations and Plotting ################################################################################ 

set.seed(123)

random_lambdas <- runif(22, min = 2, max = 20)

timepoints1 <- timepoints
timepoints2 <- timepoints
timepoints3 <- timepoints
timepoints4 <- timepoints
timepoints5 <- timepoints

for (i in seq(length(timepoints$district))){
  timepoints1$crime_count[i] = rpois(1, random_lambdas[timepoints$district[i]])
}

for (i in seq(length(timepoints$district))){
  timepoints2$crime_count[i] = rpois(1, random_lambdas[timepoints$district[i]])
}

for (i in seq(length(timepoints$district))){
  timepoints3$crime_count[i] = rpois(1, random_lambdas[timepoints$district[i]])
}

for (i in seq(length(timepoints$district))){
  timepoints4$crime_count[i] = rpois(1, random_lambdas[timepoints$district[i]])
}

for (i in seq(length(timepoints$district))){
  timepoints5$crime_count[i] = rpois(1, random_lambdas[timepoints$district[i]])
}



dummy_data <- rbind(timepoints1, timepoints2, timepoints3, timepoints4, timepoints5)

dummy_data <- cbind(dummy_data[, 1], year = rep(2012:2016, each = 16060), dummy_data[, -1])

colnames(dummy_data)[1] <- "district"

dummy_data$district = factor(dummy_data$district)
dummy_data$date = factor(dummy_data$date)
dummy_data$hour_of_day = factor(dummy_data$hour_of_day)
dummy_data$day_of_week = factor(dummy_data$day_of_week)
dummy_data$month = factor(dummy_data$month)
dummy_data$year = factor(dummy_data$year)

hour <- unlist(data.frame(dummy_data$hour_of_day))
district <- unlist(data.frame(dummy_data$district))
month <- unlist(data.frame(dummy_data$month))
weekday <- unlist(data.frame(dummy_data$day_of_week))
date <- unlist(data.frame(dummy_data$date))

count <- unlist(data.frame(dummy_data$crime_count))

sampled_rows <- dummy_data[sample(nrow(dummy_data), 88),]
dummy_data <- rbind(dummy_data, sampled_rows)



negbinmodel_concept <- glm.nb(count ~ hour + district + month + weekday, data = dummy_data)



new_crime_count_two_year_gen_I_concept <- get_new_crime_count(negbinmodel_concept, 0.1, dummy_data)

updated_dataset_two_year_gen_I_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_concept, dummy_data)

updated_model_two_year_gen_I_concept <- update_model(updated_dataset_two_year_gen_I_concept)



new_crime_count_two_year_gen_II_concept <- get_new_crime_count(updated_model_two_year_gen_I_concept, 0.1, updated_dataset_two_year_gen_I_concept)

updated_dataset_two_year_gen_II_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_concept, updated_dataset_two_year_gen_I_concept)

updated_model_two_year_gen_II_concept <- update_model(updated_dataset_two_year_gen_II_concept)



new_crime_count_two_year_gen_III_concept <- get_new_crime_count(updated_model_two_year_gen_II_concept, 0.1, updated_dataset_two_year_gen_II_concept)

updated_dataset_two_year_gen_III_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_concept, updated_dataset_two_year_gen_II_concept)

updated_model_two_year_gen_III_concept <- update_model(updated_dataset_two_year_gen_III_concept)



new_crime_count_two_year_gen_IV_concept <- get_new_crime_count(updated_model_two_year_gen_III_concept, 0.1, updated_dataset_two_year_gen_III_concept)

updated_dataset_two_year_gen_IV_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_concept, updated_dataset_two_year_gen_III_concept)

updated_model_two_year_gen_IV_concept <- update_model(updated_dataset_two_year_gen_IV_concept)



new_crime_count_two_year_gen_V_concept <- get_new_crime_count(updated_model_two_year_gen_IV_concept, 0.1, updated_dataset_two_year_gen_IV_concept)

updated_dataset_two_year_gen_V_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_V_concept, updated_dataset_two_year_gen_IV_concept)

updated_model_two_year_gen_V_concept <- update_model(updated_dataset_two_year_gen_V_concept)



new_crime_count_two_year_gen_VI_concept <- get_new_crime_count(updated_model_two_year_gen_V_concept, 0.1, updated_dataset_two_year_gen_V_concept)

updated_dataset_two_year_gen_VI_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_VI_concept, updated_dataset_two_year_gen_V_concept)

updated_model_two_year_gen_VI_concept <- update_model(updated_dataset_two_year_gen_VI_concept)



new_crime_count_two_year_gen_VII_concept <- get_new_crime_count(updated_model_two_year_gen_VI_concept, 0.1, updated_dataset_two_year_gen_VI_concept)

updated_dataset_two_year_gen_VII_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_VII_concept, updated_dataset_two_year_gen_VI_concept)

updated_model_two_year_gen_VII_concept <- update_model(updated_dataset_two_year_gen_VII_concept)



new_crime_count_two_year_gen_VIII_concept <- get_new_crime_count(updated_model_two_year_gen_VII_concept, 0.1, updated_dataset_two_year_gen_VII_concept)



new_crime_count_and_district_gen_I_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_I_concept)
new_crime_count_and_district_gen_II_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II_concept)
new_crime_count_and_district_gen_III_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_III_concept)
new_crime_count_and_district_gen_IV_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_IV_concept)
new_crime_count_and_district_gen_V_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_concept)
new_crime_count_and_district_gen_VI_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VI_concept)
new_crime_count_and_district_gen_VII_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VII_concept)
new_crime_count_and_district_gen_VIII_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII_concept)

new_crime_count_by_district_gen_I_concept <- as.data.frame(new_crime_count_and_district_gen_I_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_II_concept <- as.data.frame(new_crime_count_and_district_gen_II_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_III_concept <- as.data.frame(new_crime_count_and_district_gen_III_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_V_concept <- as.data.frame(new_crime_count_and_district_gen_IV_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_V_concept <- as.data.frame(new_crime_count_and_district_gen_V_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VI_concept <- as.data.frame(new_crime_count_and_district_gen_VI_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VII_concept <- as.data.frame(new_crime_count_and_district_gen_VII_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VIII_concept <- as.data.frame(new_crime_count_and_district_gen_VIII_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))

district_total_counts_average <- c()
for (i in unique(dummy_data$district)){
  district_total_counts_average <- append(district_total_counts_average, sum(dummy_data[dummy_data$district == i,]$crime_count/5))
}
total_count_average_by_district <- data.frame(district = c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,22,24,25), total_count = district_total_counts_average)



plot <- ggplot() +
  geom_line(data = total_count_average_by_district, aes(x = district, y = total_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_by_district_gen_II_concept, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_IV_concept, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_VI_concept, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_VIII_concept, aes(x = district, y = total_count, color = "purple"), linetype = "dashed") +
  labs(title = "Proof of Concept District Average Crime Count per Year (Dummy Data) vs Eight Generations of Simulated Crime Counts", subtitle = "Five years of dummy data, effective two-year rolling window model, showing every other simulated year for 'α' = 0.1",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "chartreuse3", "orange", "purple", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VIII", "Gen I"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_two_year_gen_VIII_by_district$total_count), by = 2000))
print(plot)



new_crime_count_two_year_gen_I_conf_concept <- get_new_crime_count(negbinmodel_concept, 0.1, dummy_data)

updated_dataset_two_year_gen_I_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_I_conf_concept, dummy_data)

updated_model_two_year_gen_I_conf_concept <- update_model(updated_dataset_two_year_gen_I_conf_concept)



new_crime_count_two_year_gen_II_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_I_conf_concept, 0.1, dummy_data, updated_dataset_two_year_gen_I_conf_concept, 0.5)

updated_dataset_two_year_gen_II_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_II_conf_concept, updated_dataset_two_year_gen_I_conf_concept)

updated_model_two_year_gen_II_conf_concept <- update_model(updated_dataset_two_year_gen_II_conf_concept)



new_crime_count_two_year_gen_III_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_II_conf_concept, 0.1, updated_dataset_two_year_gen_I_conf_concept, updated_dataset_two_year_gen_II_conf_concept, 0.5)

updated_dataset_two_year_gen_III_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_III_conf_concept, updated_dataset_two_year_gen_II_conf_concept)

updated_model_two_year_gen_III_conf_concept <- update_model(updated_dataset_two_year_gen_III_conf_concept)



new_crime_count_two_year_gen_IV_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_III_conf_concept, 0.1, updated_dataset_two_year_gen_II_conf_concept, updated_dataset_two_year_gen_III_conf_concept, 0.5)

updated_dataset_two_year_gen_IV_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_IV_conf_concept, updated_dataset_two_year_gen_III_conf_concept)

updated_model_two_year_gen_IV_conf_concept <- update_model(updated_dataset_two_year_gen_IV_conf_concept)



new_crime_count_two_year_gen_V_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_IV_conf_concept, 0.1, updated_dataset_two_year_gen_III_conf_concept, updated_dataset_two_year_gen_IV_conf_concept, 0.5)

updated_dataset_two_year_gen_V_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_V_conf_concept, updated_dataset_two_year_gen_IV_conf_concept)

updated_model_two_year_gen_V_conf_concept <- update_model(updated_dataset_two_year_gen_V_conf_concept)



new_crime_count_two_year_gen_VI_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_V_conf_concept, 0.1, updated_dataset_two_year_gen_IV_conf_concept, updated_dataset_two_year_gen_V_conf_concept, 0.5)

updated_dataset_two_year_gen_VI_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_VI_conf_concept, updated_dataset_two_year_gen_V_conf_concept)

updated_model_two_year_gen_VI_conf_concept <- update_model(updated_dataset_two_year_gen_VI_conf_concept)



new_crime_count_two_year_gen_VII_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_VI_conf_concept, 0.1, updated_dataset_two_year_gen_V_conf_concept, updated_dataset_two_year_gen_VI_conf_concept, 0.5)

updated_dataset_two_year_gen_VII_conf_concept <- update_dataset_rolling_two(new_crime_count_two_year_gen_VII_conf_concept, updated_dataset_two_year_gen_VI_conf_concept)

updated_model_two_year_gen_VII_conf_concept <- update_model(updated_dataset_two_year_gen_VII_conf_concept)



new_crime_count_two_year_gen_VIII_conf_concept <- get_new_crime_count_with_conf_bias(updated_model_two_year_gen_VII_conf_concept, 0.1, updated_dataset_two_year_gen_VI_conf_concept, updated_dataset_two_year_gen_VII_conf_concept, 0.5)



new_crime_count_and_district_gen_I_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_I_conf_concept)
new_crime_count_and_district_gen_II_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_II_conf_concept)
new_crime_count_and_district_gen_III_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_III_conf_concept)
new_crime_count_and_district_gen_IV_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_IV_conf_concept)
new_crime_count_and_district_gen_V_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_V_conf_concept)
new_crime_count_and_district_gen_VI_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VI_conf_concept)
new_crime_count_and_district_gen_VII_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VII_conf_concept)
new_crime_count_and_district_gen_VIII_conf_concept <- data.frame(district = as.numeric(levels(timepoints$district))[timepoints$district], new_crime_count = new_crime_count_two_year_gen_VIII_conf_concept)

new_crime_count_by_district_gen_I_conf_concept <- as.data.frame(new_crime_count_and_district_gen_I_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_II_conf_concept <- as.data.frame(new_crime_count_and_district_gen_II_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_III_conf_concept <- as.data.frame(new_crime_count_and_district_gen_III_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_IV_conf_concept <- as.data.frame(new_crime_count_and_district_gen_IV_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_V_conf_concept <- as.data.frame(new_crime_count_and_district_gen_V_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VI_conf_concept <- as.data.frame(new_crime_count_and_district_gen_VI_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VII_conf_concept <- as.data.frame(new_crime_count_and_district_gen_VII_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))
new_crime_count_by_district_gen_VIII_conf_concept <- as.data.frame(new_crime_count_and_district_gen_VIII_conf_concept %>% group_by(district) %>% summarise(total_count = sum(new_crime_count)))



plot <- ggplot() +
  geom_line(data = total_count_average_by_district, aes(x = district, y = total_count, color = "black"), linetype = "solid") +
  geom_line(data = new_crime_count_by_district_gen_II_conf_concept, aes(x = district, y = total_count, color = "chartreuse3"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_IV_conf_concept, aes(x = district, y = total_count, color = "orange"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_VI_conf_concept, aes(x = district, y = total_count, color = "red2"), linetype = "dashed") +
  geom_line(data = new_crime_count_by_district_gen_VIII_conf_concept, aes(x = district, y = total_count, color = "purple"), linetype = "dashed") +
  labs(title = "Proof of Concept District Average Crime Count per Year (Dummy Data) vs Eight Generations of Simulated Crime Counts", subtitle = "Five years of dummy data, effective two-year rolling window model, showing every other simulated year for 'α' = 0.1 and 'β' = 0.5",
       x = "District",
       y = "Crime Count per Year") +
  theme_minimal() +
  scale_color_manual(
    values = c("black", "chartreuse3", "orange", "purple", "red2"),
    labels = c("Original", "Gen IV", "Gen II", "Gen VI", "Gen VIII"),
    guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  guides(color = guide_legend(title = "",
                              override.aes = list(linetype = c("solid", "dashed", "dashed", "dashed", "dashed")))) +
  scale_x_discrete(limits = unique(original_average_crime_count_by_district$district)) +
  scale_y_continuous(breaks = seq(0, max(new_crime_count_by_district_gen_VIII_conf_concept$total_count), by = 2000))
print(plot)
