# Load the data
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

# Create the Poisson regression model
poissonmodel <- glm(count ~ hour + district + month + weekday, data = chic, family = poisson)

summary(poissonmodel)

# Use model to predict crime count
predict(poissonmodel, newdata = data.frame(hour = factor(2), district = factor(1), month = factor(7), weekday = factor(5)), type = "response")
