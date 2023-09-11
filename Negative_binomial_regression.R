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
year = unlist(data.frame(chic$year))

# Create the dependent variable
count <- unlist(data.frame(chic$crime_count))

# Create the Negative Binomial regression model
negbinmodel <- glm.nb(count ~ hour + district + month + weekday, data = chic)

summary(negbinmodel)

# Use model to predict crime count
predict(negbinmodel, newdata = data.frame(hour = factor(2), district = factor(11), month = factor(7), weekday = factor(5)), type = "response")

################################################################################ Creating five-year model ################################################################################ 

chic_five <- subset(chic, chic$year %in% c(2012, 2013, 2014, 2015, 2016))

hour <- unlist(data.frame(chic_five$hour_of_day))
district <- unlist(data.frame(chic_five$district))
month <- unlist(data.frame(chic_five$month))
weekday <- unlist(data.frame(chic_five$day_of_week))
date <- unlist(data.frame(chic_five$date))

count <- unlist(data.frame(chic_five$crime_count))


negbinmodel_five <- glm.nb(count ~ hour + district + month + weekday, data = chic_five)



################################################################################ Creating Model with All Variables Included to Derive Summary Statistics ################################################################################ 

negbinmodel <- glm.nb(count ~ hour + district + month + weekday + date + year, data = chic)

options(max.print=1000000)

summary(negbinmodel)
