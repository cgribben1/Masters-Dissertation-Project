################################################################################  Loading and formatting the data ################################################################################ 

chic <- read.csv("Chicago_data_absolute_final.csv")

chic$district = factor(chic$district)
chic$date = factor(chic$date)
chic$hour_of_day = factor(chic$hour_of_day)
chic$day_of_week = factor(chic$day_of_week)
chic$month = factor(chic$month)
chic$year = factor(chic$year)

hour <- unlist(data.frame(chic$hour_of_day))
district <- unlist(data.frame(chic$district))
month <- unlist(data.frame(chic$month))
weekday <- unlist(data.frame(chic$day_of_week))
date <- unlist(data.frame(chic$date))

count <- unlist(data.frame(chic$crime_count))

################################################################################ Best-subset Selection ################################################################################

bestglm(chic, family=poisson, IC="AIC",method="forward")
bestglm(chic, family=negative.binomial(theta=10.197), IC="AIC",method="forward")

################################################################################  Making models which take into account the Year, for reference in results... ################################################################################ 

year <- unlist(data.frame(chic$year))

yearpoissonmodel <- glm(count ~ hour + district + month + weekday + year, data = chic, family = poisson)
yearnegbinmodel <- glm.nb(count ~ hour + district + month + weekday + year, data = chic)

################################################################################ Comparing Deviance ################################################################################

poissonmodel$deviance
negbinmodel$deviance
yearpoissonmodel$deviance
yearnegbinmodel$deviance

################################################################################ Calculating Deviance Ratios ################################################################################ 

1 - poissonmodel$deviance / poissonmodel$null.deviance
1 - negbinmodel$deviance / negbinmodel$null.deviance
1 - yearpoissonmodel$deviance / yearpoissonmodel$null.deviance
1 - yearnegbinmodel$deviance / yearnegbinmodel$null.deviance
1 - negbinmodel_five$deviance / negbinmodel_five$null.deviance

################################################################################ Calculating AIC and BIC measures ################################################################################ 

AIC(poissonmodel)
AIC(negbinmodel)

BIC(poissonmodel)
BIC(negbinmodel)

AIC(yearpoissonmodel)
BIC(yearpoissonmodel)

AIC(yearnegbinmodel)
BIC(yearnegbinmodel)

################################################################################ Calculating McFadden's pseudo R-squared ################################################################################ 

intercept_model_poisson <- glm(chic$crime_count ~ 1, family = poisson)
intercept_model_negbin <- glm.nb(chic$crime_count ~ 1)

log_likelihood_intercept_poisson <- logLik(intercept_model_poisson)
log_likelihood_intercept_negbin <- logLik(intercept_model_negbin)

log_likelihood_model_poisson <- logLik(poissonmodel)
log_likelihood_model_negbin <- logLik(negbinmodel)

R_squared_McFadden_poisson <- 1 - (log_likelihood_model_poisson / log_likelihood_intercept_poisson)
R_squared_McFadden_negbin <- 1 - (log_likelihood_model_negbin / log_likelihood_intercept_negbin)

R_squared_McFadden_poisson
R_squared_McFadden_negbin

################################################################################ k-fold Cross Validation ################################################################################ 

subchic <- chic[c(1,3,5,6,7)]

cv_control <- trainControl(method = "cv", number = 5)

cv_results_poisson <- train(crime_count ~ ., data = subchic, method = "glm", family = poisson (link = "log"), trControl = cv_control)
cv_results_negbin <- train(crime_count ~ ., data = subchic, method = "glm", family = negative.binomial(theta = 5, link = "log"), trControl = cv_control)

cv_results_poisson
cv_results_negbin

################################################################################ Plotting residuals etc. for model ################################################################################ 

plot(poissonmodel)

################################################################################ Chi Square Test ################################################################################ 

deviance_poisson <- 648617
deviance_negative_binomial <- 275577
df_poisson <- 257923
df_negative_binomial <- 257922

deviance_difference <- deviance_poisson - deviance_negative_binomial

df_difference <- df_poisson - df_negative_binomial

se <- sqrt(deviance_difference / df_difference)

expected_dev_diff <- se**2

test_statistic <- deviance_difference / sqrt(expected_dev_diff)

p_value <- 1 - pchisq(test_statistic, df = 1,lower.tail = TRUE, log.p = FALSE)

p_value

################################################################################ Comparing Base Models with Zero-inflated Models ################################################################################ 

vuong(poissonmodel, zipmodel)

vuong(negbinmodel, zinbmodel)



AIC(poissonmodel)
AIC(zipmodel)

AIC(negbinmodel)
AIC(zinbmodel)

################################################################################ Visualizing Goodness-of-fit of Poisson vs NegBin Model ################################################################################ 


observed_counts <- chic$crime_count
observed_counts <- observed_counts[seq(1, length(observed_counts), by = 5000)]

predicted_poisson <- predict(poissonmodel, type = "response")
predicted_negbin <- predict(negbinmodel, type = "response")

predicted_poisson <- predicted_poisson[seq(1, length(predicted_poisson), by = 5000)]
predicted_negbin <- predicted_negbin[seq(1, length(predicted_negbin), by = 5000)]



plot_data <- data.frame(Observed = observed_counts, Poisson = predicted_poisson, Negative_Binomial = predicted_negbin)

ggplot(plot_data, aes(x = Observed)) +
  geom_point(aes(y = Poisson, color = "Poisson"), alpha = 0.7) +
  geom_point(aes(y = Negative_Binomial, color = "Negative Binomial"), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "grey32", linetype = "dashed", size = 1.25) +
  labs(title = "Goodness-of-Fit Comparison: Poisson vs Negative Binomial Regression Model", subtitle = "Both models constructed using predictors 'district', 'hour_of_day', 'month', and 'day_of_week'. 1 in every 5,000 counts plotted for readability.",
       x = "Observed Counts",
       y = "Predicted Counts",
       color = "Model Type") +
  scale_color_manual(values = c("Poisson" = "blue", "Negative Binomial" = "red")) +
  theme_minimal()


### And just for NegBin Model
observed_counts <- chic$crime_count
predicted_poisson <- predict(poissonmodel, type = "response")
predicted_negbin <- predict(negbinmodel, type = "response")


plot_data <- data.frame(Observed = observed_counts, Poisson = predicted_poisson, Negative_Binomial = predicted_negbin)

ggplot(plot_data, aes(x = Observed)) +
  geom_point(aes(y = Negative_Binomial), color = "cornflowerblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "grey32", linetype = "dashed", size = 1.25) +
  labs(title = "Goodness-of-Fit Plot of Negative Binomial Regression Model", subtitle = "Model constructed using predictors 'district', 'hour_of_day', 'month', and 'day_of_week'. All counts shown except for four outliers.",
       x = "Observed Counts",
       y = "Predicted Counts") +
  xlim(0, 125) +
  theme_minimal() +
  theme(legend.position = "none")

