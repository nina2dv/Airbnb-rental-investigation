library(dplyr)
calendar<- read.csv("C:\\Users\\zheng\\Downloads\\lodging\\datasets\\calendar.csv")

listings<-read.csv("C:\\Users\\zheng\\Downloads\\lodging\\datasets\\listings.csv")

finaldata<-read.csv("C:\\Users\\zheng\\Downloads\\final data test 5.csv")

#fit of total space available
fit.initial <- lm(total_space_available~annual_GDP+mean_household_income+median_household_income+total_population+personal_income_average+ur_average+ZHVI+ZRI, data=finaldata)
summary(fit.initial)
# Residuals vs fitted 
plot(fit.initial$fitted.values, fit.initial$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
# Distribution of residuals
h<-hist(fit.initial$residuals)#,xlab = "Residuals", main = "Histogram of Model Residuals") 
xfit <- seq(min(fit.initial$residuals), max(fit.initial$residuals), length = 40) 
yfit <- dnorm(xfit, mean = mean(fit.initial$residuals), sd = sd(fit.initial$residuals)) 
yfit <- yfit * diff(h$mids[1:2]) * length(fit.initial$residuals) 
lines(xfit, yfit, col = "black",lwd =1)

#QQ norm
qqnorm(scale(fit.initial$residuals)); abline(0,1)



# Residuals vs individual predictors
plot(finaldata$mean_household_income, fit.initial$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$median_household_income, fit.initial$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$total_population, fit.initial$residuals, pch=20, col="green", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")

#fit of total space available
fit <- lm(log(total_space_available)~annual_GDP+log(mean_household_income)+log(median_household_income)+log(total_population)+personal_income_average+ur_average+ZHVI+ZRI, data=finaldata)
#fit of listing count
#fit <- lm(listing_count~annual_GDP+median_household_income+total_population+personal_income_average+ZHVI+ZRI, data=finaldata)
summary(fit)


# Residual plots
# Residuals vs fitted
plot(fit$fitted.values, fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")

# Residuals vs individual predictors
plot(finaldata$annual_GDP, fit$residuals, pch=20, col="blue", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$mean_household_income, fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$median_household_income, fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$total_population, fit$residuals, pch=20, col="green", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$personal_income_average, fit$residuals, pch=20, col="blue", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$ur_average, fit$residuals, pch=20, col="blue", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$ZHVI, fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
plot(finaldata$ZRI, fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
# Distribution of residuals
hist(fit$residuals)
qqnorm(scale(fit$residuals)); abline(0,1)
#qqnorm(fit$residuals, datax = TRUE, pch=16)
#qqline(fit$residuals, datax = TRUE)

###################################################################################
library(MASS)
# Stepwise regression model
step.model <- stepAIC(fit,direction = "both", 
                      trace = FALSE)
summary(step.model)
# Distribution of residuals
hist(step.model$residuals)
qqnorm(scale(step.model$residuals)); abline(0,1)
# Residual plots
# Residuals vs fitted
plot(step.model$fitted.values, step.model$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")

###################################################################################
# Running a regression model on the fitted vs residuals
res.fit <- lm(abs(step.model$residuals) ~ step.model$fitted.values)
plot(step.model$fitted.values, abs(step.model$residuals))
abline(res.fit, col="red")

# Assign Weights
w <- 1/(res.fit$fitted.values^2)
# Running weighted least squares for listing
wls.fit <- lm(listing_count~annual_GDP+mean_household_income+median_household_income+total_population+personal_income_average, data=finaldata, weights=w)
summary(wls.fit)
# Running weighted least squares for volume
wls.space.fit <- lm(log(total_space_available)~annual_GDP+log(mean_household_income)+log(median_household_income)+log(total_population)+personal_income_average+ur_average+ZHVI+ZRI, data=finaldata, weights = w)
summary(wls.space.fit)

# Distribution of residuals
h<-hist(wls.space.fit$residuals)
xfit <- seq(min(wls.space.fit$residuals), max(wls.space.fit$residuals), length = 40) 
yfit <- dnorm(xfit, mean = mean(wls.space.fit$residuals), sd = sd(wls.space.fit$residuals)) 
yfit <- yfit * diff(h$mids[1:2]) * length(wls.space.fit$residuals) 
lines(xfit, yfit, col = "black",lwd =1)

#normal qq #
qqnorm(scale(wls.space.fit$residuals)); abline(0,1)
# Residual plots
# Residuals vs fitted
plot(wls.space.fit$fitted.values, wls.space.fit$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")

#################################################################################################
# Stepwise regression model
step.model.after <- stepAIC(wls.space.fit,direction = "both", 
                      trace = FALSE)
coef(step.model.after)
summary(step.model.after)
par(mfrow=c(1,1))
hist(step.model.after$residuals, main = '')
#qqnorm(scale(step.model.after$residuals)); abline(0,1)
# Residual plots
# Residuals vs fitted
plot(step.model.after$fitted.values, step.model.after$residuals, pch=20, col="red", cex=0.7)
abline(h=0, lty=2, lwd=2, col="black")
#Then get 95% CI:
confint(step.model.after, level = 0.95)
###################################################################################
#Modelling stop at here.



# Running a regression model on the fitted vs residuals
res.fit.sw <- lm(abs(step.model.after$residuals) ~ step.model.after$fitted.values)
plot(step.model.after$fitted.values, abs(step.model.after$residuals))
abline(step.model.after, col="red")

# Assign Weights
w2 <- 1/(res.fit.sw$fitted.values^2)

# Running weighted least squares
wls.sw.fit <- lm(listing_count~annual_GDP+mean_household_income+median_household_income+total_population+personal_income_average, data=finaldata, weights=w2)
summary(wls.sw.fit)

wls.space.sw.fit <- lm(log(total_space_available)~log(annual_GDP)+log(median_household_income)+log(total_population)+personal_income_average+ZHVI+ZRI, data=finaldata, weights = w2)
summary(wls.space.sw.fit)

hist(wls.space.sw.fit$residuals)
qqnorm(scale(wls.space.sw.fit$residuals)); abline(0,1)


