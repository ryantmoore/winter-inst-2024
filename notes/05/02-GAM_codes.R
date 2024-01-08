library(mgcv)
library(ggplot2)


# always use ?function() e.g. ?gam()
?gam()
# silde 7---------------
# Simulating a non-linear dataset
set.seed(123) # Setting a seed for reproducibility
x <- seq(0, 10, length.out = 100)
y <- sin(x) + rnorm(100, sd = 0.2) # Adding some noise

# Creating a data frame
data <- data.frame(x, y)

# Visualizing the dataset
plot(data$x, data$y, main = "Simulated Dataset", xlab = "X", ylab = "Y", pch = 19)

# slide 8----------------------------------
# Fit a linear model
lm_model_simulated <- lm(y ~ x, data = data)

# Fit a GAM model
gam_model_simulated <- gam(y ~ s(x), data = data)

# Create a plot with both fits
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "darkred", se = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x), colour = "navyblue", se = FALSE) +
  labs(title = "Comparison of LM and GAM on Simulated Data",
       x = "X",
       y = "Predicted Y")




# model comparsion slide 9-11--------------


summary(lm_model_simulated)
summary(gam_model_simulated)

# Calculate residuals for LM
lm_resid <- residuals(lm_model_simulated)

# Calculate residuals for GAM
gam_resid <- residuals(gam_model_simulated)

# Plot residuals
par(mfrow = c(1, 2))
plot(lm_resid, main = "Residuals of LM")
plot(gam_resid, main = "Residuals of GAM")

# AIC for LM
AIC(lm_model_simulated)

# AIC for GAM
AIC(gam_model_simulated)


# Print model summary for LM
summary(lm_model_simulated)

# Print model summary for GAM 
summary(gam_model_simulated)

# Calculate RMSE for both models
lm_rmse <- sqrt(mean(resid(lm_model_simulated)^2)) 
gam_rmse <- sqrt(mean(resid(gam_model_simulated)^2))

# Print RMSE
cat("LM RMSE:", lm_rmse, "\n")
cat("GAM RMSE:", gam_rmse, "\n")

# Calculate R-squared for both models
lm_rsq <- summary(lm_model_simulated)$r.squared
gam_rsq <- summary(gam_model_simulated)$r.sq

# Print R-squared
cat("LM R-squared:", lm_rsq, "\n") 
cat("GAM R-squared:", gam_rsq, "\n")

# Perform anova to compare models 
anova(lm_model_simulated, gam_model_simulated, test="F")


dev.off()
# slide 13-15 [mtcars]------
# Loading the mtcars dataset
data(mtcars)

# View the first few rows of the dataset
head(mtcars)

# Selecting variables of interest, for example, mpg (Miles/(US) gallon) and hp (Gross horsepower)
# Exploring the relationship between 'mpg' and 'hp'
plot(mtcars$hp, mtcars$mpg, main = "Relationship between Horsepower and MPG", xlab = "Horsepower (hp)", ylab = "Miles per Gallon (mpg)", pch = 19)



# Fit a linear model
lm_model_mtcars <- lm(mpg ~ hp, data = mtcars)

# Fit a GAM model
gam_model_mtcars <- gam(mpg ~ s(hp), data = mtcars)

# Create a plot with both fits
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "darkred", se = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x), colour = "navyblue", se = FALSE) +
  labs(title = "Comparison of LM and GAM on mtcars Data",
       x = "Horsepower (hp)",
       y = "Miles per Gallon (mpg)")


# Make predictions using the GAM model
gam_predictions <- predict(gam_model_mtcars)
lm_predictions <- predict(lm_model_mtcars)
gam_predictions
gam_predictions
# Set up the graphical space as a 2x1 grid
par(mfrow = c(2, 1))

# Create histograms of the predictions
# Histogram of linear model predictions
hist(lm_predictions, breaks=10, main="Histogram of Linear Model Predictions of MPG", xlab="Predicted MPG", col="blue", xlim=range(c(lm_predictions, gam_predictions)))

# Histogram of GAM model predictions
hist(gam_predictions, breaks=10, main="Histogram of GAM Model Predictions of MPG", xlab="Predicted MPG", col="green", xlim=range(c(lm_predictions, gam_predictions)))

# Reset the graphical parameters
par(mfrow = c(1, 1))

# jeff's codes, slide 21---------------------------------
x <- seq(1,25,length=600)
y <- (2/(pi*x))^(0.5)*(1-cos(x)) + rnorm(100,0,1/10)
par(mar=c(3,3,2,2), bg="white")
plot(x,y,pch="+")
ols.object <- lm(y~x)
abline(ols.object,col="blue")
lo.object <- lowess(y~x,f=2/3)
lines(lo.object$x,lo.object$y,lwd=2,col="red")
lo.object <- lowess(y~x,f=1/5)
lines(lo.object$x,lo.object$y,lwd=2,col="purple")

##
x <- seq(1, 25, length = 600)
y <- (2 / (pi * x))^(0.5) * (1 - cos(x)) + rnorm(600, 0, 1/10)

# Linear Model Summary
summary(lm(y ~ x))$coef

# Setting plot parameters
par(mar = c(3, 3, 2, 2), bg = "white")
plot(x, y, pch = "+")

# Linear Regression
ols.object <- lm(y ~ x)
abline(ols.object, col = "blue")

# Lowess Smoothing with f=2/3
lo.object <- lowess(y ~ x, f = 2/3)
lines(lo.object$x, lo.object$y, lwd = 2, col = "red")

# Lowess Smoothing with f=1/5
lo.object <- lowess(y ~ x, f = 1/5)
lines(lo.object$x, lo.object$y, lwd = 2, col = "purple")
####

# Creating a data frame for GAM
df <- data.frame(x = x, y = y)

# Fit a GAM model
gam_model <- gam(y ~ s(x), data = df)

# Add the GAM smooth to the plot
lines(df$x, predict(gam_model, df), col = "green", lwd = 2)

# Add Legend
legend("topright", 
       legend = c("Linear Regression", "Lowess f=2/3", "Lowess f=1/5", "GAM"), 
       col = c("blue", "red", "purple", "green"), 
       lwd = 2)



# End of the codes--------------



