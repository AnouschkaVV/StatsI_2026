# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd("/Users/anouschkaverdon/Desktop/StatsI_2026/tutorials/Week 5")

# Check working directory
getwd()

# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("../../datasets/fictional_data.csv")
View(df)

# Create a scatter plot 
# Improve visualization with detailed axis and main labels, save as PDF
plot(fictional_data$edu, fictional_data$income,
     main = "The relationship between eduation and income",
     xlab = "Education",
     ylab = "Income",
     col = "hotpink",
     pch = 16)

# Calculate correlation
r <- cor(df$edu, df$income, method = "pearson")
r

# Add correlation to scatter plot
text(1.5, 2600, sprintf("Correlation=%s", round(r, 4)))

# Run a t-test for the correlation coefficient
cor.test(df$edu, df$income)

# Check p-value
sprintf("%.20f", 7.52e-07)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
# Save model as object

# Output interpretation:
#  - Intercept (b0)
#  - Slope (b1)

# Conduct t-test for the slope of a regression line
model <- lm(income ~ edu, data = df) #first variable is always the y
model

summary(model)
250.64/33.06

# Check p-value
sprintf("%.20f", 2.17e-06)

# Create a residual plot
model.res <- resid(model)
model.res

plot(df$edu, model.res,
     xlab = "Residuals",
     ylab = "University education (in years)",
     col = "hotpink",
     pch = 16)
abline(0,0, col = "orange") #draw a baseline/reference line at 0

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients
confint_95 <- confint(model, level = 0.95)
confint_95

confint_99 <-confint(model, level = 0.99)
confint_99

# Create a scatter plot w/ educate and income, add regression line
plot(fictional_data$edu, fictional_data$income,
     main = "The relationship between eduation and income",
     xlab = "Education",
     ylab = "Income",
     col = "hotpink",
     pch = 16)
abline(model, col = "orange")

# What is the prediction equation?
summary(model)
#predicted income = 976.15 + 250.64 * edu

# Make predictions for first observation in df
head(df)
976.16 + 250.64 * 1 #predicted outcome
model$fitted.values

# Make predictions for a range of x values (specifically min to max of education)
predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Add standard errors for your predicted values
predict(model, newdata=data.frame(edu = c(0, 1, 2, 3, 4, 5, 6, 7, 8)))
predict(model, newdata=data.frame(edu = c(0, 1, 2, 3, 4, 5, 6, 7, 8)), se.fit = TRUE)

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x
predict(model, newdata=data.frame(edu = c(0, 1, 2, 3, 4, 5, 6, 7, 8)), interval = "confidence")

predict(model, newdata=data.frame(edu = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 15)), se.fit = TRUE)
#when x is beyond the data we have, predictions get more and more inaccurate
#(e.g., with x = 15, standard error = 387.00340)

# Make predictions with **prediction intervals**
# Predict an individual's response at any chosen value of x 
# Remember: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2