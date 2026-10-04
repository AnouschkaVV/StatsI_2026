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
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven"),  pkgTest)

# Set wd for current folder
setwd("/Users/anouschkaverdon/Desktop/StatsI_2026/tutorials/Week 3")

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# -------------------------------#
# 2. Load & Inspect Data
# -------------------------------#

df <- read_csv("../../datasets/fictional_data.csv")

# Quick overview
head(df)
str(df)
summary(df)

# Variables:
# - income: Monthly net income (numeric)
# - edu: University-level education in years (numeric)
# - cap: Binary variable (1 = lives in capital, 0 = otherwise)

# Quick recap: 

# Find the mean, variance, standard deviation and standard error of income
# Your answer here:
# Mean
# Variance
# Standard deviation
# Standard error


# -------------------------------#
# 3. Visualizing the Distribution
# -------------------------------#

# Create a histogram of income
hist(df$income,
     #breaks = 20,
     main = "Monthly net income",
     xlab = "Euro",
     col = "hotpink")

# Create a density plot of income
#first method
plot(density(df$income),
     main = "Monthly net income",
     xlab = "Euro")

#second method
ggplot(df, aes(x = income)) +
  geom_density(fill = "hotpink", color = "hotpink", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Monthly net income", x = "Euro")


# -----------------------------------------#
# 4. Sampling Distribution & Standard Error
# -----------------------------------------#
# Which kind of inferences can we make with regards to the population,
# based on the sample data, specifically the sample mean and SE?

# Why do we need the standard error?

# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

#find standard error
s <- sd(df$income, na.rm = TRUE)

n <- length(na.omit(df$income))

se_income <- s/sqrt(n)
print(se_income)

# Calculate the lower and upper CI
lower_95_n <- qnorm(0.025,
                    mean = mean(df$income),
                    sd = se_income)

upper_95_n <- qnorm(0.975,
                    mean = mean(df$income),
                    sd = se_income)
lower_95_n
upper_95_n

# Let's talk about qnorm()
?qnorm
qnorm(0.025) # value for first 2.5%
qnorm(0.975) # value last 2.5%
qnorm(0.025, mean=2, sd=0.4) # Change mean and standard error

# ---- 99% Confidence Interval (t Distribution) ----
# Let's talk about qt()
?qt
qt(0.005, df=length(df$income)-1) # critical value for first 0.5%
qt(0.995, df=length(df$income)-1) # last 0.5%
qt(0.005, df=length(df$income)-1, lower.tail=FALSE) # last 0.5%

# t distribution is used when the sample size is small
t_score <- qt(0.995, df = length(df$income) - 1)

# Re-calculate 99% CI around mean_income
#calculate mean
mean_income <- mean(df$income, na.rm = TRUE)

#get dgrees of freedom
n <- sum(!is.na(df$income))
df_degrees <- n-1

#find critical t-value
t_critical_99 <- qt(0.995, df = df_degrees)

lower_99_t <- mean_income - (t_critical_99 * se)

upper_99_t <- mean_income + (t_critical_99 * se)

lower_99_t
mean_income
upper_99_t
# -------------------------------#
# 6. Significance Tests
# -------------------------------#

# In statistics, a **significance test** checks whether an observed sample
# could plausibly have come from a population with a hypothesized parameter value.
# Here we focus on:
#   (a) Testing a single population mean
#   (b) Testing the difference between two group means

# ---------------------------------------------#
# Question:
# Is the average monthly income in our sample
# different from the population mean in Ireland (from Google: 3034)?

# Hypotheses: Should our test be one or two-sided? 
#ANSWER: should be two-sided
#H0: Average monthly income is 3034 (mu is equal to 3034)
#H1: Average monthly income is not 3034 (mu != 3034)

?t.test

# Conduct appropriate test (using built-in R function)
t.test(df$income, mu = 3034)
t.test(df$income, mu = 3034, alternative = "less")

# What is our conclusion?
t.test(df$income, conf.level = 0.99, alternative = "two.sided")

# ---------------------------------------------#
# Question:
#   Do people living in the capital earn different
#   incomes than those living elsewhere?
#
# Hypotheses: Should our test be one or two-sided? 

# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# -----------------------------------------------------------#
### Extra activity with real-world data (difference in means): 
# -----------------------------------------------------------#

# Goal: Test whether mean Polity scores differ between
#       Eastern Europe vs Western Europe & North America.
# Data: polity.dta — Polity score (0–10), higher = more democratic.

# Why not load("polity.dta")?
# - load() is for .RData/.rda (R’s serialized objects), not Stata files.
# - Use haven::read_dta() for .dta files.
data <- read.dta("../../datasets/polity.dta")

# Quick look
head(data)
glimpse(data)
table(data$region)

# Variable of interest: fh_polity2 - numeric Polity score (0-10)

# Subset the two regions of interest:
west <- data$fh_polity2[data$region == "Western Europe and North America"]
east <- data$fh_polity2[data$region == "Eastern Europe"]

# Quick descriptive statistics - careful for missing values! 
mean_west <- # your answer here
  mean_east <- # your answer here
  n_west    <- # your answer here
  n_east    <- # your answer here
  sd_west   <- # your answer here
  sd_east   <- # your answer here
  
mean_west; mean_east
n_west; n_east
sd_west; sd_east

# Calculate the SEs
# SE = sample SD / sqrt(n)
se_west <- # your answer here
se_east <- # your answer here
  
se_west; se_east

# -------------------------------------#
#  Analytical CI (Normal Approximation)
# -------------------------------------#

# By hand using:
#   Diff = mean_west - mean_east
#   SE_diff = sqrt(Var_west/n_west + Var_east/n_east)
#   95% CI = Diff ± 1.96 * SE_diff

# ------------------------#
#  Two-Sample t-test
# ------------------------#

# Conduct a two-sided diff-in-means test

# Conclusion?