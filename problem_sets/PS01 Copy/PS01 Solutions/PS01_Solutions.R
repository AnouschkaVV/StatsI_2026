#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)

lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Question 1: Find a 90% confidence interval for the average student IQ in the school

#histogram of IQ scores to check normal distribution
hist(y,
     col = "hotpink",
     main = "Histogram of student IQ scores",
     xlab = "IQ scores"
)

#descriptive statistics
#sample mean of IQ: sum of y / length of y
mean_y <- sum(y)/length(y)
mean_y

#standard deviation of IQ
sd_y <- sd(y)
sd_y

#length of y
n <- length(y)
n

#standard error
se_y <-sd_y /sqrt(n)
se_y

#confidence interval
ci_y <- mean_y + (se_y)

#find critical value
t_score <- qt(0.95, df = n-1)
t_score

#CI = mean +/- critical value * SE
lower_90 <- mean_y - t_score * se_y
upper_90 <- mean_y + t_score * se_y

lower_90
upper_90

#Question 2: Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

#hypotheses
#null hypothesis: the average IQ is the same as the country average, μ = 100
#alternative hypothesis: the average IQ is higher than the country average, μ > 100

#calculate test statistic
critical_value <- (mean_y - 100) / se_y
critical_value

#t-value = 1.71 (3 s.f.)
#critical value = -0.596 (3 s.f.)
#(-0.596 < 1.71) therefore we fail to reject the null hypothesis
#This means that there is not enough statistical evidence at a 90% confidence level to
#conclude that the average student IQ of the class is higher than the country's average

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

#Expenditure dataset structure
head(expenditure)
str(expenditure)
summary(expenditure)

expenditure$Region <- factor(expenditure$Region,
                             levels = 1:4,
                             labels = c("Northeast", "North Central", "South", "West"))
table(expenditure$Region)

#variables
#State:   50 states in US
#Y:       per capita expenditure on shelters/housing assistance in state
#X1:      per capita personal income in state
#X2:      Number of residents per 100,000 that are "financially insecure” in state
#X3:      Number of people per thousand residing in urban areas in state
#Region:  1=Northeast, 2= North Central, 3= South, 4=West

#PART 1: Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
#among them (you just need to describe the graph and the relationships among them)?

#using a scatterplot matrix to plot the relationship among Y, X1, X2, and X3
pairs(expenditure[, c("Y", "X1", "X2", "X3")],
      main = "Relationship among Y, X1, X2, X3",
      pch = 16, #solid dots
      upper.panel = NULL) #remove repeats of scaterrplots

#PLOT 1: The top left plot shows the relationship between Y (the per capita expenditure on shelters/housing
#assistance in state) and X1 (the per capita personal income in state). There appears to be a strong
#positive correlation, which indicates that there is a positive association between per capita
#expenditure and per capita personal income. This suggests that states with higher per capita
#personal income spend  more on housing assistance and shelters than states with lower per capita
#personal incomes.

#PLOT 2: The second plot down on the left shows the relationship between Y (the per capita expenditure
#on shelters/housing assistance in state) and X2 (number of residents per 100,000 that are
#"financially insecure” in state). It shows a moderately positive relationship between per capita
#expenditure and the financial insecurity of residents. The points are less tightly clustered as in 
#the first plot but there still appears to be an upward trend, indicating that when the state's  
#residents are more financially insecure, there is higher spending on shelters and housing assistance.

#PLOT 3: The second plot from the right shows the relationship between X1 (per capita personal
#income in state) and X2 (number of residents per 100,000 that are "financially insecure” in state).2



#PART 2: Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?

#plot the relationship between Y and Region




#region with highest per capita expenditure on housing assistance is ...



#PART 3: Please plot the relationship between Y and X1 ? Describe this graph and the relationship.
#Reproduce the above graph including one more variable Region and display different regions with
#different types of symbols and colors.

#plot relationship between Y and X1


#describe graph and the relationship



#reproduce graph with one more variable Region + display different regions




