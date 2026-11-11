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
# lapply(c("stringr"),  pkgTest)
lapply(c("stringr", "readr", "ggplot2", "dplyr", "viridis", "foreign", "haven", "stargazer"),  pkgTest)

#set wd for current folder
setwd("/Users/anouschkaverdon/Desktop/StatsI_2026/problem_sets/PS03/my_answers")

#read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#view data
View(inc.sub)
head(inc.sub)
str(inc.sub)
summary(inc.sub)

#QUESTION 1
#We are interested in knowing how the difference in campaign spending between incumbent
#and challenger affects the incumbent’s vote share.

#a) Run a regression where the outcome variable is "voteshare" and the explanatory variable
#is "difflog".

regression_1 <- lm(inc.sub$voteshare ~ inc.sub$difflog)
summary(regression_1)

#b) Make a scatterplot of the two variables and add the regression line.
pdf(file = "P3_graph_1.pdf", width = 14, height = 8) #save to pdf

ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(size = 0.4) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") + #line of best fit
  labs(
    title = "Relationship between campaign spending and incumbent vote share",
    x = "Difference in campaign spending",
    y = "Incumbent vote share"
  )

dev.off() 

#c) Save the residuals of the model in a separate object.
residuals_1 <- residuals(regression_1)
head(residuals_1)

#d) Write the prediction equation.

#y-intercept = 0.57903
#beta value = 0.04167
#prediction equation: predicted_voteshare = 0.57903 + 0.04167(difflog)
#predicted_voteshare = 0.58 + 0.04(difflog)           rounded to 2 d.p.


#QUESTION 2
#We are interested in knowing how the difference between incumbent and challenger’s
#spending and the vote share of the presidential candidate of the incumbent’s party are related.

#a) Run a regression where the outcome is "presvote" and the explanatory variable is "difflog".
regression_2 <- lm(inc.sub$presvote ~ inc.sub$difflog)
summary(regression_2)

#b) Make a scatterplot of the two variables and add the regression line.
pdf(file = "P3_graph_2.pdf", width = 14, height = 8) #save to pdf

ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(size = 0.4) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") + #line of best fit
  labs(
    title = "Relationship between campaign spending and president votes",
    x = "Difference in campaign spending",
    y = "President votes"
  )

dev.off() 

#c) Save the residuals of the model in a separate object.
residuals_2 <- residuals(regression_2)
head(residuals_2)

#d) Write the prediction equation.

#y-intercept = 0.50758
#beta value = 0.02384
#prediction equation: predicted_presvote = 0.50758 + 0.02384(difflog)
#predicted_presvote = 0.51 + 0.02(difflog)           rounded to 2 d.p.


#QUESTION 3
#We are interested in knowing how the vote share of the presidential candidate of
#the incumbent’s party is associated with the incumbent’s electoral success.

#a) Run a regression where the outcome variable is "voteshare" and the explanatory
#variable is "presvote".
regression_3 <- lm(inc.sub$voteshare ~ inc.sub$presvote)
summary(regression_3)

#b) Make a scatterplot of the two variables and add the regression line.
pdf(file = "P3_graph_3.pdf", width = 14, height = 8) #save to pdf

ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(size = 0.4) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") + #line of best fit
  labs(
    title = "Relationship between incumbent vote share and president votes",
    x = "President votes",
    y = "Incumbent vote share"
  )

dev.off() 

#c) Write the prediction equation.

#y-intercept = 0.4413
#beta value = 0.3880
#prediction equation: predicted_voteshare = 0.4413 + 0.3880(presvote)
#predicted_voteshare = 0.44 + 0.39(presvote)           rounded to 2 d.p.


#QUESTION 4
#The residuals from part (a) tell us how much of the variation in voteshare is not explained
#by the difference in spending between incumbent and challenger. The residuals in part (b)
#tell us how much of the variation in presvote is not explained by the difference in spending
#between incumbent and challenger in the district.

#a) Run a regression where the outcome variable is the residuals from Question 1 and the 
#explanatory variable is the residuals from Question 2.
regression_4 <- lm(residuals_1 ~ residuals_2)
summary(regression_4)

#b) Make a scatterplot of the two residuals and add the regression line.
pdf(file = "P3_graph_4.pdf", width = 14, height = 8) #save to pdf

ggplot(data = data.frame(residuals_2, residuals_1), aes(x = residuals_2, y = residuals_1)) +
  geom_point(size = 0.4) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") + #line of best fit
  labs(
    title = "Relationship between residuals",
    x = "Residuals 2",
    y = "Residuals 1"
  )

dev.off() 

#c) Write the prediction equation.

#y-intercept = -4.860e-18
#beta value = 2.569e-01
#prediction equation: residuals_1 = -4.860e-18 + 2.569e-01(residuals_2)
#residuals_1 = 0 + 0.26(residuals_2)           rounded to 2 d.p.


#QUESTION 5
#What if the incumbent’s vote share is affected by both the president’s popularity and the
#difference in spending between incumbent and challenger?

#a) Run a regression where the outcome variable is the incumbent's "voteshare" and the
#explanatory variables are "difflog" and "presvote".
regression_5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
summary(regression_5)

#b) Write the prediction equation.

#y-intercept = 0.44864
#beta value 1 = 0.03554 
#beta value 2 = 0.25688
#prediction equation: voteshare = 0.44864 + 0.03554(difflog) + 0.25688(presvote)
#voteshare = 0.45 + 0.04(difflog) + 0.26(presvote)           rounded to 2 d.p.

#c) What is it in this output that is identical to the output in Question 4? Why do you
#think this is the case?

#They have the same coefficent 0.26 because they're both holding difflog constant



