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

#set wd
setwd("/Users/anouschkaverdon/Desktop/StatsI_2026/problem_sets/PS01 Copy/PS01 Solutions")

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest)

lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven", "stargazer"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Question 1: Find a 90% confidence interval for the average student IQ in the school

pdf(file = "IQ_histogram.pdf", width = 14, height = 8) #save to pdf

#histogram of IQ scores to check normal distribution
hist(y,
     breaks= 8,
     col = "hotpink",
     probability = TRUE,
     main = "Histogram of student IQ scores",
     xlab = "IQ scores"
)

lines(density(y), col = "blue", lwd = 3)

dev.off()

#descriptive statistics
n <- length(y) #length of y
mean_y <- sum(y)/n #sample mean of IQ: sum of y / length of y
sd_y <- sd(y) #standard deviation of IQ
se_y <-sd_y /sqrt(n) #standard error
n; mean_y; sd_y; se_y
#25; 98.44; 13.09 (2 d.p.); 2.62 (2 d.p.)

#confidence interval
#find critical value
t_critical <- qt(0.95, df = n-1)
t_critical #t-value = 1.71 (3 s.f.)

#CI = mean +/- critical value * SE
lower_90 <- mean_y - t_critical * se_y
upper_90 <- mean_y + t_critical * se_y

lower_90 #93.95 (2 d.p.)
upper_90 #102.92 (2 d.p.)

#Question 2: Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

#hypotheses
#null hypothesis: the average IQ is the same as the country average, μ = 100
#alternative hypothesis: the average IQ is higher than the country average, μ > 100

#calculate test statistic
t_score <- (mean_y - 100) / se_y
t_score #-0.596 (3 s.f.)

p_value <- pt(t_score, df = n-1, lower.tail = FALSE)
p_value #0.722 (3 s.f.)

#0.72 > 0.05 therefore we fail to reject the null hypothesis

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

#Expenditure dataset structure
head(expenditure)
str(expenditure) #structure of dataset
summary(expenditure)

#make Region a factor
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
pdf(file = "P1_expenditure_relationships.pdf", width = 14, height = 8) #save to pdf

vars <- c("Y", "X1", "X2", "X3")

labels <- c(
  "Expenditure",
  "Income",
  "Financial insecurity",
  "Urban residents"
)

par(mfrow = c(2,3)) #a 2 by 3 grid layout

#for loop
for (i in 1:(length(vars)-1)) {
  for (j in (i+1): length(vars)) {
    plot(
      expenditure[[vars[j]]],
      expenditure[[vars[i]]],
      xlab = labels[j],
      ylab = labels[i],
      main = paste(labels[i], "vs", labels[j]),
      cex = 0.5 #make data points smaller
    )
    abline( #line of best fit
      lm(expenditure[[vars[i]]]~ expenditure[[vars[j]]]),
      col = "hotpink"
    )
    r <- cor( #Pearson correlation coefficient ("r" value)
      expenditure[[vars[i]]],
      expenditure[[vars[j]]]
      )
    mtext(
      paste("r=", round(r,2)) #round to 2 d.p.
    )
    }
}

dev.off() 

#PART 2: Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?

#plot the relationship between Y and Region

pdf(file = "P1_expenditure_relationship_region.pdf", width = 14, height = 8) #save to pdf

#make Region a factor
expenditure$Region_factor <- factor(
  expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast", "North Central", "South", "West")
)

#make boxplot
ggplot(expenditure, aes(x = factor(Region), y = Y, color = factor(Region))) +
  geom_boxplot() +
  stat_summary(
    aes(shape = "Mean"),
    fun = mean,
    geom = "point",
    size = 3 #make mean value visually bigger
  ) +
labs(
  title = "Shelter/housing assistance expenditure by Region",
  x = "Region",
  y = "Per capita expenditure",
  color = "Region", #header for graph key
  shape = ""
) +
  scale_shape_manual(
    values = c("Mean" = 18) #show mean as diamond
    ) +
  scale_color_manual(values = c("hotpink", "violet", "orange", "firebrick1")) #colour-code regions

dev.off()

#which region has highest per capita expenditure
#means by region
region_means <- aggregate(Y~ Region, data = expenditure, FUN = mean) #find means of each region
region_means

#medians by region
region_medians <- aggregate(Y~ Region, data = expenditure, FUN = median) #find means of each region
region_medians

#merge mean and median
region_mean_median <- merge(region_means, region_medians, by = "Region")

colnames(region_mean_median) <- c("Region", "Mean expenditure", "Median expenditure")

#generate table
stargazer::stargazer(
  region_mean_median,
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  digits = 1,
  title = "Mean and median expenditure by Region",
  label = "tab:region_mean_median",
  out = "expenditure_mean_median.tex"
)

#PART 3: Please plot the relationship between Y and X1 ? Describe this graph and the relationship.
#Reproduce the above graph including one more variable Region and display different regions with
#different types of symbols and colors.

pdf(file = "P1_expenditure_income.pdf", width = 14, height = 8) #save to pdf

#reset multi-panel layout
par(mfrow = c(1,1))

#plot relationship between Y and X1
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "hotpink") + #line of best fit
  labs(
    title = "Relationship between expenditure and personal income",
    x = "Per capita personal income",
    y = "Per capita expenditure",
  )

dev.off()

pdf(file = "P1_expenditure_income_region.pdf", width = 14, height = 8) #save to pdf


#reproduce graph with one more variable Region + display different regions
ggplot(
  expenditure,
  aes(
    x = X1,
    y = Y,
    color = factor(Region),
    shape = factor(Region)
    )
  ) +
  geom_point() +
  labs(
    title = "Expenditure vs Income by Region",
    x = "Per capita personal income",
    y = "Per capita expenditure",
    color = "Region",
    shape = "Region"
    )

dev.off()
  