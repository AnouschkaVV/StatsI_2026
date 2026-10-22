
#set working directory
setwd("/Users/anouschkaverdon/Desktop/StatsI_2026/problem_sets/PS02 Solution")

install.packages("stargazer")
library(stargazer)

#Question 1: Political science
#QUESTION 1A
#Calculate the chi square statistic by hand

#(sum(observed frequency - expected frequency)^2)/expected frequency

#tabulate data
political_science <- matrix(c(14, 6, 7,
                             7,7,1),
                      nrow = 2,
                      byrow = TRUE)
rownames(political_science) <- c("Upper class", "Lower class")
colnames(political_science) <- c("Not stopped", "Bribe requested", "Stopped/given warning")
political_science

#add margins: totals of each row/column
political_science_sums <- addmargins(political_science)
political_science_sums

#get table
stargazer(political_science_sums,
          type = "latex",
          summary = FALSE,
          title = "Contingency table with marginal totals",
          label = "tab:polisci_sums")

#expected frequency = ((row total)*(column total))/N
row_totals <- rowSums(political_science)
col_totals <- colSums(political_science)
N <- sum(political_science)

expected_frequencies <- outer(row_totals, col_totals) / N
expected_frequencies

#chi square stat = ((observed - expected frequencies)^2) / expected frequencies
chi_square_stat <- sum((political_science - expected_frequencies)^2/ expected_frequencies)
chi_square_stat

#QUESTION 1B
#Now calculate the p-value from the test statistic you just created.
#What do you conclude if alpha = 0.1?

#get degrees of freedom
#df = (n_rows -1)*(n_cols -1)
n_rows <- 2
n_cols <- 3

df <- (n_rows -1)*(n_cols - 1)
df

p_val <- pchisq(chi_square_stat, df, lower.tail = FALSE)
p_val

#In a chi-square test, null hypothesis = the variables are independent; and alternative
#hypothesis = the variables are dependent. The p-value is 0.15 (2 d.p.). If alpha = 0.1,
#we reject the null hypothesis because the p-value is greater than the alpha value: 0.15 > 0.1.
#This means that the variables are not independent.

#QUESTION 1C
#Calculate the standardized residuals for each cell and put them in the table below.
#z = (observed - expected frequencies)/sqrt(expected frequency x (1 - row p) x (1- col p))

#row and column proportions
row_prop <- row_totals/N
col_prop <- col_totals/N

#match dimensions for 2 rows and 3 columns
prop_adjustment <- outer(1-row_prop, 1-col_prop)

standardised_residuals <- (political_science - expected_frequencies) / sqrt((expected_frequencies) * prop_adjustment)
standardised_residuals

#tabulate standardized residuals
residuals_table <- as.data.frame(standardised_residuals)
rownames(residuals_table) <- c("Upper class", "Lower class")
colnames(residuals_table) <-  c("Not stopped", "Bribe requested", "Stopped/given warning")
round(residuals_table, 3) #round to 3 d.p.

#get table
stargazer(residuals_table,
          type = "latex",
          summary = FALSE,
          digits = 3,
          title = "Table of standardised residuals",
          label = "tab:residuals")

#QUESTION 1D
#How might the standardized residuals help you interpret the results?

#Standardised residuals show how much the data in each cell vary from the model.
#The lower class has positive residual 1.642, meaning they experienced more bribe
#requests than expected. Conversely, the upper class has a negative residual -1.642,
#meaning they experienced fewer bribe requests than expected. Upper class was
#stopped but let off with a warning more often than expected, while the lower
#class was was stopped and let go less often than expected. Both residuals for
#upper and lower class in the stopped category are very close to zero, showing
#they approximately match the expected frequencies in this category. Critical
#threshold = +/- 1.96. None of the adjusted residuals breach the threshold,
#meaning that, while there are apparent trends in stopping/bribe requests/warnings
#across upper and lower classes, these deviations are not statistically significant.
#These trends observed could be due to random chance.

#Question 2: Economics
#QUESTION 2A: State a null and alternative (two-tailed) hypothesis.

#Null hypothesis: the reservation policy had no effect on the number of new or 
#repaired drinking water facilities in the villages, mu = 0
#Alternative hypothesis: the reservation policy did have an effect on the number
#of new or repaired drinking water facilities in the villages, mu != 0

#QUESTION 2B: Run a bivariate regression to test this hypothesis in R.

economics_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

View(economics_data)
head(economics_data)
str(economics_data)

#built-in R using lm()
#use "reserved" variable to avoid confounding variables
biv_regression <- lm(economics_data$water ~ economics_data$reserved)
biv_regression
summary(biv_regression)

#alpha = 14.738
#beta = 9.252
#t = 2.344
#p-value = 0.0197

#The p-value is less than standard significance threshold: 0.0197 < 0.05. As such,
#we reject the null hypothesis in favour of the alternative hypothesis, meaning
#that there is enough statistical evidence to suggest that the reservation policy
#had an effect on the drinking water facilities in these villages.


#QUESTION 2C: Interpret the coefficient estimate for reservation policy.

#The coefficient estimate is: beta = 9.252. This shows that the reservation policy
#had a positive effect, meaning that there were more drinking water facilities
#due to the reservation policy. In other words, villages that were randomly selected
#to have have their GP reserved for female leaders built/repaired 9.252 more
#drinking water facilities than villages without the reservation policy.


