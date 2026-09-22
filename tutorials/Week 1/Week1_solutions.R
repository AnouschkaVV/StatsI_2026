# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
sum_y <- sum(y)
print(sum_y)

# (2) find mean of y using your "own" function
mean_y <- (sum(y))/length(y)
print(mean_y)

# now do the same thing, but faster using the built-in R function
mean(y)

# (3) find sum of demeaned values
sum_demeaned <- sum(y- (mean_y))
print(sum_demeaned)

# (4) calculate sum of squared error
sum_squared_error <- sum_demeaned^2
print(sum_squared_error)


###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 
sort(quantilesVec)
median(quantilesVec)

#different way
summary(quantilesVec)

# (2) calculate quantiles
summary(quantilesVec, c(0.25, 0.5, 0.75))

#make boxplot
boxplot(quantilesVec, col = "hotpink")

# (3) make a histogram of state median income
state.x77[,2]

hist(state.x77[,2], main = "Distribution of state median income", xlab = "Median income", col = "hotpink")

# remember to save your plot as a pdf
pdf("medianincomeHist.pdf")
dev.off()


