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

lapply(c("tidytext"),  pkgTest)
lapply(c("ggplot2", "stargazer"),  pkgTest)

# set working directory
setwd("~/Documents/Trinity/stats/problems/PS1/PS01")
getwd()


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# ~~PART 1 - CONFIDENCE INTERVAL~~ #

#visualise the data

histo1 <- hist(y,
     main = "Histogram of IQ",
     xlab = "IQ Score"
     )
dev.off()

plotdens <- plot(density(y),
     main = "Pdf of IQ",
     xlab = "IQ Score"
     )

qqnorm(y)
qqline(y,
       distribution = qnorm)

# As seen from the graphs, the data is not normally distributed
# Therefore, the confidence interval should be constructed using a t-distribution

stderror <- sd(y)/sqrt(length(y)) 
t_score <- qt(.05, df = length(y)-1, lower.tail = FALSE)
CI_lower_t <- mean(y) - (stderror * t_score)
CI_upper_t <- mean(y) + (stderror * t_score)

#Double-checking answer

iqt <- t.test(y, conf.level = 0.9, alternative = "two.sided")
(iqt)
output_stargazer("iqttest_output.tex", iqt)

# ~~PART 2: HYPOTHESIS TESTING~~ #

# STEP ONE: Assumptions about your data
# As shown from the data visualisations in the last part, the data is not normally distributed and has a small sample size

# Step TWO: Null and alternative hypothesis
# In this case, we would have a one-tailed hypothesis test, as we are only testing for one direction. Therefore:
# H0: sample mean <= 100
# H1: sample mean > 100

# STEP THREE: TEST STATISTIC 
t.test(y, conf.level = 0.95, alternative = "greater", mu = 100)

?t.test
# STEP FOUR: P-VALUE

pt(37.593, df=24, lower.tail = F)

# STEP FIVE: CONCLUSION 

#####################
# Problem 2
#####################

expenditure <- read.table("C:/Users/kelle/Documents/Trinity/stats/problems/PS1/PS01/data/expenditure.txt", header=T)

expenditure$Region <- factor(expenditure$Region)


#Plotting Y and X1
# create scatterplots of Y against the X variables
par(mfrow = c(2, 2))
graph1 <- plot(expenditure$X1, expenditure$Y)
abline(lm(expenditure$Y ~ expenditure$X1))
graph2 <- plot(expenditure$X2, expenditure$Y)
abline(lm(expenditure$Y ~ expenditure$X2))
graph3 <- plot(expenditure$X3, expenditure$Y)
abline(lm(expenditure$Y ~ expenditure$X3))
graph4 <- plot(expenditure$Region, expenditure$Y, type = "h")


#Carrying out regressions
regression1 <- lm(expenditure$Y ~ expenditure$X1)
regression2 <- lm(expenditure$Y ~ expenditure$X2)
regression3 <- lm(expenditure$Y ~ expenditure$X3)

# creating the ggplot to mark expendi

graph5 <- ggplot(expenditure, aes(x = X1, y = Y))+ geom_point(aes(color = factor(Region)))

graph5

typeof(expenditure)

?ggplot()

dev.off()

# run an example regression, to show how to save table
regression1 <- lm(Y~X1, data=expenditure)
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("regression_output1.tex", regression1)
output_stargazer("regression_output2.tex", regression2)
output_stargazer("regression_output3.tex", regression3)

