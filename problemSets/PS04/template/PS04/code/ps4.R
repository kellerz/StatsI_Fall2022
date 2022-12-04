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
lapply(c("stringr", "tidyverse", "stargazer", "broom"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(car)
data(Prestige)
help(Prestige)

# Question 1 (a), creating new variable

professional <- c()


for (i in Prestige$type) {
  if (is.na(i) == TRUE) {
    professional <- append(professional, NA)
  }
  else if (i == "prof") {
    professional <- append(professional, 1)
  }
  else {
    professional <- append(professional, 0)
  }
  
}

Prestige <- cbind(Prestige, professional)   

# (b) Running regression

reg1 <- lm(prestige ~ income + professional + income*professional, data = Prestige)

summary(reg1)

# Creating table using Stargazer

stargazer(reg1, type = "latex", title = "Regression")

# Plotting data

intercept_bcwc <- reg1$coefficients[1]
slope_bcwc <- reg1$coefficients[2]

intercept_prof <- reg1$coefficients[1] + reg1$coefficients[3]
slope_prof <- reg1$coefficients[2] + reg1$coefficients[4]

plot(Prestige$income, Prestige$prestige, type = "n",
     ylab = "Presitge", xlab = "Income")
abline(intercept_bcwc, slope_bcwc, col=2)
abline(intercept_prof, slope_prof, col=4, lty=2)


?pt()

2*(pt(2.625, df=128, lower.tail = FALSE))

2*(pt(3.231, df=128, lower.tail = FALSE))

2*(pt(-0.14, df=379))
