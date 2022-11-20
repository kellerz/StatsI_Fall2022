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

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")


#Question 1

reg1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(reg1)


ggplot(aes(x = difflog, # Scatterplot
           y = voteshare), 
       data = inc.sub) +
  geom_smooth(method = "lm") +
  geom_point()


resid1 <- reg1$residuals

stargazer(reg1, type = "latex", out = "reg1.tex", title = "Difflog and Voteshare") # Latex table

#Question 2

reg2 <- lm(presvote ~ difflog, data = inc.sub)
summary(reg2)


ggplot(aes(x = difflog, # Scatterplot
           y = presvote), 
       data = inc.sub) +
  geom_smooth(method = "lm") +
  geom_point()


resid2 <- reg2$residuals

stargazer(reg2, type = "latex", out = "reg2.tex", title = "Difflog and Presvote")

#Question 3

reg3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(reg3)


ggplot(aes(x = presvote, # Scatterplot
           y = voteshare), 
       data = inc.sub) +
  geom_smooth(method = "lm") +
  geom_point()


stargazer(reg3, type = "latex", out = "reg3.tex", title = "Presvote and Voteshare")

#Question 4

reg4 <- lm(resid1 ~ resid2)
summary(reg4)


residuals <- cbind.data.frame(resid1, resid2)

plot(residuals$resid2, residuals$resid1)
abline(lm(resid1 ~ resid2), col = "blue")

ggplot(aes(x = resid2, # Scatterplot
           y = resid1), data = reg4) 
  geom_smooth(method = "lm")


stargazer(reg4, type = "latex", out = "reg4.tex", title = "Residuals of Reg1 and Reg2")

#Question 5

reg5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(reg5)

stargazer(reg5, type = "latex", out = "reg5.tex", title = "Voteshare and Difflog + Presvote")

install.packages("car")
library(car)
?avPlot()

avPlots(reg5) # AddedVariable Plot
