#Loading in packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse"),  pkgTest)
lapply(c("ggplot2", "stargazer"),  pkgTest)


#Creating data and adding it to a matrix
dat <- c(14, 7, 6, 7, 7, 1)


datmat <- matrix(data = dat, nrow = 2, ncol = 3)
rownames(datmat) <- c("upper class", "lower class")
colnames(datmat) <- c("not stopped", "bribe requested", "stopped/given warning")

#Getting column and row totals
tab <- addmargins(datmat)

tab

#Getting P-value of chi square test
pchisq(3.80342207, df=2, lower.tail=F)

#Comparing answers
chisq.test(datmat)


