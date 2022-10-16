library("tidyverse")

dat <- c(14, 7, 6, 7, 7, 1)


datmat <- matrix(data = dat, nrow = 2, ncol = 3)
rownames(datmat) <- c("upper class", "lower class")
colnames(datmat) <- c("not stopped", "bribe requested", "stopped/given warning")

tab <- addmargins(datmat)

tab

pchisq(3.80342207, df=2, lower.tail=F)

chisq.test(datmat)


