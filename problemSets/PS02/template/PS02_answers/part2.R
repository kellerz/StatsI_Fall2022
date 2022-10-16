

dat3 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")


dat3$female <- as.factor(dat3$female)

graph <- ggplot(dat3, aes(x = irrigation, y = water))+ geom_point(aes(color = factor(female)))
graph

reg1 <- lm(water ~ irrigation*female, data = dat3)

summary(reg1)

gp_male <- dat3[dat3$female==0,]
gp_female <- dat3[dat3$female==1,]
reg_male <- lm(water ~ irrigation, data=gp_male)
reg_female <- lm(water ~ irrigation, data=gp_female)

reg_male$coefficients
reg_female$coefficients

summary(reg_female)
summary(reg_male)

plot(water ~ irrigation, data=dat3, col=as.integer(female), xlab="Irrigation", 
     ylab="Water", pch=19, las=1)
abline(reg_male$coefficients, col=1, lwd=2)
abline(reg_female$coefficients, col=2, lwd=2)


output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

output_stargazer("regression_output1.tex", reg1)
