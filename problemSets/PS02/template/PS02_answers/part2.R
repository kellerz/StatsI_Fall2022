
#Reading in the data
dat3 <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#Changing the reserved variable to a factor variable
dat3$reserved <- as.factor(dat3$reserved)

#Visualising the data
graph <- ggplot(dat3, aes(x = reserved, y = water))+ geom_point(aes(color = factor(reserved)))
graph

#Main regression
reg <- lm(water ~ reserved, data = dat3)

summary(reg)

#Function for output files + Creating output files
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

output_stargazer("regression_output1.tex", reg)
