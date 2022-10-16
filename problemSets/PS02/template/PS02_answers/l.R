bigup <- lm(water ~ reserved, data = dat3)
summary(bigup)


plot(water ~ reserved, data=dat3, col=as.integer(female), xlab="Reserved", 
     ylab="Water", pch=19, las=1)

