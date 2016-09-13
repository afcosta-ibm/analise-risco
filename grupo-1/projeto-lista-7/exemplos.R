plot(ddd[,1], ddd[,2])

plot(ddd[,1], ddd[,7])

abline(lm(ddd[,7]~ddd[,1]))

boxplot(ddd[,7])

boxplot(ddd[,3])

plot(ddd[,3], ddd[,7])

abline(lm(ddd[,7]~ddd[,3]))
