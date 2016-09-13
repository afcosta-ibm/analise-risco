library(MASS)
library(triangle)

NumberOfSamples <- 1000

c1 <- rtriangle(NumberOfSamples,10,30,20)
c2 <- rtriangle(NumberOfSamples,20,60,40)

ct <- 2 * c1+rnorm(1000,30,10)

covariancia <- (c1-mean(c1)) * (c2-mean(c2))

print(head(covariancia, 20))

#plot(c1,c2)

print(sum(covariancia)/ NumberOfSamples)

print(cor(c1, c2))

# ct <- c1+c2
# plot(c1,c2, main = "c1 e c2")
# 
# ct <- 2 * c1+rnorm(1000,30,10)
# 
# hist(ct)
# plot(c1,ct, main = "c1 e ct")
# 
# ct <- 2 * c1+rnorm(1000,10,5)
# hist(ct)
# plot(c1,ct, main = "c1 e ct 2")
# 
# ct <- 2 * c1+rnorm(1000,5,2)
# hist(ct)
# plot(c1,ct, main = "c1 e ct 3")
# 
# ct <- rnorm(1000,30,10) - 2 * c1
# hist(ct)
# plot(c1,ct, main = "c1 e ct 4")
# 
# ct <- rnorm(1000,5,2) - 2 * c1
# hist(ct)
# plot(c1,ct, main = "c1 e ct 4")