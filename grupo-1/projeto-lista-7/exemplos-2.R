c1 <- rtriangle(1000,10,30,20)
c2 <- rtriangle(1000,20,60,40)

ct <- c1+c2
plot(c1,c2, main = "c1 e c2")

ct <- 2 * c1+rnorm(1000,30,10)

hist(ct)
plot(c1,ct, main = "c1 e ct")

ct <- 2 * c1+rnorm(1000,10,5)
hist(ct)
plot(c1,ct, main = "c1 e ct 2")

ct <- 2 * c1+rnorm(1000,5,2)
hist(ct)
plot(c1,ct, main = "c1 e ct 3")

ct <- rnorm(1000,30,10) - 2 * c1
hist(ct)
plot(c1,ct, main = "c1 e ct 4")

ct <- rnorm(1000,5,2) - 2 * c1
hist(ct)
plot(c1,ct, main = "c1 e ct 4")
