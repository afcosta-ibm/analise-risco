esc <- c(75,82.5,92.5,81.51)
fd <- c(57.5,67.5,77.5,71.95)
est <- c(430,445,472.5,443.26)
tel <- c(140,145,157.5,144.69)
acab <- c(72.5,92.5,107.5,88.35)

# med <- mean(c(mean(esc),mean(fd),mean(est),mean(tel),mean(acab)))

xmed <- mean(esc) + mean(fd) + mean(est) + mean(tel) + mean(acab)

#xvar <- sqrt(var(esc)^2 + var(fd)^2 + var(est)^2 + var(tel)^2 + var(acab)^2)
xvar <- var(esc) + var(fd) + var(est) + var(tel) + var(acab)
