C<- matrix(c(1,0,0,1),ncol=2)
x<-mvrnorm(1000, mu = c(0,0), Sigma = C)
plot(x[,1], x[,2])


C<- matrix(c(1,1,1,1),ncol=2)
x<-mvrnorm(1000, mu = c(0,0), Sigma = C)
plot(x[,1], x[,2])

C<- matrix(c(1,-0.5,-0.5,1),ncol=2)
x<-mvrnorm(1000, mu = c(0,0), Sigma = C)
plot(x[,1], x[,2])

C<- matrix(c(1,0.5,0.5,1),ncol=2)
x<-mvrnorm(1000, mu = c(0,0), Sigma = C)
plot(x[,1], x[,2])

