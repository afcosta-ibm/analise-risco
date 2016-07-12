l1ex8<-function(amostra){
  
  #gera distribuição normal aletaorias 
  x1 <- rnorm(amostra, mean=0,sd=1)
  x2 <- rnorm(amostra, mean=0,sd=1)
  
  ind = which(x1 < x2)
  y = x1
  y[ind] <- x2[ind]
  
  #plota o grafico e a media
  hist(y, main = "Histograma do experimento L1Ex3",col = "lightblue", breaks = 100)
  abline(v=mean(y),col="red",lwd=1)
  abline(v=sd(y),col="black",lwd=1)
  
}