kcq2<-function(kc,lounch){
  #kc: vetor de cara (1) e coroa (0)
  #lounch:lancamentos
  #numcoin: repeticao do experimento
  
  vex = 1:lounch
  for (count in 1:length(vex)) {
    x <- sample(kc,lounch,replace=T,prob=rep(1/(2^20), times=20))
    vex[count] <- sum(x)
    #hist(x)
    # sum(x)
    
  }
  
  cat("Media =",print(mean(vex), digits=3, zero.print = "."),"\n")
  cat("Desvio Padrao =",print(sd(vex), digits=3, zero.print = "."),"\n")
  cat("Mediana =",print(median(vex), digits=3, zero.print = "."),"\n")
  cat("Variancia =",print(var(vex), digits=3, zero.print = "."),"\n")
  hist(vex, main = "Histograma do experimento",col = "lightblue", breaks = 100)
  abline(v=mean(vex),col="red",lwd=1)
  #plot(hist(vex))
  #hist(vex)
  #vex
}