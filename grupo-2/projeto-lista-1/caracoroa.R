caracoroa<-function(kc,lounch,repex){
  #kc: vetor de cara (1) e coroa (0)
  #lounch:lancamentos
  #repex: repeticao do experimento
  # Se louch e repex tendem a infinito, aproximamos da distribuição normal pelo TCL
  
  vex = 1:repex
  for (count in 1:length(vex)) {
  x <- sample(kc,lounch,replace=T,prob=(c(0.5,0.5)))
  vex[count] <- sum(x)
  #hist(x)
  # sum(x)
  
  }
  #plota o grafico e a media
  hist(vex, main = "Histograma do experimento",col = "lightblue", breaks = 100)
  abline(v=mean(vex),col="red",lwd=1)
  #plot(hist(vex))
  #hist(vex)
  #vex
}