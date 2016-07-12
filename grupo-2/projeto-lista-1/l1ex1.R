l1ex1<-function(lounch,repex){
  #probabilidade de cara (1) e coroa (0) = 0.5
  #Por exemplo, para gerar dez números aleatórios de uma distribuição binomial 
  #com 20 tentativas probabilidade 0,63.
  #rbinom(10, size = 20, prob = 0.63) 
  
  x<- rbinom(lounch,repex,0.5)
  
  #plota o grafico e a media
  hist(x, main = "Histograma do experimento L1Ex1",col = "lightblue", breaks = 100)
  abline(v=mean(x),col="red",lwd=1)
  
}