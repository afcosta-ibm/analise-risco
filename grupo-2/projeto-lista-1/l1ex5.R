l1ex5<-function(amostra){
  
  #gera distribuição uniforme de 12 variaveis aletaorias 
  x <- rtriangle(amostra,1,5,3)
  
  #gera distribuição normal com media e sd da triangle
  y<-rnorm(amostra, mean=mean(x),sd=sd(x))
  
  #plota o grafico e a media
  hist(y, main = "Histograma do experimento L1Ex3",col = "lightblue", breaks = 100)
  abline(v=mean(y),col="red",lwd=1)
  abline(v=sd(y),col="black",lwd=1)
  
  
}