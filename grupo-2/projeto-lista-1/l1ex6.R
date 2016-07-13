l1ex6<-function(amostra){
  
  #gera distribuição uniforme de 12 variaveis aletaorias 
  x <- rtriangle(amostra,1,5,3)
  
  #gera distribuição normal com media e sd da triangle
  #y<-rnorm(amostra, mean=mean(x),sd=sd(x))
  
  #plota o grafico e a media
  hist(x, main = "Histograma do experimento L1Ex3",col = "lightblue", breaks = 100)
  abline(v=mean(x),col="red",lwd=1)
  abline(v=var(x),col="black",lwd=1)
  
  
}