filial2<-function(){
  library (triangle)
  #simulando 1 dia de movimento
  k<-30*12
  Ns<-1000
  r<-vector(length=Ns)
  #visitantes diarios
  v<-rtriangle(Ns,40,120,60)
  #gasto dos clientes
  gc<-rtriangle(Ns,90,250,130)
  #estimativa de lucro
  lb<-rtriangle(Ns,0.15,0.30,0.22)
  
  #faturamento bruto anual
  fat<-v*gc*k
  
  #lucro bruto
  lbd<-fat*lb
  #maxlucro
  maxlc<-120*250*0.30*k
  minlc<-40*90*0.15*k
  avglc<-60*130*0.22*k
  
  mulc<-mean(lbd)
  varlc<-var(lbd)
  dplc<-varlc^0.5
  #myear<-k*mulc
  #vyear<-k*varlc
  #dpyear<-vyear^0.5
  r<-list(mulc,dplc,1-pnorm(maxlc,mean=mulc,sd=dplc),1-pnorm(minlc,mean=mulc,sd=dplc),1-pnorm(avglc,mean=mulc,sd=dplc))
  x<-seq(-3*dplc,3*dplc,minlc)
  #plot(x, pnorm(x), type="h")
  #curve(dnorm(x), from=-3*dplc, to=3*dplc, xlab = "Curva do Lucro Filial",col = "black",lwd=2)
  hist(lbd, main = "Histograma do Lucro Filial",col = "lightblue", breaks = 100)
  abline(v=mulc,col="red",lwd=2)
  #abline(v=minlc,col="black",lwd=1)
  #abline(v=maxlc,col="black",lwd=1)
  abline(v=avglc,col="blue",lwd=1)
  r
} 