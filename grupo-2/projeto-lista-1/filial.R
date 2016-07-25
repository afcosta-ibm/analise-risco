filial<-function(){
  k=30*12
  #maxlucro
  maxlc<-120*250*0.30*k
  minlc<-40*90*0.15*k
  avglc<-60*130*0.22*k
  
  muv<-(40+60+120)/3
  mugc<-(90+130+250)/3
  varv<-(40^2+60^2+120^2-40*60-40*120-60*120)/18
  vargc<-(90^2+130^2+250^2-90*130-90*250-130*250)/18
  # media de faturamento diario
  mufatb<-muv*mugc
  varfatb<-muv^2*vargc+mugc^2*varv
  
  # parametros  de lucro associado
  mulb<-(0.15+0.22+0.30)/3
  varlb<-(0.15^2+0.22^2+0.30^2-0.15*0.22-0.15*0.30-0.22*0.30)/18
  
  #Lucro dia
  mulcdia<-mufatb*mulb
  varlcdia<-mufatb^2*varlb+mulb^2*varfatb
  
  muyear<-k*mulcdia
  varyear<-k*varlcdia
  dpyear<-varyear^0.5
  
  #r<-list(muyear,dpyear,1-pnorm(maxlc,mean=muyear,sd=dpyear))
  r<-list(muyear,dpyear,1-pnorm(maxlc,mean=muyear,sd=dpyear),1-pnorm(minlc,mean=muyear,sd=dpyear),1-pnorm(avglc,mean=muyear,sd=dpyear))
  limitinf <- -3*dpyear
  limitsup <- 3*dpyear
  
  #y<-seq(limitinf,limitsup,minlc)
  #plot(y, pnorm(y), type="h",xlab = "Curva do experimento L1Ex4")
  #lines(y, pnorm(y), col="blue", lwd=1)
  
  curve(dnorm(x,mean=muyear,sd=dpyear), from=limitinf, to=limitsup, xlab = "Curva do Lucro Filial",col = "black",lwd=2)
  #x <- rnorm(1000, mean=muyear, sd=dpyear)
  #hist(x)
  abline(v=muyear,col="red",lwd=2)
  #abline(v=dpyear,col="blue",lwd=1)
  #abline(v=minlc,col="black",lwd=1)
  #abline(v=maxlc,col="red",lwd=1)
  
  #x<-0:1000
  #matriz com a probabilidades binomial e normal
  #y<-matrix(nrow=1001,ncol=2)
  #coluna 1: valores da binomial
  #y[,1]<-rnorm(1001, mean=muyear, sd=dpyear)
  #parametros da normal aproximanda
  #coluna 2: valores da normal
  #y[,2]<-dnorm(x,muyear,dpyear)
  #plotando as duas funçoes sobrepostas
  #matplot(x,y,type="l") 
  r
  
} 