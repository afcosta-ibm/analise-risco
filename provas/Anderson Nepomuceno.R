#Atividades:
#Dummy para o início e fim tem custo e prazo iguais a zero
#Atividade "F" tem com uma probabilidade 0.25 de ser executada
#o coeficiente de correlação entre as atividades E e F é 0.7 e entre as atividades B e D é de 0.4.
  
testefinal<-function(NS=1000){
    library(triangle)
    Ns<-3000 
    
#importar funções
    
    projeto.teste.final<-projeto_teste_final<-
      projetotestefinal<-function(amostras=1000){
        
        print(amostras)
        
        print('montando os dados de duração e custos')
        
#monta as colunas da matriz com os nomes corretos
colunasAtividades <-c('a1','b5','c2','d1','e4','f3','g1')

#inicia matriz de custos
matrizcustos <- matrix(nrow = amostras, ncol = 7)


matrizcustos=matrix(nrow = amostras, ncol = 7)
  matrizcustos[,1] <- rtriangle(amostras, 75, 110, 90)
  matrizcustos[,2] <- rtriangle(amostras, 120, 190, 150)
  matrizcustos[,3] <- rtriangle(amostras, 60, 85, 75)
  matrizcustos[,4] <- rtriangle(amostras, 45, 60, 80)
  matrizcustos[,5] <- rtriangle(amostras, 160, 240, 200)
  matrizcustos[,6] <- rtriangle(amostras, 210, 300, 250)
  matrizcustos[,7] <- rtriangle(amostras, 85, 140, 120)
  matrizcustos[,"inicio"]<-0 #inicio
  matrizcustos[,"fim"]<-0 #fim 
  matrizcustos[,"eventof"]<-rbinom(ns,1,0.25)
 
  
  #inicia matriz de duração
  matrizduracao <- matrix(nrow = amostras, ncol = 7)
  
matrizcustos=matrix(nrow = amostras, ncol = 7)
  matrizduracao[,1] <- rtriangle(amostras, 1, 4, 2)
  matrizduracao[,2] <- rtriangle(amostras, 5, 7, 6)
  matrizduracao[,3] <- rtriangle(amostras, 2, 5, 4)
  matrizduracao[,4] <- rtriangle(amostras, 1, 4, 3)
  matrizduracao[,5] <- rtriangle(amostras, 4, 7, 5)
  matrizduracao[,6] <- rtriangle(amostras, 3, 5, 4)
  matrizduracao[,7] <- rtriangle(amostras, 1, 3, 2)
  matrizduracao[,"inicio"]<-0 # Início
  matrizduracao[,"fim"]<-0 # Fim
  matrizduracao[,"evento"]<- matrizduracao[,"eventof"]
  
  #fim
  
  