# para 3000 amostras, sortear um numero entre 16 e 22 e 
# calcular a soma do almoco.
# custo do almoco = numero executivos * custo do pedido

#usar o sample para amostrar o numero de sextas sample()

# sorteia o nro de sextas
# for do ano 
#   for do nro executivos
#     custo almoco

almoco <- function(){
  # number of samples
  NS <- 3000
  retorno <- vector(length = NS)
  
  for(i in 1:NS){
    # inicializa 
    custoTotalAlmoco <- 0
    
    numeroSextas <- sample(x = c(40, 41, 42), size = 1, replace = TRUE, prob = c(0.33, 0.33, 0.34))
    
    #print(numeroSextas)
    
    for(j in 1:numeroSextas){
      numeroExecutivos <- round(rtriangle(n = 1, a = 16, b = 22, c = 18))
      # acumula os valores dos almocos do ano
      custoTotalAlmoco <- custoTotalAlmoco + sum (rtriangle(n = numeroExecutivos, a = 25, b = 35, c = 28))
    }
    #print(custoTotalAlmoco)
    
    retorno[i] <- custoTotalAlmoco
  }
  
  return (retorno)
  
}