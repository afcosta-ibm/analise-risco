#' Lista no. 7 – Risco de prazo com correlacao
#' 
#' Gera o grafo da lista 7
#' 

library(igraph)

# importa as funcoes necessarias

gera.grafo <- gera_grafo <- geraGrafo <- function(){

  #' define os caminhos de cada certice para montar o grafo
  #' PS - Projeto de Sistema
  caminhosPS <- c(1,2, 1,3, 1,4, 1,5)
  #' PM - Projeto de Modulo
  caminhosPM <- c(2,6, 3,7, 4,8, 5,9)
  #' CM - Construcao de Modulo
  caminhosCM <- c(6,10, 7,11, 8,12, 9,13)
  #' TM - Teste de Modulo
  caminhosTM <- c(10,14, 11,14, 12,14, 13,14)
  #' I - Integracao
  caminhosI <- c(14,15)
  #' TS - Teste do Sistema
  caminhosTS <- c(15,16)
  #' TA - Teste de Aceitacao
  caminhosTA <- c(16,17, 16,21)
  #' R1 - Retrabalho 1
  caminhosR1 <- c(17,18)
  #' TSR1 - Teste de Sistema do Retrabalho 1
  caminhosTSR1 <- c(18,19, 18,21)
  #' R2 - Retrabalho 2
  caminhosR2 <- c(19,20)
  #' TSR1 - Teste de Sistema do Retrabalho 2
  caminhosTSR2 <- c(20,21)
  
  #' concatena os nos para montar os vertices
  edgesG <- c(caminhosPS, caminhosPM, caminhosCM, caminhosTM, caminhosI,
              caminhosTS, caminhosTA, caminhosR1, caminhosTSR1, caminhosR2, 
              caminhosTSR2)

  #' monta o grafo
  G <- make_graph(edges = edgesG, directed = T)

  #' plota o grafo
  plot(G,
       layout=layout_nicely,
       vertex.size=15, 
       vertex.label.color="white",
       vertex.color="blue", 
       edge.arrow.size=0.1)

  #' lista todos os caminhos do grafo de 1 .. 21
  caminhos <- all_simple_paths(graph = G, from = 1, to = 21)

  #' printa os caminhos
  # print(caminhos)
  
  return(caminhos)

}