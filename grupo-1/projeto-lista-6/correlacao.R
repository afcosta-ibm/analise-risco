#' Lista no. 6 – Risco de custo e prazo

#' Grupo 1:
#' Alexandre Filgueiras Costa
#' Cristianna Madeira de Ferran
#' Eduardo Chiote
#' Stella Queiroz

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-custo-prazo.R")

#'-------------------------------------------------------------------------

listaMatrizes <- predioCustoPrazo(3000)

matrizCustos <- listaMatrizes[["custos"]]
matrizPrazos <- listaMatrizes[["prazos"]]
matrizEventos <- listaMatrizes[["eventos"]]

#====================================================================#
somaCustos <- colSums(matrizCustos)
somaPrazos <- colSums(matrizPrazos)

plot(x = somaCustos, y = somaPrazos, 
     xlab = "custo total", ylab = "prazo total", 
     main = "Correlação custo e prazo Total")

#====================================================================#
# secao 1 :: Projeto
custoSecaoProjeto <- matrizCustos["projeto.custos",]
prazoSecaoProjeto <- matrizPrazos["projeto.prazos",]

plot(x = custoSecaoProjeto, y = prazoSecaoProjeto, 
     xlab = "custo do projeto", ylab = "prazo do projeto", 
     main = "Correlação custo e prazo da seção 1 :: Projeto")
#====================================================================#

#====================================================================#
# secao 2 :: Terraplanagem
custoSecaoTerraplanagem <- matrizCustos["terraplanagem.custos",]
prazoSecaoTerraplanagem <- matrizPrazos["terraplanagem.prazos",]

plot(x = custoSecaoTerraplanagem, y = prazoSecaoTerraplanagem, 
     xlab = "custo da Terraplanagem", ylab = "prazo da Terraplanagem", 
     main = "Correlação custo e prazo da seção 2 :: Terraplanagem")
#====================================================================#

#====================================================================#
# secao 3 :: Fundações
custoSecaoFundacoes <- matrizCustos["fundacoes.custos",]
prazoSecaoFundacoes <- matrizPrazos["fundacoes.prazos",]

plot(x = custoSecaoFundacoes, y = prazoSecaoFundacoes, 
     xlab = "custo da Fundação", ylab = "prazo da Fundação", 
     main = "Correlação custo e prazo da seção 3 :: Fundação")
#====================================================================#

