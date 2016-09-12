#' Lista no. 6 – Risco de custo e prazo
#' Um novo prédio deve ser construído por um consorcio para um cliente. 
#' O projeto pode ser dividido em 7 seções como mostrado abaixo. 
#' O cliente deseja ver o resultado da análise de risco do prazo e do custo e 
#' da interdependência entre prazo e custo da obra.
#' 
#' Seção 1 – Projeto
#' O projeto detalhado demora (14,16,21) semanas, porém o arquiteto imagina que 
#' existe uma chance de 20% do cliente exigir alterações no projeto que 
#' implicam num prazo adicional de (3,4,6) semanas. O grupo de arquitetura 
#' cobra 160.000 fixo, mas requer um adicional de 12.000 por semana para 
#' qualquer tipo de retrabalho.
#' 
#' Seção 2 – Terraplanagem
#' O local deverá ser nivelado. Esta tarefa pode começar imediatamente após a 
#' assinatura do contrato. A terraplanagem vai demorar (3,4,7) semanas a um 
#' custo de (4.200,4.500,4.700) por semana. Existe um risco que a terraplanagem 
#' revele a presença de artefatos que irão requerer uma inspeção arqueológica 
#' complexa antes que o trabalho de construção possa continuar. Conhecimento 
#' sobre o local indica que existe uma probabilidade de 30% de encontrar 
#' artefatos arqueológicos, o que neste caso, implicaria em um tempo de 
#' inspeção de (8,10,14) semanas.
#' 
#' Seção 3 – Fundações
#' O trabalho de fundação pode ser iniciado assim que a terraplanagem termine 
#' e levará (6,7,8) semanas. O custo estimado é de (2.800,3.300,3.300) por 
#' semana para mão de obra e de (37.000,38.500,40.000) de materiais.
#' 
#' Seção 4 – Estrutura
#' Os componentes estruturais do prédio (pisos, pilares, cobertura) podem ser
#' iniciados, dependendo do tempo (3,4,6) semanas após o término do trabalho da
#' fundação. O prédio possui três pisos iguais, sendo que o construtor estima 
#' que cada um deles pode ser construído em (4,4.5,6) semanas dependendo das 
#' condições do tempo. Cada piso custa (4.700, 5.200, 5.500) por semana de 
#' trabalho e (17.200, 17.500, 18.000) de material, dependente do projeto final
#' detalhado. O telhado tomará (7,8,10) semanas por um preço fixo de 172.000.
#' 
#' Seção 5 – Envoltória
#' A trabalho na envoltória (paredes, janelas e portas externas)o de ser 
#' iniciado 3 semanas depois do primeiro piso ter sido terminado. A estimativa 
#' para o custo de material é de (36.000, 37.000, 40.000) por piso dependendo 
#' do projeto final de arquitetura. O piso térreo vai necessitar de portas de 
#' segurança que custam 9.800, a mão de obra da envoltória será fornecida por 
#' uma empresa de construção a um preço fixo de 197.000 que estima que cada 
#' piso deve demorar (7,8,9) semanas para ser completado, dependendo das 
#' condições do tempo. Entretanto como esta empresa foi comprada por uma 
#' empresa maior, existe uma chance de 10% que o novo dono não aceite o 
#' contrato. Neste caso, a solução alternativa é utilizar uma segunda empresa 
#' que apresentou uma cotação de 209.000 para o trabalho com uma estimativa de 
#' (6,8,11) semanas de duração para cada piso.
#' 
#' Seção 6 – Serviços e acabamento
#' Os serviços (encanamento, eletricidade, cabeamento para computadores) e o 
#' acabamento podem ser iniciados assim que cada piso estiver terminado. O 
#' arquiteto foi solicitado a fazer duas estimativas para o acabamento: a) 
#' um acabamento espartano e funcional e b) opulento e vistoso. O cliente 
#' deseja a opção (b) mas reconhece que existe uma chance de 5% dela ser 
#' vetada pelo Conselho Diretor da empresa. 
#' A opção espartana custa (92.000,95.000,107.000) enquanto que a opulenta 
#' custaria (106.000,112.000,114.000). Os serviços devem tomar (8,10,13) semanas
#' ,enquanto que o acabamento demora (9,11,13) para cada piso.
#' 
#' Seção 7- Finalização
#' São necessárias duas semanas depois de todo o trabalho ser completado para 
#' a limpeza do local e teste dos serviços a um custo de 4.000. Imagina-se que 
#' existe uma chance de 40% da empresa contratada para os serviços ser 
#' chamada para acertos que tomarão (0.2,2,5) semanas. Além disto, existe uma 
#' chance de 5% dela ser chamada uma segunda vez resultando em mais um atraso 
#' de (0.5,1,1.5) semanas.
#' 

library(triangle)
# library(igraph)

# importa as funcoes necessarias
source("grupo-1/projeto-lista-6/predio-secao-1.R")
source("grupo-1/projeto-lista-6/predio-secao-2.R")
source("grupo-1/projeto-lista-6/predio-secao-3.R")
source("grupo-1/projeto-lista-6/calcular-estrutura.R")
source("grupo-1/projeto-lista-6/calcular-envoltoria.R")
source("grupo-1/projeto-lista-6/calcular-servico-acabamento.R")
source("grupo-1/projeto-lista-6/calcular-finalizacao.R")

predio.Custo.Prazo <- predio_custo_prazo <- predioCustoPrazo <- 
  function(NumberOfSamples = 3000){
    
  print(NumberOfSamples)
  
  print(' montando os dados de custo, prazo e eventos da lista 6 ')
  # dimnames(sdsd) = list(1:3000, c("col1", "col2"))
  
  colunasMatriz <- c("projeto.custos", "projeto.prazos", 
                     "terraplanagem.custos", "terraplanagem.prazos",
                     "fundacoes.custos", "fundacoes.prazos",
                     "estrutura.custos", "estrutura.prazos",
                     "envoltoria.custos", "envoltoria.prazos",
                     "servico.acabamento.custos", "servico.acabamento.prazos",
                     "finalizacao.custos", "finalizacao.prazos")
    
  matrizCenarios <- matrix(nrow=14, ncol=NumberOfSamples, 
                           dimnames = list(colunasMatriz, 1:NumberOfSamples))
  
  # dimnames(matrizCenarios) = list(colunasMatriz, 1:NumberOfSamples)
  

  # calcula os dados da secao 1 :: Projeto
  secao1 <- predioSecao1(NumberOfSamples)
  matrizCenarios[1,] <- secao1[["custos"]]
  matrizCenarios[2,] <- secao1[["prazos"]]
  
  # calcula os dados da secao 2 :: Terraplanagem
  secao2 <- predioSecao2(NumberOfSamples)
  matrizCenarios[3,] <- secao2[["custos"]] 
  matrizCenarios[4,] <- secao2[["prazos"]] 
  
  # calcula os dados da secao 3 :: Fundacoes
  secao3 <- predioSecao3(NumberOfSamples)
  matrizCenarios[5,] <- secao3[["custos"]] 
  matrizCenarios[6,] <- secao3[["prazos"]] 
  
  # calcula os dados da secao 4 :: Estruturas
  secao4 <- calcularEstrutura(NumberOfSamples)
  matrizCenarios[7,] <- secao4[["custos"]] 
  matrizCenarios[8,] <- secao4[["prazos"]]
  
  # calcula os dados da secao 5 :: Envoltorias
  secao5 <- calcularEnvoltoria(NumberOfSamples)
  matrizCenarios[9,] <- secao5[["custos"]] 
  matrizCenarios[10,] <- secao5[["prazos"]]
  
  # calcula os dados da secao 6 :: Servicos e Acabamento
  secao6 <- calcularServicoAcabamento(NumberOfSamples)
  matrizCenarios[11,] <- secao6[["custos"]] 
  matrizCenarios[12,] <- secao6[["prazos"]]
  
  # calcula os dados da secao 7 :: Finalizacao
  secao7 <- calcularFinalizacao(NumberOfSamples)
  matrizCenarios[13,] <- secao7[["custos"]] 
  matrizCenarios[14,] <- secao7[["prazos"]]
  
  return (matrizCenarios)
  
}