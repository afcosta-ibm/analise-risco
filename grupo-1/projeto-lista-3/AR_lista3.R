#LISTA 3 - Análise de Risco

library(triangle)


#por simulação

lucro_anual = NULL
faturamento_diario = NULL
lucro_diario = NULL

for (i in 1:1000) {
  
  visita_diaria = rtriangle (360,40,120,60) #quantidade de grupos por dia
  gasto = rtriangle (360,90,250,130) #gasto por grupo
  faturamento_diario = visita_diaria*gasto #quanto o restaurante ganhou em um dia
  lucro = rtriangle (360,0.15,0.30,0.22) #porcentagem de lucro
  lucro_diario = faturamento_diario*lucro #lucro diário sem impostos
  lucro_anual[i] = sum(lucro_diario) #lucro anual
}

hist(lucro_anual)
mean(lucro_anual)
min(lucro_anual)
max(lucro_anual)