#LISTA 4

library(triangle)

custo_total = NULL

for (i in 1:3000){

#PACOTE 1 - Planejamento inicial

#O valor estimado é de (15;17;19). O fato do Prefeito não dispor de maioria na Câmara 
#implica numa chance de 50% do plano inicial ser rejeitado, o que ocasionaria alterações 
#no projeto que elevariam este custo para (20;22;25).


  chance1 = rbinom(1, 1, 0.5)
  
  if (chance1 == 1) { #plano aceito na câmara
    est1 = rtriangle (1, 15, 19, 17)}
  
  else {est1 = rtriangle (1, 20, 25, 22)} #plano não aceito na câmara
  

#PACOTE 2 - Terraplanagem

#O cenário mais favorável, com 75% de chance, é aquele em que não serão encontrados 
#problemas de infiltração do lençol freático e o custo de terraplanagem estimado é de 
#(41;42;47). No caso de infiltração este custo subirá para (45;47;50).

  chance2 = rbinom(1, 1, 0.75)
  
  if (chance2 == 1) { #cenário mais favorável (sem infiltração)
    est2 = rtriangle (1, 41, 47, 42)}
  
  else {est2 = rtriangle (1, 45, 50, 47)} #cenário menos favorável (com infiltração)


#PACOTE 3 - Material

#custo estimado (100,105,110)

  est3 = rtriangle (1, 100, 110, 105)

#PACOTE 4 - Mão de Obra

#valor estimado (40,45,52)

  est4 = rtriangle (1, 40, 52, 45)

#PACOTE 5 - Aluguel de equipamentos

#custo estimado (35,36,40)

  est5 = rtriangle (1, 35, 40, 36)

#PACOTE 6 - Acabamento e jardinagem

#custo estimado (25,26,27)

  est6 = rtriangle (1, 25, 27, 26)

#PACOTE 7 - Administração da obra

#custo estimado (15,17,19)
  
  est7 = rtriangle (1, 15, 19, 17)
  
  custo_total[i] = est1 + est2 + est3 + est4 + est5 + est6 + est7
  
}

#Gráficos

hist(custo_total, main = "Histograma do Custo Total da Obra", xlab = "custo total")

perc = (seq(0,1,by=0.01))

x = quantile(custo_total, probs=perc)

plot(x,perc, type = 'l', col = "red", 
     main = "", xlab = "custo total", ylab = "percentil",
     xlim = c(min(x), max(x)))
d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc

#Risco de custo desta obra: d[86,1]

d[86,1]

abline(v = d[86,1], col = "gray")
abline(h = d[86,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.8)
text(d[86,1], d[90,2], "p:85%, custo total:USD$300,00", cex = 0.8, col = "blue")

#Sabendo que o orçamento liberado para o gerente do projeto foi definido como o valor médio
#da distribuição de custo, calcule o valor (em USD$ 1000) a ser alocado como reserva de
#contingência.

orc_liberado = mean(custo_total)

sd(custo_total)

min (x)
max(x)

contingencia = d[86,1]-orc_liberado
