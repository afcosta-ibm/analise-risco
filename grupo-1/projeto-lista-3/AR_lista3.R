#LISTA 3 - Análise de Risco

library(triangle)


#por simulação

lucro_anual = NULL
faturamento_anual = NULL

for (i in 1:3000) {

    visita_diaria = rtriangle (360,40,120,60) #quantidade de grupos por dia
    gasto = rtriangle (360,90,250,130) #gasto por grupo
    faturamento_diario = visita_diaria*gasto #quanto o restaurante ganhou em um dia
    lucro = rtriangle (360,0.15,0.30,0.22) #porcentagem de lucro
    lucro_diario = faturamento_diario*lucro # % delucro diário sem impostos
    faturamento_anual[i] = sum(faturamento_diario)
    lucro_anual[i] = sum(lucro_diario) #lucro anual
}

w = lucro_anual/faturamento_anual

#histogramas
par(mfrow=c(1,3)) 
hist(faturamento_anual, xlab = "faturamento anual", main = "")
hist(lucro_anual, xlab = "lucro anual", main = "Histogramas")
hist(w, xlab = "porcentagem de lucro", main = "")

mean(lucro_anual)
mean(faturamento_anual)
mean(w)

sd(lucro_anual)
sd(faturamento_anual)
sd(w)

#gráfico das porcentagens de lucro


perc = (seq(0,1,by=0.01))
x = quantile(w, probs=perc)

plot(x,perc, type = 'l', col = "red", 
     main = "", xlab = "porcentagem de lucro", ylab = "percentil",
     xlim = c(min(x), max(x)))
d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc

abline(v = d[86,1], col = "gray")
abline(h = d[86,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.8)
text(d[86,1], d[89,2], "p:85%, lucro:22,5%", cex = 0.7, col = "blue")
min (x)
max(x)

#gráfico dos faturamentos

x = quantile(faturamento_anual, probs=perc)
plot(x,perc, type = 'l', col = "red", 
     main = "", xlab = "faturamento anual", ylab = "percentil",
     xlim = c(min(x), max(x)))
d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc
d[86,]
abline(v = d[86,1], col = "gray")
abline(h = d[86,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.8)
text(d[86,1], d[89,2], "p:85%, fat.:CN$4,2mi", cex = 0.7, col = "blue")
min (x)
max(x)

#gráfico dos lucros

x = quantile(lucro_anual, probs=perc)
plot(x,perc, type = 'l', col = "red", 
     main = "", xlab = "lucro anual", ylab = "percentil",
     xlim = c(min(x), max(x)))
d = matrix(nrow = length(perc), ncol = 2 )
d[,1] = x
d[,2] = perc
d[86,]
abline(v = d[86,1], col = "gray")
abline(h = d[86,2], col = "gray")
points (d[86,1], d[86,2], col = "green", cex = 0.8)
text(d[86,1], d[89,2], "p:85%, fat.:CN$941k", cex = 0.7, col = "blue")
min (x)
max(x)
