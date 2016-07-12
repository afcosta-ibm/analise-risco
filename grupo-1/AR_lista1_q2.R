#Simule o lançamento simultâneo de 20 moedas.
#cara = 1
#coroa = 0
#sample(c(0,1), 10, replace=T, prob=c(0.5,0.5))

#Calcule a freqüência relativa do "número total de caras" para 10, 100 e 1000 lançamentos.
#n = 10

n = 10
total_caras1 = NULL

for (j in 1:n) {
  resultado1 = sample(c(0,1), 20, replace=T, prob=c(0.5,0.5))
  total_caras1[j] = sum(resultado1)
}
freq1 = sum(total_caras1)/n

#n = 100

n = 100
total_caras2 = NULL

for (j in 1:n) {
  resultado2 = sample(c(0,1), 20, replace=T, prob=c(0.5,0.5))
  total_caras2[j] = sum(resultado2)
}
freq2 = sum(total_caras2)/n

# n = 1000

total_caras3 = NULL
n = 1000

for (j in 1:n) {
  resultado3 = sample(c(0,1), 20, replace=T, prob=c(0.5,0.5))
  total_caras3[j] = sum(resultado3)
}
freq3 = sum(total_caras3)/n

#Calcule a média e a variância do "número total de caras".

#n = 10
  media1 = mean(total_caras1)
  variancia1 =  var(total_caras1)  
# n = 100
  media2 = mean(total_caras2)
  variancia2 =  var(total_caras2) 
# n = 1000
  media3 = mean(total_caras3)
  variancia3 = var(total_caras3)