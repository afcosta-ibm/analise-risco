#Simule um jogo de cara ou coroa.

#(i) verifique a freqüência do número de caras com: 10,100 e 1000 lançamentos.

#cara = 1
#coroa = 0

#para n = 10

n = 10
resultado1 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
freq1 = sum(resultado1)/n

#para n = 100

n = 100
resultado2 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
freq2 = sum(resultado2)/n

# para n = 1000

n = 1000
resultado3 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
freq3 = sum(resultado3)/n

#(ii) repita cada um dos experimentos 10 vezes e compare com os resultados anteriores.

#para n = 10

n = 10
freq1 = NULL

for (i in 1:10){
  resultado1 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
  freq1[i] = sum(resultado1)/n
}

media1 = sum(freq1)/10

#n = 100
n = 100
freq2 = NULL

for (i in 1:10){
  resultado2 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
  freq2[i] = sum(resultado2)/n
}

media2 = sum(freq2)/10

#para n = 1000

n = 1000
freq2 = NULL

for (i in 1:10){
  resultado3 = sample(c(0,1), n, replace=T, prob=c(0.5,0.5))
  freq3[i] = sum(resultado3)/n
}

media3 = sum(freq3)/10