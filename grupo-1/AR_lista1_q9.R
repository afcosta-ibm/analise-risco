#Suponha um fluxo de caixa com 20 valores, cada um deles seguindo uma triangular com valores

#(8,10,12) descontados a uma taxa de 1% por período. Obtenha uma aproximação empírica para a

#distribuição de probabilidade do VPL e compare com o resultado obtido usando o TCL.

#VP = soma (VFi/(1+t)^n)


#solução por TCL: VP_total ~ Normal (soma(media), soma(variancia))

library (triangle)

n = 20

t = 0.01

pmin = 8

pmax = 12

mprov = 10

media_fluxo = mprov
var_fluxo = (pmin^2+pmax^2+mprov^2-pmin*pmax-pmin*mprov-pmax*mprov)/18

k = NULL
VP = NULL
media = NULL
variancia  = NULL

fluxo = rtriangle(n, pmin, pmax, mprov)

for (i in 1:20){
  k[i] = ((1+t)^i)
  VP[i] = fluxo[i] / k[i]
  media[i] = media_fluxo/k[i]
  variancia[i] = var_fluxo/(k[i]^2)
}

media_total = sum(media)
variancia_total = sum(variancia)



#solução empírica:

#número de simulações:
n = 1000

VP_total = NULL

for (j in 1:n) {

  k = NULL
  VP = NULL
  
  fluxo = rtriangle(n, pmin, pmax, mprov)
  
  for (i in 1:20){
    k[i] = (1+t)^i
    VP[i] = fluxo[i] / k[i]
  }
  
  VP_total[j] = sum(VP) 

}

media_total2 = mean(VP_total)
variancia_total2 = var(VP_total)