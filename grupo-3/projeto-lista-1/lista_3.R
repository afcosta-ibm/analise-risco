#LISTA 3 SIMULAÇÃO FATURAMENTO DIARIO USANDO O TCL
library(triangle)

valor<-rtriangle(360,40,120,60) 
fatu<-rtriangle(360,.15,.30,.22)
grupos<-rtriangle(3000,90,250,130)

med_valor<-mean(valor)
med_fatu<-mean(fatu)
med_grupos<-mean(grupos)

var_valor<-var(valor)
var_fatu<-var(fatu)
var_grupos<-var(grupos)

total <- med_grupos * med_fatu * med_grupos #FATURAMENTO para um dia
total360 = total * 360 #faturamento anual
total30= total * 30 #faturamento mensal
