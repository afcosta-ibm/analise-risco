#SIMULAÇÃO FOMULA DE APROXIMAÇÃO DE VA'S 
med_grupos<-(90 + 250 + 120)/3
med_valor<-(40 + 120 + 60)/3
med_fatu<-(0.15 + 0.30 + 0.22)/3
total <- med_grupos * med_fatu * med_valor

media_aprox<-med_grupos * med_fatu * med_valor


var_grupos<-(90^2 + 250^2 + 120^2 -(90 * 250) -(90 * 120) -(250*120))/18
var_valor<-(40^2 + 120^2 + 60^2 -(40 * 120) -(40* 60) -(60*120))/18
var_fatu<-(0.15^2 + 0.30^2 + 0.22^2 -(0.15 * 0.30)-(0.15*0.22)-(0.30*0.22))/18

desvio_aprox <-var_grupos ^ 0.5
#LISTA 3 SIMULAÇÃO GASTO DIARIO USANDO O TCL
library(triangle)

valor<-rtriangle(30,40,120,60)
fatu<-rtriangle(30,.15,.30,.22)
grupos<-rtriangle(3000,90,250,130)

med_valor<-mean(valor)
med_fatu<-mean(fatu)
med_grupos<-mean(grupos)


var_valor<-var(valor)
var_fatu<-var(fatu)
var_grupos<-var(grupos)
desvio_sim<-var_grupos^0.5

total <- med_grupos * med_fatu * med_grupos
media_simu<- total
total360 = total * 360 #faturamento anual
total30= total * 30 #faturamento mensal

