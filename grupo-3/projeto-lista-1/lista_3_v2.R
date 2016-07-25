#SIMULAÇÃO FOMULA DE APROXIMAÇÃO DE VA'S 
med_grupos<-(90 + 250 + 120)/3
med_valor<-(40 + 120 + 60)/3
med_fatu<-(0.15 + 0.30 + 0.22)/3
total <- med_grupos * med_fatu * med_valor

var_grupos<-(90^2 + 250^2 + 120^2 -(90 * 250) -(90 * 120) -(250*120))/18
var_valor<-(40^2 + 120^2 + 60^2 -(40 * 120) -(40* 60) -(60*120))/18
var_fatu<-(0.15^2 + 0.30^2 + 0.22^2 -(0.15 * 0.30)-(0.15*0.22)-(0.30*0.22))/18

x <- 1:360
desvio_valor = var_valor ^ 0.5
desvio_grupos = var_grupos ^ 0.5
y = rnorm(360,med_grupos,desvio_grupos)
plot(x,y,"l")