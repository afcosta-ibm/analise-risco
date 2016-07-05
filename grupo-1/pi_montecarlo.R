# Problema: calcular o valor de pi através de Montecarlo.

n=10000;# número de tiros

x=runif(n, -1, 1)
y=runif(n, -1, 1)
A=0;

#gráfico
plot.new()
plot.window(xlim = 1.1 * c(-1, 1), ylim = 1.1 * c(-1, 1))

for (i in 1 : n) {
  if (x[i]^2 + y[i]^2 <= 1) { #dentro do círculo
    A=A+1;
  
  points(x[i], y[i], pch = '.', col = "blue")
    
  }
  
  else    points(x[i], y[i], pch = '.', col = "red") #fora do círculo
}

pi_est = 4*A/n