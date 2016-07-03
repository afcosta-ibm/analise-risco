#'  Questão 1: Simule um jogo de cara ou coroa. 
#'    (i) Verifique a freqüência do número de caras com: 10, 100 e 1000 
#'    lançamentos. 
#'    (ii) Repita cada um dos experimentos 10 vezes e compare com os resultados 
#'    anteriores. 
#'  

foo <- function(throws, title, filename) {
  rounds <- 10
  x <- c(rep(0, rounds))
  for (i in 1:rounds) {
    x[i] <- throw.coin(throws)
  }
  draw(x, title, to.file = TRUE, filename)
}

foo(10, "For 10 throws (high variation)", "output/exerc1draw410.png")
foo(100, "For 100 throws (low variation)", "output/exerc1draw4100.png")
foo(1000, "For 1000 throws (very low variation)", "output/exerc1draw41000.png")
