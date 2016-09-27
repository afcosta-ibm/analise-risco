library(igraph)

#' monta o grafo
G <- make_graph(c(1,2, 1,3, 2,4, 3,4, 3,5, 4,6, 5,7, 6,7), directed = T)

print("aaa")

d <- c(1,4,5,7,2,1,1)

p<- all_simple_paths(G, from = 1, to = 7)

# pega o primeiro caminho do all_simple_path
print(p[[1]])

# pega o subset do vetor de duracao do primeiro caminho do all_simple_path
print(d[p[[1]]])

# faz a soma da duracao
print(sum(d[p[[1]]]))