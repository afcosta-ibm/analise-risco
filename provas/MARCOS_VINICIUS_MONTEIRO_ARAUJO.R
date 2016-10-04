##############################################################
##      Prova Final da Disciplina de Analise de Risco       ##      
##      Professor Eber Schmitz                              ##
##      Data: 04 de Outubro de 2016                         ##
##      Aluno: Marcos Vinicius Monteiro Araujo              ##
##      Matricula: 115148343                                ##
##############################################################
prova = function(){
  Ns = 3000
  prob_ef = 0.7
  prob_bd = 0.4
  
  library(triangle)
  library(MASS)
  library(igraph)
  
  ##########Matriz que Sorteia a Duracao das Atividades#############
  SortMatrizDuracao = function(Ns =3000){
    matrizDuracao = matrix(0,nrow = Ns, ncol= 9)
    matrizDuracao[,1] = 0 #dummy inicial
    matrizDuracao[,2] = rtriangle(n = Ns, 1,4,2) #atividade A
    matrizDuracao[,3] = rtriangle(n = Ns, 5,7,6) #atividade B
    matrizDuracao[,4] = rtriangle(n = Ns, 2,5,4) #atividade C
    matrizDuracao[,5] = rtriangle(n = Ns, 1,4,3) #atividade D
    matrizDuracao[,6] = rtriangle(n = Ns, 4,7,5) #atividade E
    matrizDuracao[,7] = rtriangle(n = Ns, 3,5,4) #atividade F
    matrizDuracao[,8] = rtriangle(n = Ns, 1,3,2) #atividade G
    matrizDuracao[,9] = 0 #dummy final
    return(matrizDuracao)#retorna a matriz para a função Prova
  
  }##############fim da funcao SortMatrizDuracao##################
  
  
  ##########Matriz que Sorteia o Custo das Atividades#############
  SortMatrizCusto = function(Ns =3000){
    matrizCusto = matrix(0,nrow = Ns, ncol= 9)
    matrizCusto[,1] = 0 #dummy inicial
    matrizCusto[,2] = rtriangle(n = Ns, 75,110,90) #atividade A
    matrizCusto[,3] = rtriangle(n = Ns, 120,190,150) #atividade B
    matrizCusto[,4] = rtriangle(n = Ns, 60,85,75) #atividade C
    matrizCusto[,5] = rtriangle(n = Ns, 45,80,60) #atividade D
    matrizCusto[,6] = rtriangle(n = Ns, 160,240,200) #atividade E
    matrizCusto[,7] = rtriangle(n = Ns, 210,300,250) #atividade F
    matrizCusto[,8] = rtriangle(n = Ns, 85,140,120) #atividade G
    matrizCusto[,9] = 0 #dummy final
    return(matrizCusto)#retorna a matriz para a função Prova
    }############## Fim do SortMatrizCusto#######################
  
  #############Sorteia probalidade da atividade F Ocorrer#########
  SortProbA_F = function(Ns =3000){
    probOcorAtivF = rbinom(Ns,1,0.25)
    return(probOcorAtivF)#retorna a matriz para a função Prova
  }####################Fim do SorTProbA_F###########################
  
  ##################GERA OS VALORES CORRELACIONADOS ENTRE E E F DO DURACAO##########
  correEFDuracao = function(Ns = 3000){
    A=matrix(c(1,0.7,0,1),ncol=2)
    m<-rep(0,times=2)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns,ncol=2)
    Tri[,1]<-qtriangle(U[,1],4,7,5)#atividade E DURACAO
    Tri[,2]<-qtriangle(U[,2],3,5,4)#atividade F DURACAO
    plot(Tri[,1],Tri[,2], main = "Correlação entre E e F na Duração", ylab = "ATIVIDADE F", xlab = "ATIVIDADE E")
    return(Tri)
  }############################FIM DA CORREEeF##################################
  
  #######################GERA OS VALORES CORRELACIONADOS ENTRE B E D do DURACAO##########
  correBDDuracao = function(Ns = 3000){
    A=matrix(c(1,0.4,0,1),ncol=2)#Matriz de Correlação
    m<-rep(0,times=2)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns,ncol=2)
    Tri[,1]<-qtriangle(U[,1],5,7,6)#atividade B DURACAO
    Tri[,2]<-qtriangle(U[,2],1,4,3)#atividade D DURACAO
    plot(Tri[,1],Tri[,2],main = "Correlação entre B e D na Duração", ylab = "ATIVIDADE D", xlab = "ATIVIDADE B")
    return(Tri)#
  }##############################FIM DA CORREBD#######################################
  
  ##################GERA OS VALORES CORRELACIONADOS ENTRE E E F DO CUSTO##############
  correEFCusto = function(Ns = 3000){
    A=matrix(c(1,0.7,0,1),ncol=2)
    m<-rep(0,times=2)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns,ncol=2)
    Tri[,1]<-qtriangle(U[,1],160,240,200)#atividade E CUSTO
    Tri[,2]<-qtriangle(U[,2],210,300,250)#atividade F CUSTO
       plot(Tri[,1],Tri[,2], main = "Correlação entre E e F no CUSTO", ylab = "ATIVIDADE F", xlab = "ATIVIDADE E")
    return(Tri)
  }########################FIM DA CORREEeF###############################################
  
  ##################GERA OS VALORES CORRELACIONADOS ENTRE B E D do CUSTO################
  correBDCusto = function(Ns = 3000){
    A=matrix(c(1,0.4,0,1),ncol=2)#Matriz de Correlação
    m<-rep(0,times=2)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns,ncol=2)
    Tri[,1]<-qtriangle(U[,1],120,190,150)#atividade B CUSTO
    Tri[,2]<-qtriangle(U[,2],45,80,60)#atividade D CUSTO
    plot(Tri[,1],Tri[,2],main = "Correlação entre B e D no CUSTO", ylab = "ATIVIDADE D", xlab = "ATIVIDADE B")
    return(Tri)#
  }###############################FIM DA CORREBD#######################################
  
  #####controi o grafo e lista os caminhos possiveis
  grafo = function(){
  g=c(1,2,1,3,1,4,2,5,2,6,3,8,4,7,7,8,8,9,6,8)
  x=make_graph(g)#constroi o grafo
  #elos = c(2,4,6,8,10,14,7)
  
  path=all_simple_paths(x,from=1,to=9)#todos os caminhos possiveis do grafo
  #ed = c(2,4,6,8,10,14,7)
  print (list(path))
  w = length(path)
  #for (i in 1:w){
    #gasto_grafo=sum(elos[path[[i]]])#calcula o grafo so para o primeiro caminho
    #print (gasto_grafo)
  #}
  #soma os elos com o caminho
  plot(x,main="Grafo de Atividades")#plota o grafo
  print ("possibilidade de caminhos",w)
  #
  
  }###########################FIM DO GRAFO##############################
  
  
  matrizDuracao = SortMatrizDuracao()#funcao que sorteia a matriz de duracao
  matrizCusto = SortMatrizCusto()#funcao que sorteia a matriz de custo
  ocorrerAtivF = SortProbA_F()#funcao que sortea probabilida de ocorrer a atividade F
  
  tri_EeF_duracao = correBDDuracao()#Corrige a correlação entre E e F Duracao
  tri_BeD_duracao = correBDDuracao()#Corrige a correlação entre B e D Duracao
  
  tri_EeF_custo = correEFCusto()#Corrige a correlação entre E e F CUSTO
  tri_BeD_custo = correBDCusto()#Corrige a correlação entre B e D CUSTO
  
  #####CORRIGIR AS COLUNAS DA MATRIZ DE DURACAO######
  matrizDuracao[,6] = tri_EeF_duracao[,1]
  matrizDuracao[,7] = tri_EeF_duracao[,2]
  ##########FIM da CORRECAO DA MATRIZ DE DURACAO#####
  
  #####CORRIGIR AS COLUNAS DA MATRIZ DE CUSTO######
  matrizCusto[,3] = tri_EeF_duracao[,1]
  matrizCusto[,5] = tri_EeF_duracao[,2]
  ##########FIM da CORRECAO DA MATRIZ DE CUSTO#####
  
  grafo = grafo()####Plota o grafo######
  
  #################Criar a Matriz de Totais###########################
  matrizTotal = matrix(0,ncol = 5, nrow = Ns)
  for (i in 1:Ns){
  if (ocorrerAtivF[i] == 0){
    matrizTotal[i,1] = sum(matrizDuracao[i,2],matrizDuracao[i,3],matrizDuracao[i,4],matrizDuracao[i,5],matrizDuracao[i,6],matrizDuracao[i,8])
    matrizTotal[i,2] = sum(matrizCusto[i,2],matrizCusto[i,3],matrizCusto[i,4],matrizCusto[i,5],matrizCusto[i,6],matrizCusto[i,8])
  }else{
    matrizTotal[i,1] = sum(matrizDuracao[i,])
    matrizTotal[i,2] = sum(matrizCusto[i,])
  }
    
  }
  #################Fim da Matriz de Totais###########################
  ######
  plot(ecdf(matrizTotal[,1]),main = "Acumulada da Duração")#plotar a Duração
  abline(h = 0.85,col="blue")
  abline(v = quantile(matrizTotal[,1],0.85),col="red")
  #######
  ######
  plot(ecdf(matrizTotal[,2]),main = "Acumulada do Custo")#plotar o Custo
  abline(h = 0.85,col="blue")
  abline(v = quantile(matrizTotal[,2],0.85),col="red")
  #######
  
  #####
  plot(matrizTotal[,1],matrizTotal[,2], main = "Relação entre Custo e Prazo", ylab = "Custo",xlab = "Prazo")
  ######
  
  ####tornado DURACAO###########
  Ni = 6
  cont = FALSE
  rs = matrix(0,nrow = Ns,ncol = 7)
  rs[,1] = matrizDuracao[,2]
  rs[,2] = matrizDuracao[,3]
  rs[,3] = matrizDuracao[,4]
  rs[,4] = matrizDuracao[,5]
  rs[,5] = matrizDuracao[,6]
  rs[,6] = matrizDuracao[,7]
  rs[,7] = matrizDuracao[,8]
  ev<-vector(length=Ns)
  ev<-rbinom(Ns,1,0.4)
  #Contingenciando
  if (cont){
    rs[,Ni]<-rs[,Ni]*ev}
  
  #totalizando
  for (j in 1:Ni){
    rs[,Ni+1]<-rs[,Ni+1]+rs[,j]
  }
  
  #Coeficiente de Correlação
  c<-cor(rs[,Ni+1],rs[,1:Ni],method="spearman")
  
  #normalization
  c<-c/sum(c)
  m<-matrix(c(1:Ni,c),ncol=2)
  #preparando tornado plot
  o<-order(m[,2])
  yname<-m[,1][o]
  barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
          names.arg=yname, main = "Analise da Sensibilidade da Duracao",
          xlab="Coeficiente de Correlação Normalizado",ylab="Atividades")
  
  #########################fim Tornado DURACAO#########################
  
  
  ###############################tornado CUSTO###########################
  Ni = 6
  cont = FALSE
  rs = matrix(0,nrow = Ns,ncol = 7)
  
  rs[,1] = matrizCusto[,2]
  rs[,2] = matrizCusto[,3]
  rs[,3] = matrizCusto[,4]
  rs[,4] = matrizCusto[,5]
  rs[,5] = matrizCusto[,6]
  rs[,6] = matrizCusto[,7]
  rs[,7] = matrizCusto[,8]
  ev<-vector(length=Ns)
  ev<-rbinom(Ns,1,0.4)
  #Contingenciando
  if (cont){
    rs[,Ni]<-rs[,Ni]*ev}
  
  #totalizando
  for (j in 1:Ni){
    rs[,Ni+1]<-rs[,Ni+1]+rs[,j]
  }
  
  #evaluate correlation coefficients
  c<-cor(rs[,Ni+1],rs[,1:Ni],method="spearman")
  
  #normalization
  c<-c/sum(c)
  m<-matrix(c(1:Ni,c),ncol=2)
  #preparando tornado plot
  o<-order(m[,2])
  yname<-m[,1][o]
  barplot(m[,2][o],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
          names.arg=yname, main = "Analise da Sensibilidade do Custo",
          xlab="Coeficiente de Correlação Normalizado",ylab="Atividades")
  
  #########################fim Tornado Custo#################################
  
  return(matrizTotal)#retorna a matriz total como saida da função principal########

}#####Fim da Funcao Prova#####