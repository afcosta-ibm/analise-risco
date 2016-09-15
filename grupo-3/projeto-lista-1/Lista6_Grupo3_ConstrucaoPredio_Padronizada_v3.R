#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Início do Programa referente a Lista 6 - Análise de Riscos - Grupo 3 @@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#predio=function(){
  library(triangle)
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Declaração das matrizes que conterão os cenários @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #Matriz de eventos
  matEvt<-matrix(nrow=3000,ncol=6)
  #m2 <- cbind(1, 1:6)
  colnames(matEvt, do.NULL = FALSE)
  colnames(matEvt) <- c("s1prj20%","s2tpl30%","s5env10%","s6acab5%","s7final40%","s7final5%")  

  #Matriz de prazos
  matPrazo<-matrix(nrow=3000,ncol=25)
  colnames(matPrazo, do.NULL = FALSE)
  colnames(matPrazo) <- c("s1prj","s1prj20%mais","s2tpl","s2tpl30%mais","s3fund","s4esperaFundEstr","s4estrutp1","s4estrutp2","s4estrutp3","s4telhado","s5envopc1p1","s5envopc1p2","s5envopc1p3","s5env10%opc2p1","s5env10%opc2p2","s5env10%opc2p3","s6servp1","s6servp2","s6servp3","s6acabp1","s6acabp2","s6acabp3","s7final","s7final40%mais","s7final5%mais")  
  
  #Matriz de custos
  matCusto<-matrix(nrow=3000,ncol=15)
  colnames(matCusto, do.NULL = FALSE)
  colnames(matCusto) <- c("s1prjvalfixo","s1prjsemanaadicretrab","s2tplsemana","s3fundMobrasemana","s3fundmat","s4estrutpisosemana","s4estrutmat","s4telhado","s5envmatporpiso","s5envportaspiso1","s5envMobraopc1","s5envMobra10%opc2","s6acabespart","s6acabopulen","s7finalvalfixo")  
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #@@@@ Declaração das matrizes que conterão os cálculos dos prazos e custos da contrução do prédio @@@@
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  Ns = 3000
  #Matriz de calculo dos custos
  matCalc<-matrix(nrow=3000,ncol=8)
  colnames(matCalc, do.NULL = FALSE)
  colnames(matCalc) <- c("Projeto","Terraplan", "Fundacoes","Estrutura","Envoltoria","ServAcab", "Finalizacao", "Total")  

  #Matriz de calculo dos prazos  
  matCalcPrazo<-matrix(nrow=3000,ncol=13)
  colnames(matCalcPrazo, do.NULL = FALSE)
  colnames(matCalcPrazo) <- c("zProjeto","zTerraplan", "zFundacoes","EsperaFundEstr","zEstrutura","zComunEstrutEnv","zEnvoltoria","zEnvoltConsiderada","zComunEnvServAcab","zServAcab","zServAcabConsiderado", "zFinalizacao", "zTotal")  
  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Início da Carga da Matriz que conterá os cenários de eventos @@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
#CargaEventos<-function(){
    
    #sorteios das probabilidades de ocorrencia dos eventos
 
    #Seção 1 Fase Projeto - 20% Cliente exigir alteração
    s1prj20 <- rbinom(Ns,1,0.2)

    #Seção 2 Fase Terraplanagem - 30% Encontrar artefatos arqueológicos
    s2tpl30 <- rbinom(Ns,1,0.3)
    
    #Seção 5 Fase Envoltória - 10% Novo dono não aceitar o contrato
    s5env10 <- rbinom(Ns,1,0.1)
    
    #Seção 6 Fase Acabamento - 5% Acabamento escolhido pelo cliente ser vetado pela diretoria
    s6acab5 <- rbinom(Ns,1,0.05)

    #Seção 7 Fase Finalização - 40% de chances de ter que realizar acertos
    s7final40 <- rbinom(Ns,1,0.4)
    
    #Seção 7 Fase Finalização - 5% de chances de ter que realizar acertos novamente
    s7final5 <- rbinom(Ns,1,0.05)
    
    for (i in 1:Ns)
      {
        matEvt[i,"s1prj20%"] <- s1prj20[i]
        matEvt[i,"s2tpl30%"] <- s2tpl30[i] 
        matEvt[i,"s5env10%"] <- s5env10[i] 
        matEvt[i,"s6acab5%"] <- s6acab5[i] 
        matEvt[i,"s7final40%"] <- s7final40[i] 
        matEvt[i,"s7final5%"] <- s7final5[i] 
      }

    #return(0)
 #} #Fechamento da função de carga dos eventos
# --------------- Fim Funcao carga dos eventos --------------------
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@ Início da Carga da Matriz que conterá os cenários de prazo @@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
#    CarregaPrazos<-function(){


      library(triangle)
      for (i in 1:Ns)
      {
          #Prazos da Seção 1 - Projeto - A sigla "mais" indica que na ocorrência do evento a qtd de semanas deve ser somada ao tempo principal
          matPrazo[i,"s1prj"] <- rtriangle(1,14,21,16) 
          matPrazo[i,"s1prj20%mais"] <- rtriangle(1,3,6,4)
          
          #Prazos da Seção 2 - Terraplanagem - A sigla "mais" indica que na ocorrência do evento a qtd de semanas deve ser somada ao tempo principal
          matPrazo[i,"s2tpl"] <- rtriangle(1,3,7,4) 
          matPrazo[i,"s2tpl30%mais"] <- rtriangle(1,8,14,10) 
          
          #Prazos da Seção 3 - Fundações
          matPrazo[i,"s3fund"] <- rtriangle(1,6,8,7)
          
          #Prazos da Seção 4 - Estrutura
          matPrazo[i,"s4esperaFundEstr"] <- rtriangle(1,3,6,4) 
          matPrazo[i,"s4estrutp1"] <- rtriangle(1,4,6,4.5) 
          matPrazo[i,"s4estrutp2"] <- rtriangle(1,4,6,4.5)
          matPrazo[i,"s4estrutp3"] <- rtriangle(1,4,6,4.5)
          matPrazo[i,"s4telhado"] = rtriangle(1,7,10,8) 
          
          #Prazos da Seção 5 - Envoltoria - Não são prazos adicionais será "um ou outro", por isso as siglas "opc1" e "opc2"
          matPrazo[i,"s5envopc1p1"] <- rtriangle(1,7,9,8) 
          matPrazo[i,"s5envopc1p2"] <- rtriangle(1,7,9,8)
          matPrazo[i,"s5envopc1p3"] <- rtriangle(1,7,9,8)
          matPrazo[i,"s5env10%opc2p1"] <- rtriangle(1,6,11,8)
          matPrazo[i,"s5env10%opc2p2"] <- rtriangle(1,6,11,8)
          matPrazo[i,"s5env10%opc2p3"] <- rtriangle(1,6,11,8)
          
          #Prazos da Seção 6 - Serviços e acabamento
          matPrazo[i,"s6servp1"] <- rtriangle(1,8,13,10) 
          matPrazo[i,"s6servp2"] <- rtriangle(1,8,13,10)
          matPrazo[i,"s6servp3"] <- rtriangle(1,8,13,10) 
          matPrazo[i,"s6acabp1"] <- rtriangle(1,9,13,11)
          matPrazo[i,"s6acabp2"] <- rtriangle(1,9,13,11)
          matPrazo[i,"s6acabp3"] <- rtriangle(1,9,13,11)
          
          #Prazos da Seção 7 - Finalização - A sigla "mais" indica que na ocorrência do evento a qtd de semanas deve ser somada ao tempo principal
          matPrazo[i,"s7final"] <- 2 
          matPrazo[i,"s7final40%mais"] <- rtriangle(1,0.2,5,2) 
          matPrazo[i,"s7final5%mais"] <- rtriangle(1,0.5,1.5,1)           
      }

      
#} # Fechamento da Função que carrega os Prazos
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@ Início da Carga da Matriz que conterá os cenários de custo @@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    #CarregaCustos<-function(){
    
    library(triangle)
    for (i in 1:Ns)
    {
      
      #c("s1prjvalfixo","s1prjsemanaadicretrab","s2tplsemana","s3fundMobrasemana","s3fundmat","s4estrutpisosemana","s4estrutmat","s4telhado","s5envmat","s5envportaspiso1","s5envMobrafixo","s6acabespart","s6acabopulen","s7finalvalfixo")  

      #Prazos da Seção 1 - Projeto 
      matCusto[i,"s1prjvalfixo"] <- 160000 
      matCusto[i,"s1prjsemanaadicretrab"] <- 12000
      
      #Prazos da Seção 2 - Terraplanagem 
      matCusto[i,"s2tplsemana"] <- rtriangle(1,4200,4700,4500) 
 
      #Prazos da Seção 3 - Fundações
      matCusto[i,"s3fundMobrasemana"] <- rtriangle(1,2800,3500,3300) #conferir esse valor de 3500 porque está errado no texto da questão
      matCusto[i,"s3fundmat"] <- rtriangle(1,37000,40000,38500)
      
      #Prazos da Seção 4 - Estrutura
      matCusto[i,"s4estrutpisosemana"] <- rtriangle(1,4700,5500,5200) 
      matCusto[i,"s4estrutmat"] <- rtriangle(1,17200,18000,17500) 
      matCusto[i,"s4telhado"] = 172000 
      
      #Prazos da Seção 5 - Envoltoria - Não são custos adicionais será "um ou outro", por isso as siglas "opc1" e "opc2"
      matCusto[i,"s5envmatporpiso"] <- rtriangle(1,36000,40000,37000) 
      matCusto[i,"s5envportaspiso1"] <- 9800 
      matCusto[i,"s5envMobraopc1"] <- 197000 
      matCusto[i,"s5envMobra10%opc2"] <- 209000 
      
      #Prazos da Seção 6 - Serviços e acabamento
      matCusto[i,"s6acabespart"] <- rtriangle(1,92000,107000,95000) 
      matCusto[i,"s6acabopulen"] <- rtriangle(1,106000,114000,112000)
      
      #Prazos da Seção 7 - Finalização - A sigla "mais" indica que na ocorrência do evento a qtd de semanas deve ser somada ao tempo principal
      matCusto[i,"s7finalvalfixo"] <- 4000
    }
    
    
    #} # Fechamento da Função que carrega os custos
    
    #---------------- Fim Função que carrega os custos das fases da construção ---------------
    
  

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@ Início dos cálculos dos Custos da Obra @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    for (i in 1:Ns)
    {
      #=========================S1 Custo Projeto =================================
      #Primeiro somamos o custo fixo
      matCalc[i,"Projeto"]= matCusto[i,"s1prjvalfixo"]
      #Depois somamos o custo adicional se o evento tiver ocorrido
      if (matEvt[i,"s1prj20%"]==1){
        matCalc[i,"Projeto"]= matCalc[i,"Projeto"] + (matCusto[i,"s1prjsemanaadicretrab"] * matPrazo[i,"s1prj20%mais"])
      }
      #=========================S2 Custo Terraplanagem =================================
      #Custo inicial da terraplanagem
      matCalc[i,"Terraplan"]=  matCusto[i,"s2tplsemana"] * matPrazo[i,"s2tpl"]
      #Depois somamos o custo adicional se o evento tiver ocorrido
      if (matEvt[i,"s2tpl30%"]==1){
        matCalc[i,"Terraplan"]= matCalc[i,"Terraplan"] + (matCusto[i,"s2tplsemana"] * matPrazo[i,"s2tpl30%mais"])
      }
      #=========================S3 Custo Fundações =================================
      #Custo Mão de Obra
      matCalc[i,"Fundacoes"]= matCusto[i,"s3fundMobrasemana"] * matPrazo[i,"s3fund"]
      #Custo com Materiais
      matCalc[i,"Fundacoes"]= matCalc[i,"Fundacoes"] + matCusto[i,"s3fundmat"]
                                                
      #=========================S4 Custo Estrutura =================================
      #Custo Mão de obra para piso 1
      matCalc[i,"Estrutura"]= matCusto[i,"s4estrutpisosemana"] * matPrazo[i,"s4estrutp1"]
      #Custo Mão de obra para piso 2
      matCalc[i,"Estrutura"]= matCusto[i,"s4estrutpisosemana"] * matPrazo[i,"s4estrutp2"]
      #Custo Mão de obra para piso 3
      matCalc[i,"Estrutura"]= matCusto[i,"s4estrutpisosemana"] * matPrazo[i,"s4estrutp3"]
      #Custo Material
      matCalc[i,"Estrutura"]= matCalc[i,"Estrutura"] + matCusto[i,"s4estrutmat"]
      #Custo Telhado
      matCalc[i,"Estrutura"]= matCalc[i,"Estrutura"] + matCusto[i,"s4telhado"]
      
      #=========================S5 Custo Envoltoria =================================
      #"","","s5envMobraopc1","s5envMobra10%opc2"
      #Custo Material por piso
      matCalc[i,"Envoltoria"]= matCusto[i,"s5envmatporpiso"] * 3  # *3 porque são 3 pisos
      #Custo Portas piso 1
      matCalc[i,"Envoltoria"]= matCalc[i,"Envoltoria"] + matCusto[i,"s5envportaspiso1"] 
      #Custo Mão de Obra
      if (matEvt[i,"s5env10%"]==1){
        matCalc[i,"Envoltoria"]= matCalc[i,"Envoltoria"] + matCusto[i,"s5envMobra10%opc2"] 
      }
      else
      {
        matCalc[i,"Envoltoria"]= matCalc[i,"Envoltoria"] + matCusto[i,"s5envMobraopc1"]
      }
      
      #=========================S6 Custo Serviços e acabamentos =================================
      if (matEvt[i,"s6acab5%"]==1){
        matCalc[i,"ServAcab"] =  matCusto[i,"s6acabespart"] 
      }
      else
      {
        matCalc[i,"ServAcab"] = matCusto[i,"s6acabopulen"]
      }
      #=========================S7 Custo Finalização =================================
        matCalc[i,"Finalizacao"] =  matCusto[i,"s7finalvalfixo"] 
        
        
      #=========================Somando os custos de cada seção =================================
        matCalc[i,"Total"] = (matCalc[i,"Projeto"] + matCalc[i,"Terraplan"] + matCalc[i,"Fundacoes"] + matCalc[i,"Estrutura"] + matCalc[i,"Envoltoria"] + matCalc[i,"ServAcab"] + matCalc[i,"Finalizacao"])
        
    } #Fim do FOR Calculos custos
      
      
    
    
    
    
    
    
        #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Início dos cálculos dos prazos @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        for (i in 1:Ns)
        {
          
          #=========================S1 Prazo Projeto =================================
          #Primeiro somamos o custo fixo
          matCalcPrazo[i,"zProjeto"]= matPrazo[i,"s1prj"]
          #Depois somamos o prazo adicional se o evento tiver ocorrido
          if (matEvt[i,"s1prj20%"]==1){
            matCalcPrazo[i,"zProjeto"]= matCalcPrazo[i,"zProjeto"] + matPrazo[i,"s1prj20%mais"]
          }
          #=========================S2 Prazo Terraplanagem =================================
          #Prazo da terraplanagem
          matCalcPrazo[i,"zTerraplan"]=  matPrazo[i,"s2tpl"]
          #Depois somamos o prazo adicional se o evento tiver ocorrido
          if (matEvt[i,"s2tpl30%"]==1){
            matCalcPrazo[i,"zTerraplan"]= matCalcPrazo[i,"zTerraplan"] + matPrazo[i,"s2tpl30%mais"]
          }
          
          #=========================S3 Prazo Fundações =================================
          #Prazo Mão de Obra
          matCalcPrazo[i,"zFundacoes"]= matPrazo[i,"s3fund"]

          #=========================S4 Prazo Estrutura =================================
          #Prazo espera iniciar estrutura após fundações
          matCalcPrazo[i,"EsperaFundEstr"]=matPrazo[i,"s4esperaFundEstr"]
          #Prazo para construir a estrutura dos 3 pisos
          matCalcPrazo[i,"zEstrutura"]= matPrazo[i,"s4estrutp1"] + matPrazo[i,"s4estrutp2"] + matPrazo[i,"s4estrutp3"]
          #Prazo para construir o telhado
          matCalcPrazo[i,"zEstrutura"]= matCalcPrazo[i,"zEstrutura"] + matPrazo[i,"s4telhado"]
          #Prazo Comum Estrutura Envoltória
          matCalcPrazo[i,"zComunEstrutEnv"] = (matPrazo[i,"s4estrutp2"] -3) + matPrazo[i,"s4estrutp3"] + matPrazo[i,"s4telhado"]
          
          #=========================S5 Custo Envoltoria =================================
          #"s5envopc1","s5env10%opc2"
          if (matEvt[i,"s5env10%"]==1){
            matCalcPrazo[i,"zEnvoltoria"]= matPrazo[i,"s5env10%opc2p1"] + matPrazo[i,"s5env10%opc2p2"] + matPrazo[i,"s5env10%opc2p3"]
            #O tempo em que a atividade de Serviços e acabamento ocorrerá em paralelo com o acabamento e serviços é a soma dos prazos dos pisos 2 e 3
            matCalcPrazo[i,"zComunEnvServAcab"] = matPrazo[i,"s5env10%opc2p2"] + matPrazo[i,"s5env10%opc2p3"]
            }
          else
          {
            matCalcPrazo[i,"zEnvoltoria"]= matPrazo[i,"s5envopc1p1"] + matPrazo[i,"s5envopc1p2"] + matPrazo[i,"s5envopc1p3"]
            #O tempo em que a atividade de Serviços e acabamento ocorrerá em paralelo com o acabamento e serviços é a soma dos prazos dos pisos 2 e 3
            matCalcPrazo[i,"zComunEnvServAcab"] = matPrazo[i,"s5envopc1p2"] + matPrazo[i,"s5envopc1p3"]
          }
          
          if (matCalcPrazo[i,"zEnvoltoria"] > matCalcPrazo[i,"zComunEstrutEnv"])
          {
            matCalcPrazo[i,"zEnvoltConsiderada"]= matCalcPrazo[i,"zEnvoltoria"] - matCalcPrazo[i,"zComunEstrutEnv"]
          }
          else
          {
            matCalcPrazo[i,"zEnvoltConsiderada"]=0
          }
          #=========================S6 Custo Serviços e acabamento =================================
          #"s6serv","s6acab"
          #"zComunEnvServAcab","zServAcab","zServAcabConsiderado"
          
            matCalcPrazo[i,"zServAcab"]= (matPrazo[i,"s6servp1"] + matPrazo[i,"s6servp2"] + matPrazo[i,"s6servp3"] + matPrazo[i,"s6acabp1"] + matPrazo[i,"s6acabp2"] + matPrazo[i,"s6acabp3"])

            if (matCalcPrazo[i,"zServAcab"] > matCalcPrazo[i,"zComunEnvServAcab"])
              {
                 matCalcPrazo[i,"zServAcabConsiderado"]= matCalcPrazo[i,"zServAcab"] - matCalcPrazo[i,"zComunEnvServAcab"]
              }
            else
             {
              matCalcPrazo[i,"zServAcabConsiderado"]=0
             }
          #=========================S7 Custo Finalização =================================    
            matCalcPrazo[i,"zFinalizacao"] = 2

            #Acrescentando os  possíveis prazos de retrabalho
            if (matEvt[i,"s7final40%"]==1){
              
              matCalcPrazo[i,"zFinalizacao"] = matCalcPrazo[i,"zFinalizacao"] + matPrazo[i,"s7final40%mais"]
              
              if (matEvt[i,"s7final5%"]==1){
              
                  matCalcPrazo[i,"zFinalizacao"] = matCalcPrazo[i,"zFinalizacao"] + matPrazo[i,"s7final5%mais"]
                
              }
              
            }
          #=========================Somando os prazos de cada seção =================================
          matCalcPrazo[i,"zTotal"] = (matCalcPrazo[i,"zProjeto"] + matCalcPrazo[i,"zTerraplan"] + matCalcPrazo[i,"zFundacoes"] + matCalcPrazo[i,"zEstrutura"] + matCalcPrazo[i,"zEnvoltoria"] + matCalcPrazo[i,"zServAcab"] + matCalcPrazo[i,"zFinalizacao"])
          
 
          
      } #Fim do FOR Calculos prazos
        

    
#}  #Fechamento da Função Principal
    
plot(matCalcPrazo[,"zTotal"],matCalc[,"Total"])