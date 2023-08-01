library(readxl)
library(openxlsx)

####Leitura do arquivo##########################################################

setwd("C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Artigos/Dados") #alterar diretorio para seu endereco


dorig = readxl::read_xlsx(path = "Base2.xlsx") #Base1 para estudo de caso 1 (Caetite) e Base2 para estudo de caso 2 (Santa Clara e AssuV)

#### Modelagem ###############################################################

i = 1
centroides<-list()
clusters<-list()
dorig$cluster<-''
dorig$cl_GE<-''
dorig$cl_GS<-''

start.time<-Sys.time()

for (m in 1:12){
  for (h in 0:23){
    clusterizacao <- clusterizar(dorig, m, h) #funcao para clusterizacao
    centroides[i]<-list(clusterizacao[1])
    clusters[i]<-list(clusterizacao[2])
    
    ctr<-data.frame(centroides[i]) #grava centroides dos clusters da hora h, mes m
    cl<-data.frame(clusters[i]) #grava clusterizacao da hora h, mes m
    for (j in 1:nrow(cl)) {
      dorig$cluster[which((dorig$Mes == m)&(dorig$Hora == h))][[j]]<-cl[[1]][[j]]
      dorig$cl_GE[which((dorig$Mes == m)&(dorig$Hora == h))][[j]]<-ctr[[1]][[cl[[1]][[j]]]]
      dorig$cl_GS[which((dorig$Mes == m)&(dorig$Hora == h))][[j]]<-ctr[[2]][[cl[[1]][[j]]]]
    }
    i = i + 1 
  }
}

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken

write.table(dorig, file='Clusterizacao.csv', sep=';', dec=',')

####Calsulo do vetor inicial de probabilidades incondicionais (mes 1, hora 0) e das matrizes de transicao####

#Vetor inicial de probabilidades incondicionais (mes 1, hora 0)

cont<-vector()
tot=0

for (c in 1:nrow(centroides[[1]][[1]])){
  cont[c]<-nrow(dorig[which((dorig$Mes == 1)&(dorig$Hora == 0)&(dorig$cluster == c)),]) #Mes e Hora sao o me e horario iniciais dos cenarios, editar se diferente de hora zero de janeiro
  tot=tot+cont[c]
}

cont=cont/tot
cont_acum<-cont

for(z in 2:length(cont)){
  cont_acum[z]<-cont_acum[z-1]+cont_acum[z]
}

#Matrizes de transicao entre os horarios do mesmo mes

i=1
aux=1
MT_ind_intra<-list()
MT_acum_intra<-list()

for (i in 1:288) { #24 matrizes de transicao para cada mes do ano
  if (aux!=24) {
    matr<-matriz_intra1(i) #funcao para MT intraday (entre horas do mesmo dia)
    MT_ind_intra[i]<-list(matr[1])
    MT_acum_intra[i]<-list(matr[2])
    aux = aux + 1
  } else {
    matr<-matriz_intra2(i) #funcao para interday (entre d e d+1, i.e., 23h (d) e 0h (d+1))
    MT_ind_intra[i]<-list(matr[1]) 
    MT_acum_intra[i]<-list(matr[2])
    aux = 1
  }
}

#Matrizes de transicao entre os meses

diasmes <- c(31,28,31,30,31,30,31,31,30,31,30,31)
num_anos <- max(dorig$Ano) - min(dorig$Ano) + 1
MT_ind_inter<-list()
MT_acum_inter<-list()

for (i in 1:11) { #11 matrizes de transicao entre os 12 meses do ano
    matr<-matriz_inter(i) #funcao para MT entre os meses 
    MT_ind_inter[i]<-list(matr[1])
    MT_acum_inter[i]<-list(matr[2])
}

###Salvando exemplos

#Obs.: na lista centroides, indice usado refere-se a hora anterior ao indice 
Hora<-12 #0 a 23

##Atencao! Se estiver simulando o estudo de caso de Caetite, que esta em pu, usar 21.6 (eolica) e 4.8 (solar) para as capacidades instaladas.
#Como os dados de Santa Clara a Assu V nao estao em p.u., usar 1 nos dois casos

cap_inst_eol<-1 #capacidade instalada da usina (eolica)
cap_inst_sol<-1 #capacidade instalada da usina (solar)

for (Mes in 1:12){
  clustersh<-data.frame(centroides[[((Hora+1)+24*(Mes-1))]])
  clustersh[,1]<-clustersh[,1]*cap_inst_eol
  clustersh[,2]<-clustersh[,2]*cap_inst_sol
  write.xlsx(clustersh, paste0("Centroide_",Mes,"_",Hora,".xlsx"))  
}

#Na lista MT_acum_intra, indice usado refere-se a transicao entre hora anterior e hora do indice
matriz_ex <- data.frame(MT_acum_intra[126]) #5 e 6
write.table(matriz_ex, file='Mt_acum_intra_horas_5e6_mes6.csv', sep=';', dec=',')

Mes<-6 

for (Hora in 5:6){
  clustersh<-data.frame(centroides[[((Hora+1)+24*(Mes-1))]])
  clustersh[,1]<-clustersh[,1]*cap_inst_eol
  clustersh[,2]<-clustersh[,2]*cap_inst_sol
  write.xlsx(clustersh, paste0("Centroide_",Mes,"_",Hora,".xlsx"))  
}

#Exemplo entre os meses 1 e 2
write.table(data.frame(MT_acum_inter[1]), file='Mt_acum_inter_mes_1e2.csv', sep=';', dec=',')

#Exemplo vetor de prob. incondicional

write.table(cont_acum, file='Prob_incond_m1h0.csv', sep=';', dec=',')

Mes<-1 
Hora<-0

clustersh<-data.frame(centroides[[((Hora+1)+24*(Mes-1))]])
clustersh[,1]<-clustersh[,1]*cap_inst_eol
clustersh[,2]<-clustersh[,2]*cap_inst_sol
write.xlsx(clustersh, paste0("Centroide_",Mes,"_",Hora,".xlsx"))  


###Salvando arquivos da modelagem

saveRDS(centroides, file="Centroides.RData")
saveRDS(clusters, file="Clusters_dados.RData")
saveRDS(dorig, file="Clusterizacao.RData")
saveRDS(cont, file="Prob_ind_incond.RData")
saveRDS(cont_acum, file="Prob_acum_incond.RData")
saveRDS(MT_acum_inter, file="MT_acum_inter.RData")
saveRDS(MT_acum_intra, file="MT_acum_intra.RData")
saveRDS(MT_ind_inter, file="MT_ind_inter.RData")
saveRDS(MT_ind_intra, file="MT_ind_intra.RData")

#### Simulacao de cenarios #########################################################

estados_sim <- data.frame(Sim=integer(),
                          Mes=integer(),
                          Dia=integer(),
                          Hora=integer(),
                          Estado=integer(),
                          GE=double(),
                          GS=double(),
                          Uniforme=double())

i=1 #Indice das linhas do df a serem preenchidas

start.time <- Sys.time()

for(cenario in 1:200) {
  for (mes in 1:12) {
    for (dia in 1:diasmes[mes]) {
      for (hora in 0:23) {
        
        estados_sim[i,1]=cenario
        estados_sim[i,2]=mes
        estados_sim[i,3]=dia
        estados_sim[i,4]=hora
        
        if(mes==1 && dia==1 && hora==0) { #primeiro valor da simulacao, i.e., 01/01, hora zero
          inicio<-estadoinicial(cont_acum) #funcao para sortear estado inicial, considerando vetor de probabilidades incondicionais
          estado2<-inicio[[1]]
          estados_sim[i,8]<-inicio[[2]] #para registrar valor da uniforme simulado
        }else if(dia==1 && hora==0) { #primeira hora do mes (exceto janeiro), MT deve ser intermensal
          MT <- data.frame(MT_acum_inter[mes-1])
          if(MT[estado1,1]=='NaN'){ 
            estado2<-novoestado_distmin(mes,estado1) #funcao para verificar distancia entre estado1 e cada estado candidato possivel e selecionar o de menor distancia
          }else{
            simulado<-novoestado(estado1,MT) #funcao para simular o proximo estado
            estado2<-simulado[[1]]
            estados_sim[i,8]<-simulado[[2]] #para registrar valor da uniforme simulado
          }
        }else {
          if(hora==0) { #nao eh primeiro dia, entao usa-se ultima MT do mes (23 para 0, interdiaria)
            MT <- data.frame(MT_acum_intra[24*mes]) 
          }else { #MT de indice igual estado a ser simulado
            MT <- data.frame(MT_acum_intra[24*(mes-1)+hora]) 
          }
          simulado<-novoestado(estado1,MT) #funcao para simular o proximo estado
          estado2<-simulado[[1]]
          estados_sim[i,8]<-simulado[[2]] #para registrar valor da uniforme simulado
        }
        
        estado1=estado2
        estados_sim[i,5]=estado2
        estados_sim[i,6]=data.frame(centroides[24*(mes-1)+(hora+1)])[estado2,1]
        estados_sim[i,7]=data.frame(centroides[24*(mes-1)+(hora+1)])[estado2,2]
        i=i+1
        
      }
    }
  }
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

write.csv(estados_sim, "Cenarios.csv")
