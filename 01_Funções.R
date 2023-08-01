
#Funcao 1

clusterizar <- function(dorig, m, h){
  set.seed(100)
  
  dadosm <- dorig[dorig$Mes == m, ]
  dadosmh <- dadosm[dadosm$Hora == h, ]
  
  mydata = dadosmh[c(4,5)]
  max_aux1 = max(dadosmh[4])
  max_aux2 = max(dadosmh[5])
  
  if (max_aux1>0) {mydata[,1] = dadosmh[,4]/max_aux1}  
  if (max_aux2>0) {mydata[,2] = dadosmh[,5]/max_aux2}
  
  wss = wss_acum = NULL
  
  for(i in 1:25){ #Caetite limite de 25, Santa Clara e Assu V limite de 20 para testar se metodologia gera bons resultados tambem
    wss[i] = sum(kmeans(mydata,centers=i,iter.max = 15)$withinss)
    if(i == 1){wss_acum = wss[i]}else{wss_acum[i] = wss_acum[i-1]+wss[i]}
  }
  
  wss_aux = wss_acum/sum(wss)
  aux_num_cluster = which(abs(wss_aux) >= 0.98)[1]
  num_cluster = aux_num_cluster
  
  set.seed(100)
  clusterizacao = kmeans(mydata,centers=aux_num_cluster,iter.max = 15)
  
  agrupamento = clusterizacao$cluster
  centroides = clusterizacao$centers
  centroides[,1] = centroides[,1]*max_aux1
  centroides[,2] = centroides[,2]*max_aux2 
  freq = clusterizacao$size
  
  return(list(centroides, cluster = agrupamento))
}

#Funcao 2

matriz_intra1 <-function(i){
  ##Individual
  Pind_matriz = matrix(nrow = nrow(centroides[[i]][[1]]), ncol = nrow(centroides[[i+1]][[1]]), data = 0)
  cl1 <- data.frame(clusters[i])
  cl2 <- data.frame(clusters[i+1])
  
  for (j in 1:nrow(cl1)) {
    estadolinha <- cl1[[1]][[j]]
    estadocoluna <- cl2[[1]][[j]]
    Pind_matriz[estadolinha,estadocoluna] = Pind_matriz[estadolinha,estadocoluna] + 1
  }
  for(z in 1:nrow(Pind_matriz)){
    soma = sum(Pind_matriz[z,])
    for(w in 1:ncol(Pind_matriz)){
      Pind_matriz[z,w] = Pind_matriz[z,w]/soma
    }
  }
  ##Acumulada
  Pacum_matriz <- Pind_matriz
  for(z in 2:ncol(Pind_matriz)){
    Pacum_matriz[,z]<-Pacum_matriz[,z-1]+Pacum_matriz[,z]
  }
  return(list(Pind_matriz, Pacum_matriz))
}

#Funcao 3

matriz_intra2 <-function(i){
  ##Individual
  Pind_matriz = matrix(nrow = nrow(centroides[[i]][[1]]), ncol = nrow(centroides[[i-23]][[1]]), data = 0)
  cl1 <- data.frame(clusters[i])
  cl2 <- data.frame(clusters[i-23])
  
  for (j in 1:(nrow(cl1)-1)) {
    estadolinha <- cl1[[1]][[j]]
    estadocoluna <- cl2[[1]][[j+1]]
    Pind_matriz[estadolinha,estadocoluna] = Pind_matriz[estadolinha,estadocoluna] + 1
  }
  for(z in 1:nrow(Pind_matriz)){
    soma = sum(Pind_matriz[z,])
    for(w in 1:ncol(Pind_matriz)){
      Pind_matriz[z,w] = Pind_matriz[z,w]/soma
    }
  }
  ##Acumulada
  Pacum_matriz <- Pind_matriz
  for(z in 2:ncol(Pind_matriz)){
    Pacum_matriz[,z]<-Pacum_matriz[,z-1]+Pacum_matriz[,z]
  }
  return(list(Pind_matriz, Pacum_matriz))
}

#Funcao 4

matriz_inter<-function(i){
  ##Individual
  Pind_matriz = matrix(nrow = nrow(centroides[[24*i]][[1]]), ncol = nrow(centroides[[(24*i)+1]][[1]]), data = 0)
  cl1 <- data.frame(clusters[24*i])
  cl2 <- data.frame(clusters[(24*i)+1])
  
  for (j in 1:num_anos) {
    estadolinha <- cl1[[1]][[diasmes[i]*j]] #ultimo horario do mes i
    estadocoluna <- cl2[[1]][[(j-1)*diasmes[i+1]+1]] #primeiro horario do mes i+1
    Pind_matriz[estadolinha,estadocoluna] = Pind_matriz[estadolinha,estadocoluna] + 1
  }
  for(z in 1:nrow(Pind_matriz)){
    soma = sum(Pind_matriz[z,])
    for(w in 1:ncol(Pind_matriz)){
      Pind_matriz[z,w] = Pind_matriz[z,w]/soma
    }
  }
  ##Acumulada
  Pacum_matriz <- Pind_matriz
  for(z in 2:ncol(Pind_matriz)){
    Pacum_matriz[,z]<-Pacum_matriz[,z-1]+Pacum_matriz[,z]
  }
  return(list(Pind_matriz, Pacum_matriz))
}

#Funcao 5 (caso inicial)

estadoinicial <- function(cont_acum){
  unif <- runif(1,0,1)
  candidato=1
  while (candidato <= length(cont_acum)) {
    if(candidato==1 && unif<=cont_acum[candidato]) {
      e2=candidato
      candidato=length(cont_acum)+1
    }else if(cont_acum[candidato]<unif && unif<=cont_acum[candidato+1]) {
      e2=candidato+1
      candidato=length(cont_acum)+1
    }else {
      candidato=candidato+1 
    }
  }
  return(list(e2,unif))
}

#Funcao 6

novoestado_distmin <- function(mes,e1){
  cent1=data.frame((centroides[24*mes-24]))[e1,1]
  cent2=data.frame((centroides[24*mes-23]))[,1]
  dist<-vector()
  for (i in 1:length(cent2)) {
    dist[i]=abs(cent1-cent2[i])
  }
  e2<-which(dist %in% min(dist))
  return(e2)
}

#Funcao 7

novoestado <- function(e1, matriz_tr){
  linha=e1
  unif <- runif(1,0,1)
  coluna=1
  while (coluna <= ncol(matriz_tr)) {
    if(coluna==1 && unif<=matriz_tr[linha,coluna]) {
      e2=coluna
      coluna=ncol(matriz_tr)+1
    }else if(matriz_tr[linha,coluna]<unif && unif<=matriz_tr[linha,coluna+1]) {
      e2=coluna+1
      coluna=ncol(matriz_tr)+1
    }else {
      coluna=coluna+1 
    }
  }
  return(list(e2,unif))
}

