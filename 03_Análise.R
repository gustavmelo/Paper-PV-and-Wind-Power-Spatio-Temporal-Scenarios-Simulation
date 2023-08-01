library(dplyr)
library(ggplot2)
library(scales)

#### Pular comandos abaixo, se nao precisar ler arquivos ######

cen1 = readxl::read_xlsx(path = "C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Artigos/Resultados/Simulacoes/Case2/Cenarios.xlsx", sheet = "Planilha1")
cen2 = readxl::read_xlsx(path = "C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Artigos/Resultados/Simulacoes/Case2/Cenarios.xlsx", sheet = "Planilha2")
estados_sim <- rbind(cen1,cen2)

dorig<-readRDS("Clusterizacao.RData")
centroides<- readRDS("Centroides.RData")
clusters<-readRDS("Clusters_dados.RData")

#### Numero de clusters por agrupamento ########################################

numberclusters<-vector()

for (i in 1:288) {
  df<-as.data.frame(clusters[[i]])
  numberclusters[i]<-max(df$cluster)
}

write.csv(numberclusters, "NumeroClusters.csv")

#### Estatisticas ##############################################################

##Atencao! Se estiver simulando o estudo de caso de Caetite, que esta em pu, usar 21.6 (eolica) e 4.8 (solar) para as capacidades instaladas.
#Como os dados de Santa Clara a Assu V nao estao em p.u., estao em geracao, usar 1 nos dois casos
cap_inst_eol<-1 #capacidade instalada da usina (eolica)
cap_inst_sol<-1 #capacidade instalada da usina (solar)

##### Media e DP historicos ####

dorig$cl_GE<-as.double(dorig$cl_GE)
dorig$cl_GS<-as.double(dorig$cl_GS)

dadoshist<-dorig[,2:5]
dadoshist[,3]<-dadoshist[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eolica
dadoshist[,4]<-dadoshist[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

##Por hora x mes 
estatis_hist_mes_hora<-group_by(dadoshist, Mes, Hora)%>%summarise(MediaGE_H=mean(GE),MediaGS_H=mean(GS),DPGE_H=sd(GE),DPGS_H=sd(GS))
write.xlsx(estatis_hist_mes_hora, "Est_hist_mes_hora_eol_sol.xlsx")

##Por mes 
estatis_hist_mes<-group_by(dadoshist, Mes)%>%summarise(MediaGE_H=mean(GE),MediaGS_H=mean(GS),DPGE_H=sd(GE),DPGS_H=sd(GS)) #Sem desconsiderar per?odo noturno da solar
write.xlsx(estatis_hist_mes, "Est_hist_mes_eol_sol.xlsx")

solar_diurno_hist<-dadoshist[which(dadoshist$GS!=0),]
solar_diurno_hist<-group_by(solar_diurno_hist, Mes)%>%summarise(MediaGS_H=mean(GS),DPGS_H=sd(GS)) #Considerando apenas per?odo diurno da solar
write.xlsx(solar_diurno_hist, "Est_hist_mes_diurno_sol.xlsx")

#### Media e DP simulados ####

dadosim<-estados_sim[,c(2,4,6,7)]
dadosim[,3]<-dadosim[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eolica
dadosim[,4]<-dadosim[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

##Por hora x mes 
estatis_sim_mes_hora<-group_by(dadosim, Mes, Hora)%>%summarise(MediaGE_Sim=mean(GE),MediaGS_Sim=mean(GS),DPGE_Sim=sd(GE),DPGS_Sim=sd(GS))
write.xlsx(estatis_sim_mes_hora, "Est_sim_mes_hora_eol_sol.xlsx")

##Por mes 
estatis_sim_mes<-group_by(dadosim, Mes)%>%summarise(MediaGE_Sim=mean(GE),MediaGS_Sim=mean(GS),DPGE_Sim=sd(GE),DPGS_Sim=sd(GS)) #Sem desconsiderar per?odo noturno da solar
write.xlsx(estatis_sim_mes, "Est_sim_mes_eol_sol.xlsx")

solar_diurno_sim<-dadosim[which(dadosim$GS!=0),]
solar_diurno_sim<-group_by(solar_diurno_sim, Mes)%>%summarise(MediaGS_Sim=mean(GS),DPGS_Sim=sd(GS)) #Considerando apenas per?odo diurno da solar
write.xlsx(solar_diurno_sim, "Est_sim_mes_diurno_sol.xlsx")

##Diferencas %

desvio_mes_hora<-estatis_hist_mes_hora[,c(1,2)]
desvio_mes_hora$Dif_MediaGE<-''
desvio_mes_hora$Dif_DPGE<-''
desvio_mes_hora$Dif_MediaGS<-''
desvio_mes_hora$Dif_DPGS<-''

for (i in 1:288){
  desvio_mes_hora$Dif_MediaGE[i]=(abs(estatis_hist_mes_hora$MediaGE_H[i]-estatis_sim_mes_hora$MediaGE_Sim[i]))/estatis_hist_mes_hora$MediaGE_H[i]
  desvio_mes_hora$Dif_DPGE[i]=(abs(estatis_hist_mes_hora$DPGE_H[i]-estatis_sim_mes_hora$DPGE_Sim[i]))/estatis_hist_mes_hora$DPGE_H[i]
  desvio_mes_hora$Dif_MediaGS[i]=(abs(estatis_hist_mes_hora$MediaGS_H[i]-estatis_sim_mes_hora$MediaGS_Sim[i]))/estatis_hist_mes_hora$MediaGS_H[i]
  desvio_mes_hora$Dif_DPGS[i]=(abs(estatis_hist_mes_hora$DPGS_H[i]-estatis_sim_mes_hora$DPGS_Sim[i]))/estatis_hist_mes_hora$DPGS_H[i]
}

write.table(desvio_mes_hora, file='Dif%_mes_hora.csv', sep=';', dec=',')

desvio_mes_eol<-estatis_hist_mes[,1]
desvio_mes_eol$Dif_MediaGE<-''
desvio_mes_eol$Dif_DPGE<-''

for (i in 1:12){
  desvio_mes_eol$Dif_MediaGE[i]=(abs(estatis_hist_mes$MediaGE_H[i]-estatis_sim_mes$MediaGE_Sim[i]))/estatis_hist_mes$MediaGE_H[i]
  desvio_mes_eol$Dif_DPGE[i]=(abs(estatis_hist_mes$DPGE_H[i]-estatis_sim_mes$DPGE_Sim[i]))/estatis_hist_mes$DPGE_H[i]
}

write.table(desvio_mes_eol, file='Dif%_mes_eol.csv', sep=';', dec=',')

desvio_mes_sol<-estatis_hist_mes[,1]
desvio_mes_sol$Dif_MediaGS<-''
desvio_mes_sol$Dif_DPGS<-''

for (i in 1:12){
  desvio_mes_sol$Dif_MediaGS[i]=(abs(solar_diurno_hist$MediaGS_H[i]-solar_diurno_sim$MediaGS_Sim[i]))/solar_diurno_hist$MediaGS_H[i]
  desvio_mes_sol$Dif_DPGS[i]=(abs(solar_diurno_hist$DPGS_H[i]-solar_diurno_sim$DPGS_Sim[i]))/solar_diurno_hist$DPGS_H[i]
}

write.table(desvio_mes_sol, file='Dif%_mes_sol.csv', sep=';', dec=',')

##### KS.Test #################################################################

eolkshist<-as.vector((dorig$cl_GE)*cap_inst_eol)
eolkssim<-dadosim$GE

ks.test(eolkssim,eolkshist)

solkshist<-as.vector((dorig$cl_GS)*cap_inst_sol)
solkssim<-dadosim$GS

ks.test(solkssim,solkshist)

#### Histogramas ##############################################################

NomeMes<-c("January","February","March","April","May","June","July","August","September","October","November","December")

# Mudar indice da variavel "Hora" para o horario desejado:

Hora<-12 #0 a 23

for (Mes in 1:1){
  clustersh<-clusters[[((Hora+1)+24*(Mes-1))]] #pegando o vetor da clusterizacao do historico para o horario desejado
  
  numclustersh<-dorig[which(dorig$Mes==Mes & 
                              dorig$Hora==Hora),]
  numclustersh<-numclustersh$cluster
  numclustersh<-max(as.integer(numclustersh))
  
  
  freqhist<-hist(clustersh$cluster,probability = T, 
                 c(0:numclustersh))
  
  filtrohorasim<-estados_sim[which(estados_sim$Mes==Mes & 
                                     estados_sim$Hora==Hora),]
  freqsim<-hist(filtrohorasim$Estado,probability = T, 
                c(0:numclustersh))
  
  Hist_tab<-data.frame(c(rep("Histórico",numclustersh),
                         rep("Simulação",numclustersh)),
                       c(freqhist$density,
                         freqsim$density),
                       rep(c(1:numclustersh),2))
  colnames(Hist_tab)<-c("Origem", "Densidade", "Estados")
  
  ggplot(Hist_tab, aes(x= as.factor(Estados), y=Densidade, fill=Origem)) +
    geom_bar(stat='identity', position='dodge') + 
    ylab("Density") +
    xlab("States") +
    ggtitle(paste0(NomeMes[Mes]," ",Hora, " pm - Santa Clara and Assú V Plants"))+ #Modificar se for Caetite
    scale_fill_manual(values = c("#2e4057","#4CA268"))+  
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(colour="black"),
          axis.text.y = element_text(colour="black"),
          title =element_text(size=12),
          plot.title = element_text(hjust = 0.5))+
    theme(legend.position="none") #Removendo a legenda inteira
  #theme(legend.title=element_blank(),
  #     legend.position="bottom", #Legenda sem titulo e embaixo
  #    legend.text = element_text(size = 11),
  #   legend.key.size = unit(0.5,'cm'))
  
  ggsave(paste0("Hist_",Mes,"_",Hora,".png"), width = 15, height = 8, units = "cm")  
}

#### FACs #####################################################################

NomeFonte=c("Wind","PV")
CorSim=c("#097F05","#DC9404")

for(Fonte in 4:5) { #obs: indice 4 para eolica, 5 para solar
    
    dorig2<-dorig
    estadosim2<-estados_sim
    
    conf.level <- 0.95
    ciline <- qnorm((1 + conf.level)/2)/sqrt(nrow(dorig2))
    bacf <- acf(dorig2[,Fonte], lag=52, plot = FALSE) 
    bacfdf <- with(bacf, data.frame(lag, acf))
    
    bacf2 <- acf(estadosim2[,Fonte+2], lag=52, plot = FALSE)
    bacfdf2 <- with(bacf2, data.frame(lag, acf))
    
    bacfdf$teste<-"Histórico"
    bacfdf2$teste<-"Simulação"
    bacfdf<- rbind(bacfdf,bacfdf2)
    
    ggplot(data=bacfdf,aes(x=lag, y=acf, colour=teste, linetype=teste, size=teste))+
      geom_line()+
      scale_linetype_manual(values = c("solid", "dashed"))+
      scale_size_manual(values=c(1.2,1.2))+
      scale_color_manual(values = c("#645858", CorSim[Fonte-3]))+  
      ggtitle(paste("Santa Clara Plant - ", NomeFonte[Fonte-3], " Source"))+  #Modificar se for Assu V ou Caetite
      scale_y_continuous(name = "ACF",
                         labels = label_number(accuracy = 0.01))+
      scale_x_continuous(name = "Lag (hours)")+
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(colour="black"),
            axis.text.y = element_text(colour="black"),
            title =element_text(size=12),
            plot.title = element_text(hjust = 0.5))+
      geom_hline(yintercept=c(ciline, -ciline), linetype="dashed")+
      theme(legend.position="none") #Removendo a legenda inteira
    #theme(legend.title = element_blank(),  #Para salvar com legenda, opcao 1
    #      legend.position = c(0.89, 0.15), #0.89,0.85 para eolica e 0.89,0.15 para solar
    #      legend.key.size = unit(0.72,'cm'))
    #theme(legend.title = element_blank(),  #Para salvar com legenda, opcao 2
    #      legend.position = "bottom",
    #      legend.text = element_text(size = 18),
    #      legend.key.size = unit(2,'cm'))+
    #guides(linetype = guide_legend(override.aes = list(size = 1.1))) 
    
    ggsave(paste0("FAC_","_",NomeFonte[Fonte-3],".png"), width = 15, height = 8, units = "cm")
    
}

#### Grafico nuvem medias ########################################


#Medias mensais simuladas
media_sim_eol<-estatis_sim_mes[,c(1,2)]
names(media_sim_eol)[names(media_sim_eol) == "MediaGE_Sim"] <- "MediaGE_S"
media_sim_sol<-solar_diurno_sim[,c(1,2)]
names(media_sim_sol)[names(media_sim_sol) == "MediaGS_Sim"] <- "MediaGS_S"

##Limites simulados
dadosim<-estados_sim[,c(1,2,6,7)]
dadosim[,3]<-dadosim[,3]*cap_inst_eol #Multiplicando pela capacidade instalada eolica
dadosim[,4]<-dadosim[,4]*cap_inst_sol #Multiplicando pela capacidade instalada solar

media_cen_sim_eol<-group_by(dadosim, Sim, Mes)%>%summarise(MediaGE_C=mean(GE))
media_cen_sim_sol<-dadosim[which(dadosim$GS!=0),]
media_cen_sim_sol<-group_by(media_cen_sim_sol, Sim, Mes)%>%summarise(MediaGS_C=mean(GS))

limites_eol<-data.frame(Mes=c(1:12))
limites_eol$Inferior<-''
limites_eol$Superior<-''
limites_sol<-data.frame(Mes=c(1:12))
limites_sol$Inferior<-''
limites_sol$Superior<-''

for (mes in 1:12){
  
  auxiliar_eol<-filter(media_cen_sim_eol[,c(2,3)],Mes==mes)
  limites_eol$Inferior[mes]<-quantile(auxiliar_eol$MediaGE_C,prob=0.05)
  limites_eol$Superior[mes]<-quantile(auxiliar_eol$MediaGE_C,prob=0.95)
  
  auxiliar_sol<-filter(media_cen_sim_sol[,c(2,3)],Mes==mes)
  limites_sol$Inferior[mes]<-quantile(auxiliar_sol$MediaGS_C,prob=0.05)
  limites_sol$Superior[mes]<-quantile(auxiliar_sol$MediaGS_C,prob=0.95)
  
}


timeline_table_eol = data.frame(Month=rep(1:12,4),
                            Series=c(rep("Média Simulada",12),
                                     rep("Média Histórica",12),
                                     rep("Limite Inferior",12),  
                                     rep("Limite Superior",12)), 
                            Value = as.numeric(c(media_sim_eol$MediaGE_S,
                                                 estatis_hist_mes$MediaGE_H,
                                                 limites_eol$Inferior, 
                                                 limites_eol$Superior)))

timeline_table_eol$Series<-factor(timeline_table_eol$Series,levels = c("Limite Superior","Média Simulada","Média Histórica","Limite Inferior"))


#Grafico solar
timeline_table_sol = data.frame(Month=rep(1:12,4),
                                Series=c(rep("Média Simulada",12),
                                         rep("Média Histórica",12),
                                         rep("Limite Inferior",12),
                                         rep("Limite Superior",12)),
                                Value = as.numeric(c(media_sim_sol$MediaGS_S,
                                                     solar_diurno_hist$MediaGS_H,
                                                     limites_sol$Inferior, 
                                                     limites_sol$Superior)))

timeline_table_sol$Series<-factor(timeline_table_sol$Series,levels = c("Limite Superior","Média Simulada","Média Histórica","Limite Inferior"))


#### Nuvens de cenarios das duas fontes

medias_eol<-media_cen_sim_eol
as.numeric(medias_eol$MediaGE_C)

medias_sol<-media_cen_sim_sol
as.numeric(medias_sol$MediaGS_C)


###### Grafico final - sem legenda

## Eolica
names(medias_eol)[names(medias_eol) == "MediaGE_C"] <- "Value"
names(medias_eol)[names(medias_eol) == "Mes"] <- "Month"
names(medias_eol)[names(medias_eol) == "Sim"] <- "Series"
medias_eol<-medias_eol[,c(2,1,3)]
medias_eol$Series<-factor(medias_eol$Series)
timeline_table_eol_2 <- rbind(medias_eol,timeline_table_eol)

ggplot(data=timeline_table_eol_2, aes(x=Month ,y=Value, group=Series, color=Series, linetype=Series, size=Series))+
  geom_line()+
  scale_linetype_manual(values = c(rep("solid",200),"dashed", "solid", "solid", "dashed"))+
  scale_colour_manual(values = c(rep("grey",200), "#4E4E4E", "#2AD13DC7","#127A1DBA", "#4E4E4E"))+
  scale_size_manual(values=c(rep(0.5,200),1.1,1.1,1.1,1.1),guide="none")+
  theme_classic()+
  scale_x_continuous(name = "Month",
                     breaks=seq(1,12))+
  scale_y_continuous(name = "Generation (MWh)",
                     breaks = seq(2,18,2),
                     labels = label_number(accuracy = 0.01))+
  ggtitle("Santa Clara Plant - Wind Source")+ #Modificar se for Caetite
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

ggsave("Eólica_Nuvem_Histverdeescuro.png", width = 20, height = 11, units = "cm")

## Solar
names(medias_sol)[names(medias_sol) == "MediaGS_C"] <- "Value"
names(medias_sol)[names(medias_sol) == "Mes"] <- "Month"
names(medias_sol)[names(medias_sol) == "Sim"] <- "Series"
medias_sol<-medias_sol[,c(2,1,3)]
medias_sol$Series<-factor(medias_sol$Series)
timeline_table_sol_2 <- rbind(medias_sol,timeline_table_sol)

ggplot(data=timeline_table_sol_2, aes(x=Month ,y=Value, group=Series, color=Series, linetype=Series, size=Series))+
  geom_line()+
  scale_linetype_manual(values = c(rep("solid",200),"dashed", "solid", "solid", "dashed"))+
  scale_colour_manual(values = c(rep("grey",200), "#4E4E4E", "#FFFF3B", "#DC9404", "#4E4E4E"))+ #FFFFA1 FFFF00
  scale_size_manual(values=c(rep(0.5,200),1.1,1.1,1.1,1.1),guide="none")+
  theme_classic()+
  scale_x_continuous(name = "Month",
                     breaks=seq(1,12))+
  scale_y_continuous(name = "Generation (MWh)",
                     breaks = seq(0,3,0.2),
                     labels = label_number(accuracy = 0.01))+
  ggtitle("Assú V Plant - PV Source")+ #Modificar se for Caetite
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        title = element_text(size=14),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

ggsave("Solar_Nuvem.png", width = 20, height = 11, units = "cm")
