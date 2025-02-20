
library(ggnewscale)
library(ggpubr)
library(gridExtra)
library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(openxlsx)
library(dplyr)
library(gmapsdistance)  # for getting info from the API
library(cowplot)
library(RColorBrewer)

rm(list=ls())
setwd("F:\\05. Inestigaciones RON\\02. Tratamiento de Radioterapia")
source("03. Codigo R\\02. Funciones y Get Data\\00. Funciones.r")

df = data.table(values=1:999)
df$Sum_Dig = apply(df, 1, Sum_Dig)

df_num = data.table(values=df[Sum_Dig==7, values])
df_num$Agre.Ceros = apply(df_num, 1, agregarCeros, cant=3)

df_Comb_Norte = data.table(df_num, t(apply(df_num, 1, Separar)))
colnames(df_Comb_Norte) = c("values","Agre.Ceros", "IREN.NORTE", "HBT", "HRL")

df_Comb_Norte = df_Comb_Norte[IREN.NORTE<5 & HBT<4 & HRL<4, ]



df = data.table(values=1:99999)
df$Sum_Dig = apply(df, 1, Sum_Dig)

df_num = data.table(values=df[Sum_Dig==3, values])
df_num$Agre.Ceros = apply(df_num, 1, agregarCeros, cant=5)

df_Comb_Sur = data.table(df_num, t(apply(df_num, 1, Separar)))
colnames(df_Comb_Sur) = c("values","Agre.Ceros_Sur", "IREN.Sur", "HRHDE", "HG", "HALC", "HADC")
df_Comb_Sur$T_Promedio = as.numeric()
df_Comb_Sur$P_NO_Cubierta = as.numeric()
df_Comb_Sur = df_Comb_Sur[IREN.Sur>0 & IREN.Sur<3 & HRHDE<3 & HG<3 & HALC<3 & HADC<3, ]

df_Comb = data.table()
for (ag in df_Comb_Norte$Agre.Ceros) {
  df_Comb = rbind(df_Comb, cbind(select(df_Comb_Norte[Agre.Ceros==ag, ], -"values"), select(df_Comb_Sur, -"values")))
}

df_Comb[, Agre.Ceros:=paste0(Agre.Ceros,Agre.Ceros_Sur)]
df_Comb$Agre.Ceros_Sur = NULL


k = 0
for (val in df_Comb$Agre.Ceros) {
  k = k + 1
  print(k)
  Carp = "02. Apl SIS no cubiertas por PV\\" # BD mapa: % pbl SIS cambiada
  Anho = 2023
  source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
  
  IPRESS_ONC = IPRESS_ONC[Anho_==Anho,]
  IPRESS_ONC[IPRESS_ABREV%in%c("HNCH", "HNHU", "HMA"), Equipos_RE:=1] # Fase 01
  IPRESS_ONC[IPRESS_ABREV%in%c("INEN"), Equipos_RE:=Equipos_RE+1] # Fase 01
  IPRESS_ONC[IPRESS_ABREV%in%c("HRL.FSAI"), Equipos_RE:=1] # Fase 02
  IPRESS_ONC[IPRESS_ABREV%in%c("HT"), Equipos_RE:=1] # Fase 02
  IPRESS_ONC[IPRESS_ABREV%in%c("IREN.CENTRO"), Equipos_RE:=Equipos_RE+2] # Fase 03
  
  iren.N = df_Comb[Agre.Ceros==val, IREN.NORTE]
  hbt = df_Comb[Agre.Ceros==val, HBT]
  hrl = df_Comb[Agre.Ceros==val, HRL]
  
  iren.S = df_Comb[Agre.Ceros==val, IREN.Sur]
  hrhde = df_Comb[Agre.Ceros==val, HRHDE]
  hg = df_Comb[Agre.Ceros==val, HG]
  halc = df_Comb[Agre.Ceros==val, HALC]
  hadc = df_Comb[Agre.Ceros==val, HADC]
  
  IPRESS_ONC[IPRESS_ABREV=="IREN.NORTE", Equipos_RE:=Equipos_RE+iren.N]
  IPRESS_ONC[IPRESS_ABREV=="HBT", Equipos_RE:=Equipos_RE+hbt]
  IPRESS_ONC[IPRESS_ABREV=="HRL", Equipos_RE:=Equipos_RE+hrl]
  
  IPRESS_ONC[IPRESS_ABREV=="IREN.SUR", Equipos_RE:=Equipos_RE+iren.S]
  IPRESS_ONC[IPRESS_ABREV=="HRHDE", Equipos_RE:=Equipos_RE+hrhde]
  IPRESS_ONC[IPRESS_ABREV=="HG", Equipos_RE:=Equipos_RE+hg]
  IPRESS_ONC[IPRESS_ABREV=="HALC", Equipos_RE:=Equipos_RE+halc]
  IPRESS_ONC[IPRESS_ABREV=="HADC", Equipos_RE:=Equipos_RE+hadc]
  
  IPRESS_ONC = IPRESS_ONC[Equipos_RE>0, ]
  
  Pob_Map_Ubi = Pob_Map_Ubi %>% filter(Anho==2023)
  
  IPRESS_ONC[, Pct_Limite:=Equipos_RE*240*60]
  IPRESS_ONC$Pob_IPRESS_RT = 0
  
  
  ################## MINSA
  IPRESS_ONC = IPRESS_ONC[INSTITUCION%in%c("GOBIERNO REGIONAL", "MINSA"),]
  ##################
  Time$IPRESS_AUX = NA
  Medida_Usar = Time[, c(IPRESS_ONC[, IPRESS_ABREV], "IPRESS_AUX")]
  Aux_Cal = data.frame(Medida_Usar)
  Aux_Cal$Aux_IPRESS = NA
  
  ## AGREGAMOS LOS DISTRITOS AL HNHU
  L.identf_HNHU = c("LIMA_LIMA_SAN JUAN DE LURIGANCHO", "LIMA_LIMA_EL AGUSTINO")
  Relacion = data.table(Identf=L.identf_HNHU, Prox="HNHU", Medida_API=c(1703, 206))
  Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), L.identf_HNHU), 
                    setdiff(colnames(Aux_Cal), "HNHU")]
  
  
  for (i in 1:nrow(Aux_Cal)) {
    min_AC = min(Aux_Cal, na.rm=T)
    if(min_AC==Inf){
      break
    }else{
      Aux_row = rowSums(Aux_Cal==min_AC, na.rm = TRUE)
      Aux_row = names(Aux_row[Aux_row!=0])[1] # Identificamos la fila con el menor valor
      Aux_col = colSums(Aux_Cal[Aux_row,]==min(Aux_Cal[Aux_row,], na.rm=T), na.rm = TRUE)
      Aux_col = names(Aux_col[Aux_col!=0])[1] # Identificamos la columna con el menor valor
      
      Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC))
      
      Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Aux_row), ] # Retiramos la fila
      
      Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Apl_P.SIS_RE
      IPRESS_ONC[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
      
      Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
      
      # if(Aux_col=="IREN.SUR"){
      #   print(min_AC)
      #   print(Poblc)
      #   print(Aux_row)
      #   print(IPRESS_ONC[, c("IPRESS_ABREV", "Pct_Limite", "Pob_IPRESS_RT")])
      # }
    }
  }
  
  RT_Ext_MINSA = merge(Pob_Map_Ubi, Relacion, by="Identf", all.x=TRUE)
  RT_Ext_MINSA = merge(RT_Ext_MINSA, 
                       rename(select(IPRESS_ONC, -c("UBIGEO", "Pob_IPRESS_RT", "Pct_Limite", "COD_UNICO")), 
                              Prox=IPRESS_ABREV), by="Prox", all.x=TRUE)
  
  Aux_TB_RE = data.table(RT_Ext_MINSA)[, .(Distritos=.N,
                                           Apl_Sis_Onc_RE=sum(Apl_P.SIS_RE),
                                           Equip_RE=unique(Equipos_RE)), keyby=.(Prox, Categoria)]
  Aux_TB_RE[, Apl_Posibles_RE:=Equip_RE*240*60]
  Aux_TB_RE = Aux_TB_RE[, c("Prox", "Categoria", "Equip_RE", "Apl_Posibles_RE", "Distritos", "Apl_Sis_Onc_RE")]
  
  Aux = data.table(RT_Ext_MINSA)[, .(Apl_RE=sum(Apl_P.SIS_RE),
                               S_tp=sum(Apl_P.SIS_RE*Medida_API)), keyby=Prox]
  
  df_Comb[Agre.Ceros==val, T_Promedio:=sum(Aux[is.na(Prox)==F, S_tp])/sum(Aux[is.na(Prox)==F, Apl_RE])]
  df_Comb[Agre.Ceros==val, P_NO_Cubierta:=Aux[is.na(Prox), Apl_RE]]
}

df_Comb[, T_Promedio:=T_Promedio/3600]
df_Comb = df_Comb[order(T_Promedio)]
# write.xlsx(df_Comb, "09. Modelo RT\\Implementacion por fases 2023\\df_Comb.xlsx")



######################## ANALIZAMOS 
df_Comb = read.xlsx("09. Modelo RT\\Implementacion por fases 2023\\df_Comb.xlsx")
df_Comb = data.table(df_Comb)

ggplot(df_Comb, aes(x=T_Promedio, y=P_NO_Cubierta, label=Agre.Ceros))+
  geom_point(size=1.3)+geom_text_repel(size=3, min.segment.length=0, arrow = arrow(length = unit(0.001, "npc")),
                                     segment.inflect=T, segment.square=T, segment.size=0.5, max.overlaps=Inf) +
  labs(title="Dispersión del Tiempo promedio y población no cubierta",
                                            x="Tiempo promedio", y="Población no cubierta") 



ggplot(df_Comb[T_Promedio<3.3,], aes(x=T_Promedio, y=P_NO_Cubierta, label=Agre.Ceros))+
  geom_point(size=1.3)+geom_text_repel(size=3, min.segment.length=0, arrow = arrow(length = unit(0.001, "npc")),
                                       segment.inflect=T, segment.square=T, segment.size=0.5, max.overlaps=Inf) +
  labs(title="Dispersión del Tiempo promedio y población no cubierta",
       x="Tiempo promedio", y="Población no cubierta") 


