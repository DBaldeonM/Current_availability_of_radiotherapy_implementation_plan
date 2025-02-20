
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
library(robustbase)

rm(list=ls())
setwd("C:\\Users\\Asus\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")

################################################################################
#################### -------------  Funciones ------------- #################### 
leng_f = function(vec){
  return(sum(vec>0))
}

convertir_a_hms <- function(segundos) {
  segundos = round(segundos,0)
  horas <- floor(segundos / 3600)
  minutos <- floor((segundos %% 3600) / 60)
  segundos <- segundos %% 60
  return(sprintf("%02d:%02d:%02d", horas, minutos, segundos))
}


Grafico_Menor.Distancia = function(df_Min_Model){
  IPRESS_ONC_Sim = merge(IPRESS_ONC, df_Min_Model, all.y=T)
  IPRESS_ONC_Sim = IPRESS_ONC_Sim %>% 
    filter(Equipos_RE>0) %>% 
    mutate(Pct_Limite=Equipos_RE*240*60,
           Pob_IPRESS_RT = 0)
  
  
  Aux_Cal = data.frame(Medida_Usar)
  Aux_Cal$IPRESS_AUX = NA
  
  Aux_Cal = Aux_Cal[, c(IPRESS_ONC_Sim[, IPRESS_ABREV], "IPRESS_AUX")]
  Relacion = data.table()
  
  Dist_DPD = rownames(Aux_Cal)
  
  
  #---- PARA CALLAO - HRL.SAI
  Dist_HNDAC = Dist_DPD[grep("^CALLAO_", Dist_DPD)]
  Aux_col = "HNDAC"
  for (Aux_row in Dist_HNDAC) {
    min_AC = Aux_Cal[Aux_row, Aux_col]
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Aj.Ap.SIS_R
    IPRESS_ONC_Sim[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC, Aj.Ap.SIS_R=Poblc))
  }
  Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Dist_HNDAC), ]
  Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC_Sim[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
  
  
  
  #---- PARA LORETO - HRL.SAI
  Dist_HRL.FSAI = setdiff(Dist_DPD[grep("^LORETO_", Dist_DPD)], 
                          c("LORETO_ALTO AMAZONAS_YURIMAGUAS", "LORETO_DATEM DEL MARAÑON_MANSERICHE"))
  Aux_col = "HRL.FSAI"
  for (Aux_row in Dist_HRL.FSAI) {
    min_AC = Aux_Cal[Aux_row, Aux_col]
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Aj.Ap.SIS_R
    IPRESS_ONC_Sim[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC, Aj.Ap.SIS_R=Poblc))
  }
  Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Dist_HRL.FSAI), ]
  Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC_Sim[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
  
  
  #---- PARA LORETO - HRP
  Dist_HRP = c("UCAYALI_PURUS_PURUS", "UCAYALI_CORONEL PORTILLO_MASISEA", "UCAYALI_CORONEL PORTILLO_IPARIA",
               "UCAYALI_ATALAYA_YURUA", "UCAYALI_ATALAYA_TAHUANIA", "UCAYALI_ATALAYA_SEPAHUA")
  Aux_col = "HRP"
  
  for (Aux_row in Dist_HRP) {
    min_AC = Aux_Cal[Aux_row, Aux_col]
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Aj.Ap.SIS_R
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC, Aj.Ap.SIS_R=Poblc))
    IPRESS_ONC_Sim[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
  }
  Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Dist_HRP), ]
  Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC_Sim[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
  
  
  #---- PARA LORETO - HT
  Dist_HT = c("SAN MARTIN_SAN MARTIN_CHIPURANA", "SAN MARTIN_SAN MARTIN_HUIMBAYOC", "SAN MARTIN_SAN MARTIN_PAPAPLAYA")
  Aux_col = "HT"
  
  for (Aux_row in Dist_HT) {
    min_AC = Aux_Cal[Aux_row, Aux_col]
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Aj.Ap.SIS_R
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC, Aj.Ap.SIS_R=Poblc))
    IPRESS_ONC_Sim[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
  }
  Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Dist_HT), ]
  Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC_Sim[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
  
  
  for (i in 1:nrow(Aux_Cal)) {
    min_AC = min(Aux_Cal, na.rm=T)
    Aux_row = rowSums(Aux_Cal==min_AC, na.rm = TRUE)
    Aux_row = names(Aux_row[Aux_row!=0])[1] # Identificamos la fila con el menor valor
    Aux_col = colSums(Aux_Cal[Aux_row,]==min(Aux_Cal[Aux_row,], na.rm=T), na.rm = TRUE)
    Aux_col = names(Aux_col[Aux_col!=0])[1] # Identificamos la columna con el menor valor
    Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Aux_row), ] # Retiramos la fila
    
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Aj.Ap.SIS_R
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC, Aj.Ap.SIS_R=Poblc))
    IPRESS_ONC_Sim[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
    Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC_Sim[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])] # Retiramos las IPRESS que hayan excedido su capacidad
    
  }
  
  RT_Ext_MINSA = merge(Pob_Map_Ubi %>% select(-Aj.Ap.SIS_R), Relacion, by="Identf", all.x=TRUE)
  RT_Ext_MINSA = merge(RT_Ext_MINSA, 
                       rename(IPRESS_ONC, Prox=IPRESS_ABREV), by="Prox", all.x=TRUE)
  
  
  
  #####################################
  #####################################
  Col_R.ES = c("HAP.CSR"="#ff6ae3", "HAS"="#EA8331", "HRP"="#BE7E38", "IREN.NORTE"="#35A2FF", "IREN.SUR"="#be9b7b", 
               "HRDC"="#FF6A98", "HADC"="#39B600", "HALC"="#F8766D", "IREN.CENTRO"="#9590FF", "HRL"="#ffcc00", 
               "HRMNB"="#00C3C8", "HEGB"="#00BB4E", "HRI"="#00B496", "HT"="#C09B00", "HRHV"="#00a2bf", 
               "HRA.MAMLL"="#A3A500", "HMA"="#c47dff", "HNHU"="#DDB509", "HNCH"="#4d8e6f", "INEN"="#7CAE00", 
               "HG"="#e55f88", "HNDAC"="#3A60CA", "HRL.FSAI"="#339900")   
  
  
  p_RE.Ext_MINSA = ggplot(RT_Ext_MINSA) + geom_sf(aes(fill=Prox)) + 
    geom_label_repel(data=IPRESS_ONC_Sim %>% filter(Equipos_RE>0), 
                     aes(x=LONGITUD, y=LATITUD, label=paste0(IPRESS_ABREV, " (", Equipos_RE, ")"), fill=IPRESS_ABREV),
                     size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                     segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
    labs(x = "", y = "", fill = "Institution") + 
    scale_fill_manual(values = Col_R.ES, name = "Institution") + 
    theme_minimal() + 
    theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
          axis.text.y = element_text(color = "black"),  # Eje Y en negro
          legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
          panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) +  
    annotate("text", x = -80, y = 0, label = "A", size = 13, family = "Calibri") 
  
  
  LC_MINSA = RT_Ext_MINSA %>% filter(NOMBPROV%in%c("LIMA", "CALLAO"))
  p_RE.Ext_MINSA_LC = ggplot(LC_MINSA) + geom_sf(aes(fill=Prox)) + 
    geom_text_repel(data=LC_MINSA, aes(x=centroid_LON, y=centroid_LAT, label=NOMBDIS),
                    size=2, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                    segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
    geom_label_repel(data=IPRESS_ONC_Sim %>% filter(Equipos_RE>0) %>% filter(IPRESS_ABREV%in%c("INEN", "HMA", "HNHU", "HNDAC", "HSEB")), 
                     aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                     size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                     segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
    labs(x = "", y = "", fill = "Institution") + 
    scale_fill_manual(values = Col_R.ES, name = "Institution") + 
    theme_minimal() + 
    theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
          axis.text.y = element_text(color = "black"),  # Eje Y en negro
          legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
          panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) + # Borde del panel
    annotate("text", x = -76.65, y = -11.6, label = "B", size = 13, family = "Calibri") 
  
  return(ggarrange(p_RE.Ext_MINSA, p_RE.Ext_MINSA_LC, nrow=1))
  
}

# ---------------------------------------------------------------------------- #


################################################################################
Result_DF = read.xlsx("Resultados Recargable 2035_V2.xlsx")
Result_DF$Cat_IPRESS = apply(Result_DF %>% 
                               select(-c("Ipress_NivelII", "Media_Pond", "Media", "Dist_out", "Dist_out.MC", "ID")), 1, leng_f)
glimpse(Result_DF)


### Analisis de simulacion
ggplot(Result_DF, aes(x=as.factor(Cat_IPRESS), y=Media_Pond)) + geom_boxplot()
ggplot(Result_DF, aes(x=as.factor(Ipress_NivelII), y=Media_Pond)) + geom_boxplot() + facet_wrap(~Cat_IPRESS)                                                                                    
ggplot(Result_DF, aes(x=Media, y=Media_Pond)) + geom_point()

xtabs(~Ipress_NivelII+Cat_IPRESS, Result_DF)

Result_DF %>% 
  group_by(Ipress_NivelII, Cat_IPRESS) %>% 
  summarise(min_T=convertir_a_hms(min(Media_Pond, na.rm=T))) %>% 
  pivot_wider(
    names_from=Cat_IPRESS,
    values_from=min_T)


ggplot(Result_DF, aes(x = as.factor(Cat_IPRESS), y = Media_Pond)) +
  geom_boxplot() +
  scale_y_continuous(
    breaks = seq(0, max(Result_DF$Media_Pond, na.rm = TRUE), by = 1200),  # Cada 1200 segundos (20 minutos)
    labels = function(x) sapply(x, convertir_a_hms)
  ) +
  labs(y = "Media Pond (hh:mm:ss)")
# Tenemos el minimo y una opcion recomendada

##### Minimo
# Min_Model = Result_DF %>%
#   filter(Ipress_NivelII==12, Cat_IPRESS==23) %>%  # Ipress_NivelII==11, Cat_IPRESS==22
#   slice_min(order_by = Media_Pond, n = 1) %>%
#   select(-c("Ipress_NivelII", "Media_Pond", "Media", "Dist_out", "Dist_out.MC", "ID", "Cat_IPRESS"))
# 
# # Min_Model = Result_DF %>% 
# #   filter(Dist_out.MC==0, Dist_out==0) %>%  # Ipress_NivelII==11, Cat_IPRESS==22
# #   slice_min(order_by = Media_Pond, n = 1) %>% 
# #   select(-c("Ipress_NivelII", "Media_Pond", "Media", "Dist_out", "Dist_out.MC", "ID", "Cat_IPRESS"))
# 
# 
# 
# df_Min_Model = Min_Model %>% 
#   pivot_longer(
#     cols = names(Min_Model),
#     names_to = "IPRESS_ABREV",
#     values_to = "Equipos_RE",
#     values_drop_na = TRUE) %>% 
#   filter(Equipos_RE>0)






##### Obtenemos los datos
Carp = "01. Porc ENDES\\"
Anho = 2035
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")

IPRESS_ONC = IPRESS_ONC %>% 
  select(IPRESS_ABREV, Categoria, LATITUD, LONGITUD)
Medida_Usar=Time
Pob_Map_Ubi = Pob_Map_Ubi[Pob_Map_Ubi$Anho==Anho,]


# graf_maps = Grafico_Menor.Distancia(df_Min_Model)
# print(graf_maps)



######### ANALISIS TOTAL:
Min_Model = Result_DF %>%
  slice_min(order_by = Media_Pond, n = 1) 



pdf("Resultados principal solo 1.pdf", width=20, height=10)


for (i in 1:nrow(Min_Model)) {
  
  df_Min_Model_ = Min_Model[i,] %>%
    select(-c("Ipress_NivelII", "Media_Pond", "Media", "Dist_out", "Dist_out.MC", "ID", "Cat_IPRESS")) 
  
  df_Min_Model = df_Min_Model_ %>% 
    pivot_longer(
      cols = names(df_Min_Model_),
      names_to = "IPRESS_ABREV",
      values_to = "Equipos_RE",
      values_drop_na = TRUE) %>% 
    filter(Equipos_RE>0)
  
  
  graf_maps = Grafico_Menor.Distancia(df_Min_Model)
  
  
  print(graf_maps)
  
  
  
}
dev.off()




