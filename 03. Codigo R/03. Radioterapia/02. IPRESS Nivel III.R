
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
setwd("C:\\Users\\Asus\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")
#########################################################################
# CAMBIO DE CARPETA:
Carp = "01. Porc ENDES\\" # BD mapa: % pbl SIS cambiada
#########################################################################
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")

IPRESS_ONC_23 = IPRESS_ONC_23 %>% mutate(
  INSTITUCION=ifelse(INSTITUCION=="PRIVADO", "PRIVATE", INSTITUCION))
IPRESS_ONC_30 = IPRESS_ONC_30 %>% mutate(
  INSTITUCION=ifelse(INSTITUCION=="PRIVADO", "PRIVATE", INSTITUCION))
IPRESS_ONC_35 = IPRESS_ONC_35 %>% mutate(
  INSTITUCION=ifelse(INSTITUCION=="PRIVADO", "PRIVATE", INSTITUCION))

Inst_G = c("ESSALUD"="#0082C6", "MINSA"="#00BA38", "PRIVATE"="#e86af0")
Pob_Map_Ubi$Apl_P.Onc_RE[Pob_Map_Ubi$NOMBPROV%in%c("LIMA", "CALLAO")] = NA

Reg_RE.ES_23 = ggplot(Pob_Map_Ubi %>% filter(Anho==2023)) + 
  geom_sf(aes(fill=Apl_P.Onc_RE)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Required\napplications\nper district", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_23,
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Insurance") + 
  labs(x="", y="", title="INSTITUTIONS WITH ACTIVE RADIOTHERAPY DEVICES", subtitle="2023") + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
        axis.text.y = element_text(color = "black"),  # Eje Y en negro
        legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
        panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) 

Reg_RE.ES_30 = ggplot(Pob_Map_Ubi %>% filter(Anho==2030)) + 
  geom_sf(aes(fill=Apl_P.Onc_RE)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Required\napplications\nper district", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_30,
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Insurance") + 
  labs(x="", y="", title="INSTITUTIONS WITH ACTIVE RADIOTHERAPY DEVICES", subtitle="2030") + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
        axis.text.y = element_text(color = "black"),  # Eje Y en negro
        legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
        panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) 

Reg_RE.ES_35 = ggplot(Pob_Map_Ubi %>% filter(Anho==2035)) + 
  geom_sf(aes(fill=Apl_P.Onc_RE)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Required\napplications\nper district", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_35,
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Insurance") + 
  labs(x="", y="", title="INSTITUTIONS WITH ACTIVE RADIOTHERAPY DEVICES", subtitle="2035") + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
        axis.text.y = element_text(color = "black"),  # Eje Y en negro
        legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
        panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) 


pdf("09. Modelo RT\\01. Descripcion\\01. IPRESS Con RAD EXT.pdf", width=20, height=10)
print(ggarrange(Reg_RE.ES_23, Reg_RE.ES_30, Reg_RE.ES_35, nrow=1))
dev.off() 


###############################
###############################
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")


Inst_G = c("ESSALUD"="#0082C6", "MINSA"="#00BA38", "PRIVADO"="#e86af0")
Pob_Map_Ubi$Apl_P.Onc_BQ[Pob_Map_Ubi$NOMBPROV%in%c("LIMA", "CALLAO")] = NA

Reg_BQ.ES_23 = ggplot(Pob_Map_Ubi %>% filter(Anho==2023)) + 
  geom_sf(aes(fill=Apl_P.Onc_BQ)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Aplicaciones\nRequiere BQ", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_23[Equipos_BQ>0,],
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Sistema \nde Salud") + 
  labs(x="", y="", title="IPRESS CON SERVICIO EN BRAQUITERAPIA", subtitle="2023") + theme_minimal()

Reg_BQ.ES_30 = ggplot(Pob_Map_Ubi %>% filter(Anho==2030)) + 
  geom_sf(aes(fill=Apl_P.Onc_BQ)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Aplicaciones\nRequiere BQ", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_30[Equipos_BQ>0,],
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Sistema \nde Salud") + 
  labs(x="", y="", title="IPRESS CON SERVICIO EN BRAQUITERAPIA", subtitle="2030") + theme_minimal()

Reg_BQ.ES_35 = ggplot(Pob_Map_Ubi %>% filter(Anho==2035)) + 
  geom_sf(aes(fill=Apl_P.Onc_BQ)) +
  scale_fill_gradient(low="#d4af73", high="#9f376c", name="Aplicaciones\nRequiere BQ", 
                      labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC_35[Equipos_BQ>0,],
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=INSTITUCION),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Inst_G, name="Sistema \nde Salud") + 
  labs(x="", y="", title="IPRESS CON SERVICIO EN BRAQUITERAPIA", subtitle="2035") + theme_minimal()


pdf("09. Modelo RT\\01. Descripcion\\02. IPRESS Con Braquiterapia.pdf", width=20, height=10)
print(ggarrange(Reg_BQ.ES_23, Reg_BQ.ES_30, Reg_BQ.ES_35, nrow=1))
dev.off() 


#############################################################################################
#############################################################################################


Col_G = c("II-1"="#CC79A7", "II-2"="#E69F00", "II-E"="#D55E00", 
          "III-1"="#0072B2", "III-2"="#009E73", "III-E"="#548235")

M_LC = ggplot(Pob_Map_Ubi %>% filter(Anho==2023) %>% filter(NOMBPROV%in%c("LIMA", "CALLAO"))) +
  geom_sf(aes(fill=Pbl_Onc)) +
  scale_fill_gradient(low="#DDB509", high="#7030A0", name="Población \nOncológica", labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
  geom_text_repel(aes(x=centroid_LON, y=centroid_LAT, label=NOMBDIS), size=1.9) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC %>% filter(Provincia%in%c("LIMA", "CALLAO")) 
                   %>% filter(Categoria %in% c("III-1", "III-2", "III-E")),
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=Categoria),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_G, name="Categoría") + 
  labs(x="", y="", title="IPRESS POTENCIALMENTE APTOS PARA RADIOTERAPIA", subtitle="LIMA METROPOLITANA Y CALLAO") + theme_minimal()



Pob_Map_Ubi$Pbl_Onc[Pob_Map_Ubi$NOMBPROV%in%c("LIMA", "CALLAO")] = NA
Aux_IONC = IPRESS_ONC[!Provincia%in%c("LIMA", "CALLAO") & (Categoria %in% c("III-1", "III-2", "III-E") | IPRESS_ABREV%in%c("IREN.CENTRO"))]
Aux_IONC = Aux_IONC[INSTITUCION=="MINSA",]
M_P = ggplot(Pob_Map_Ubi %>% filter(Anho==2023)) +
  geom_sf(aes(fill=Pbl_Onc)) +
  scale_fill_gradient(low="#DDB509", high="#7030A0", name="Población \nOncológica", labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
  new_scale_fill() +
  geom_label_repel(data=Aux_IONC,
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=Categoria),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_G, name="Categoría") + 
  labs(x="", y="", title="IPRESS POTENCIALMENTE APTOS PARA RADIOTERAPIA", subtitle="REGIONES") + theme_minimal()


pdf("09. Modelo RT\\01. Descripcion\\03. IPRESS potencialmente aptos - Peru.pdf", width=20, height=10)
print(ggarrange(M_P, M_LC, nrow=1))
dev.off() 




################################### ESSALUD #################################################
#############################################################################################


Col_G = c("II-1"="#CC79A7", "II-2"="#E69F00", "II-E"="#D55E00", 
          "III-1"="#0072B2", "III-2"="#009E73", "III-E"="#548235")

M_LC = ggplot(Pob_Map_Ubi %>% filter(Anho==2023) %>% filter(NOMBPROV%in%c("LIMA", "CALLAO"))) +
  geom_sf(aes(fill=Pbl_Onc)) +
  scale_fill_gradient(low="#DDB509", high="#7030A0", name="Población \nOncológica", labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
  geom_text_repel(aes(x=centroid_LON, y=centroid_LAT, label=NOMBDIS), size=1.9) +
  new_scale_fill() +
  geom_label_repel(data=IPRESS_ONC %>% filter(Provincia%in%c("LIMA", "CALLAO")) 
                   %>% filter(Categoria %in% c("III-1", "III-2", "III-E")),
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=Categoria),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_G, name="Categoría") + 
  labs(x="", y="", title="IPRESS POTENCIALMENTE APTOS PARA RADIOTERAPIA", subtitle="LIMA METROPOLITANA Y CALLAO") + theme_minimal()



Pob_Map_Ubi$Pbl_Onc[Pob_Map_Ubi$NOMBPROV%in%c("LIMA", "CALLAO")] = NA
Aux_IONC = IPRESS_ONC[!Provincia%in%c("LIMA", "CALLAO") & (Categoria %in% c("III-1", "III-2", "III-E") | IPRESS_ABREV%in%c("IREN.CENTRO"))]
Aux_IONC = Aux_IONC[INSTITUCION=="MINSA",]
M_P = ggplot(Pob_Map_Ubi %>% filter(Anho==2023)) +
  geom_sf(aes(fill=Pbl_Onc)) +
  scale_fill_gradient(low="#DDB509", high="#7030A0", name="Población \nOncológica", labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
  new_scale_fill() +
  geom_label_repel(data=Aux_IONC,
                   aes(LONGITUD, LATITUD, label=IPRESS_ABREV, fill=Categoria),
                   size=3.6, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_G, name="Categoría") + 
  labs(x="", y="", title="IPRESS POTENCIALMENTE APTOS PARA RADIOTERAPIA", subtitle="REGIONES") + theme_minimal()


pdf("09. Modelo RT\\01. Descripcion\\03. IPRESS potencialmente aptos - Peru.pdf", width=20, height=10)
print(ggarrange(M_P, M_LC, nrow=1))
dev.off() 













