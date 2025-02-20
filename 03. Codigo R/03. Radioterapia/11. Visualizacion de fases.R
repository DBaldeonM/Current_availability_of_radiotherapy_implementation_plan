
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
#########################################################################################
# CAMBIO DE CARPETA:
Carp = "02. Apl SIS no cubiertas por PV\\" # BD mapa: % pbl SIS cambiada
Anho = 2023
#########################################################################################


#########################################################################################
######################################### FASE 01 ####################################### 
source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
fase = "Fase 01"
IPRESS_ONC = IPRESS_ONC[Anho_==Anho,]
IPRESS_ONC[IPRESS_ABREV%in%c("HNCH", "HNHU", "HMA"), Equipos_RE:=1]
IPRESS_ONC[IPRESS_ABREV%in%c("INEN"), Equipos_RE:=Equipos_RE+1]
IPRESS_ONC = IPRESS_ONC[Equipos_RE>0, ]
sum(IPRESS_ONC$Equipos_RE)
source("03. Codigo R\\03. Radioterapia\\08. Implementacion por fases.r")



#########################################################################################
######################################### FASE 02 ####################################### 
source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
fase = "Fase 02"
IPRESS_ONC = IPRESS_ONC[Anho_==Anho,]
IPRESS_ONC[IPRESS_ABREV%in%c("HNCH", "HNHU", "HMA"), Equipos_RE:=1] # Fase 01
IPRESS_ONC[IPRESS_ABREV%in%c("INEN"), Equipos_RE:=Equipos_RE+1] # Fase 01
IPRESS_ONC[IPRESS_ABREV%in%c("HRL.FSAI"), Equipos_RE:=1] # Fase 02
IPRESS_ONC[IPRESS_ABREV%in%c("HT"), Equipos_RE:=1] # Fase 02
IPRESS_ONC = IPRESS_ONC[Equipos_RE>0, ]
sum(IPRESS_ONC$Equipos_RE)
source("03. Codigo R\\03. Radioterapia\\08. Implementacion por fases.r")



#########################################################################################
######################################### FASE 03 ####################################### 
source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
fase = "Fase 03"
IPRESS_ONC = IPRESS_ONC[Anho_==Anho,]
IPRESS_ONC[IPRESS_ABREV%in%c("HNCH", "HNHU", "HMA"), Equipos_RE:=1] # Fase 01
IPRESS_ONC[IPRESS_ABREV%in%c("INEN"), Equipos_RE:=Equipos_RE+1] # Fase 01
IPRESS_ONC[IPRESS_ABREV%in%c("HRL.FSAI"), Equipos_RE:=1] # Fase 02
IPRESS_ONC[IPRESS_ABREV%in%c("HT"), Equipos_RE:=1] # Fase 02
IPRESS_ONC[IPRESS_ABREV%in%c("IREN.CENTRO"), Equipos_RE:=Equipos_RE+2] # Fase 03
IPRESS_ONC = IPRESS_ONC[Equipos_RE>0, ]
sum(IPRESS_ONC$Equipos_RE)
source("03. Codigo R\\03. Radioterapia\\08. Implementacion por fases.r")


#########################################################################################
######################################### FASE 04  y 05 #################################

rm(list=ls())
setwd("F:\\05. Inestigaciones RON\\02. Tratamiento de Radioterapia")
#########################################################################################
# CAMBIO DE CARPETA:
Carp = "02. Apl SIS no cubiertas por PV\\" # BD mapa: % pbl SIS cambiada
Anho = 2023
#########################################################################################


fase = "Fase 04 y 05"
df_Comb = read.xlsx("09. Modelo RT\\Implementacion por fases 2023\\df_Comb.xlsx")
df_Comb = data.table(df_Comb)

k = 0
pdf(paste0("09. Modelo RT\\Implementacion por fases ", Anho, "\\01. ALCANCE DE IPRESS MINSA ", fase, ".pdf"), width=20, height=10)
for (ac in df_Comb$Agre.Ceros[1:30]) {
  k = k + 1
  source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
  
  IPRESS_ONC = IPRESS_ONC[Anho_==Anho,]
  IPRESS_ONC[IPRESS_ABREV%in%c("HNCH", "HNHU", "HMA"), Equipos_RE:=1] # Fase 01
  IPRESS_ONC[IPRESS_ABREV%in%c("INEN"), Equipos_RE:=Equipos_RE+1] # Fase 01
  IPRESS_ONC[IPRESS_ABREV%in%c("HRL.FSAI"), Equipos_RE:=1] # Fase 02
  IPRESS_ONC[IPRESS_ABREV%in%c("HT"), Equipos_RE:=1] # Fase 02
  IPRESS_ONC[IPRESS_ABREV%in%c("IREN.CENTRO"), Equipos_RE:=Equipos_RE+2] # Fase 03
  
  iren.N = df_Comb[Agre.Ceros==ac, IREN.NORTE]
  hbt = df_Comb[Agre.Ceros==ac, HBT]
  hrl = df_Comb[Agre.Ceros==ac, HRL]
  
  iren.S = df_Comb[Agre.Ceros==ac, IREN.Sur]
  hrhde = df_Comb[Agre.Ceros==ac, HRHDE]
  hg = df_Comb[Agre.Ceros==ac, HG]
  halc = df_Comb[Agre.Ceros==ac, HALC]
  hadc = df_Comb[Agre.Ceros==ac, HADC]
  
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
  
  sum(IPRESS_ONC$Equipos_RE)
  source("03. Codigo R\\03. Radioterapia\\08.1. Implementacion por fases dinamico.r")
  
  print(p_RE.Ext_MINSA)
  
}

dev.off()






