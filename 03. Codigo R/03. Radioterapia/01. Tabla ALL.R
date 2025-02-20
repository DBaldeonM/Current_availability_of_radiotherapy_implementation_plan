
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
setwd("C:\\Users\\Prospectiva01\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")
#########################################################################
# CAMBIO DE CARPETA:
Carp = "01. Porc ENDES\\" # BD mapa: % pbl SIS cambiada
#########################################################################
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")


Accesibilidad = data.frame(rowSums(!is.na(Time[, c("INEN", "HRL.FSAI")]), na.rm=T)) %>% 
  rename(Accesibilidad=rowSums..is.na.Time...c..INEN....HRL.FSAI......na.rm...T.)
Accesibilidad$Identf = rownames(Accesibilidad)
Accesibilidad$API_GOOGLE = ifelse(Accesibilidad$Accesibilidad==0, "Inaccesible", "Accesible")
rownames(Accesibilidad) = NULL
Accesibilidad$Accesibilidad = NULL


Ex_Acceso = merge(Pob_Map_Ubi[Pob_Map_Ubi$Anho==2023,], Accesibilidad, by="Identf")
Acceso_TB = Ex_Acceso %>% data.frame() %>% 
  group_by(API_GOOGLE) %>% 
  summarise(
    Distritos=n(),
    Poblacion_2023=sum(PBL),
    Poblacion_Onc=sum(Pbl_Onc),
    Pbl_P.SIS_RE=sum(Apl_P.SIS_RE), 
    Superf=sum(superfc))

ggplot(Ex_Acceso, aes(fill=API_GOOGLE))+geom_sf()

write.xlsx(Acceso_TB, "09. Modelo RT\\01. Descripcion\\00. Accesibilidad - API Google.xlsx")


#####################################

TB_M01_Aux = data.table(Pob_Map_Ubi)[,.(Distritos=.N, 
                                        Poblacion=sum(PBL),
                                        Pbl_Onc=sum(Pbl_Onc),
                                        Apl_P_On_RE=sum(Apl_P.Onc_RE),
                                        Apl_P_On_BQ=sum(Apl_P.Onc_BQ)),
                                     keyby=.(NOMBDEP, Anho)]
TB_M01 = TB_M01_Aux %>%
  pivot_wider(
    names_from = Anho,
    values_from = c(Poblacion, Pbl_Onc, Apl_P_On_RE, Apl_P_On_BQ))


TB_M01 = TB_M01[, c("NOMBDEP", "Distritos",        
           "Poblacion_2023", "Pbl_Onc_2023", "Apl_P_On_RE_2023", "Apl_P_On_BQ_2023",
           "Poblacion_2030", "Pbl_Onc_2030", "Apl_P_On_RE_2030", "Apl_P_On_BQ_2030",
           "Poblacion_2035", "Pbl_Onc_2035", "Apl_P_On_RE_2035", "Apl_P_On_BQ_2035")]


write.xlsx(TB_M01 %>% arrange(desc(Poblacion_2023)), "09. Modelo RT\\01. Descripcion\\01. Apl y Pbl Radioterapia.xlsx")


#####################################
IPRESS_ONC = rbind(data.table(IPRESS_ONC_23, Anho=2023), 
                   data.table(IPRESS_ONC_30, Anho=2030),
                   data.table(IPRESS_ONC_35, Anho=2035))

IPRESS_ONC = merge(IPRESS_ONC, distinct(data.table(Pob_Map_Ubi)[, c("UBIGEO", "mcrrgn_")]), 
                 by="UBIGEO", all.x=T)

Ipress_RT2_PW = IPRESS_ONC %>% 
  group_by(Anho, mcrrgn_, INSTITUCION) %>% 
  summarise(Equip_RE=sum(Equipos_RE),
            Equip_BQ=sum(Equipos_BQ)) %>% 
  pivot_wider(names_from=INSTITUCION, 
              values_from=c(Equip_RE, Equip_BQ),
              values_fill=0)
  

Ipress_RT2_PW = Ipress_RT2_PW[, c("Anho", "mcrrgn_", "Equip_RE_MINSA", "Equip_RE_ESSALUD", "Equip_RE_PRIVADO", 
                                  "Equip_BQ_MINSA", "Equip_BQ_ESSALUD", "Equip_BQ_PRIVADO")]

categorias = expand.grid(Anho = unique(Ipress_RT2_PW$Anho), 
                          mcrrgn_ = c("LIMA MET", "NORTE", "CENTRO", "SUR", "ORIENTE"))

df_completo = right_join(Ipress_RT2_PW, categorias, by = c("Anho", "mcrrgn_"))
df_completo[is.na(df_completo)] = 0
df_completo$mcrrgn_ = factor(df_completo$mcrrgn_, levels = c("LIMA MET", "NORTE", "CENTRO", "SUR", "ORIENTE"))
df_final = df_completo %>% arrange(Anho, mcrrgn_)


write.xlsx(df_final, "09. Modelo RT\\01. Descripcion\\02. MR Cantidad de Equipos.xlsx")


######
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")

IPRESS_ONC = rbind(data.table(IPRESS_ONC_23, Anho=2023), 
                   data.table(IPRESS_ONC_30, Anho=2030),
                   data.table(IPRESS_ONC_35, Anho=2035))

Tab_Nec_aux = IPRESS_ONC[,.(Equi_RE=sum(Equipos_RE)), keyby=.(Anho, INSTITUCION)]
Tab_Nec_aux$Apl_RE_Disp = Tab_Nec_aux$Equi_RE*240*60
Tab_Nec_aux[, ID:=paste0(Anho, INSTITUCION)]

D_Aux_ = data.table(Pob_Map_Ubi)[, .(MINSA=sum(Apl_P.SIS_RE),
                                     ESSALUD=sum(Apl_P.EsD_RE),
                                     PRIVADO=sum(Apl_P.PV_RE)),keyby=Anho]



D_Aux_ = pivot_longer(D_Aux_, !Anho, names_to="INSTITUCION", values_to="Aplicaciones")
D_Aux_$ID = paste0(D_Aux_$Anho, D_Aux_$INSTITUCION)

Tab_Nec = merge(D_Aux_, select(Tab_Nec_aux, -c("Anho", "INSTITUCION")), by="ID", all.x=T)
Tab_Nec$ID = NULL
Tab_Nec$INSTITUCION = factor(Tab_Nec$INSTITUCION, levels = c("MINSA", "ESSALUD", "PRIVADO"))


write.xlsx(Tab_Nec %>% arrange(Anho, INSTITUCION), "09. Modelo RT\\01. Descripcion\\03. Distribucion de SS.xlsx")

