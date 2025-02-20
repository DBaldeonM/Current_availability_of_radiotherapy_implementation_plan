
library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(data.table)
library(openxlsx)
library(dplyr)
library(gmapsdistance)  # for getting info from the API
library(ggvenn)


rm(list=ls())
setwd("C:\\Users\\Prospectiva01\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")

## Informacion de 1890 distritos (Poblacion, latitud. lingitud, superficie)
Infor_Distritos = read.xlsx("04. BD - Resultado usando R\\02. Infor_Distritos y poblacion proy.xlsx")


## Informacion de 1890 distritos (Poblacion, latitud. lingitud, superficie)
Porc_SIS = read.xlsx("04. BD - Resultado usando R\\03. Porcentaje Asegurados SIS Y ESSALUD.xlsx")
Porc_SIS = Porc_SIS %>% 
  mutate(provincia=ifelse(provincia=="NAZCA", "NASCA", provincia),
         DP=paste0(departamento, "-", provincia)) %>% 
  select(DP,Seg_SIS,Seg_ESSD)

# length(unique(Porc_SIS$DP)) # 188

## peru_d es la BD para maps en R, contiene 1891 distritos
Peru_map = st_read("01. LIMITE_DISTRITAL_INEI_geogpsperu\\LIMITE_DISTRITAL_INEI_geogpsperu.shp")
Peru_map$UBIGEO = as.numeric(Peru_map$UBIGEO)
Peru_map = Peru_map %>% 
  select(c("UBIGEO", "DEPARTAMEN", "PROVINCIA", "DISTRITO", "geometry")) %>% 
  mutate(centroid = map(geometry, st_centroid), 
                           coords = map(centroid, st_coordinates), 
                           centroid_LAT = map_dbl(coords, 2), 
                           centroid_LON = map_dbl(coords, 1)) %>% # Calculo de centroide
  select(-c("centroid", "coords")) %>% 
  rename(NOMBDEP=DEPARTAMEN, NOMBPROV=PROVINCIA, NOMBDIST=DISTRITO)


# lista_conjuntos = list(
#   Peru_map = Peru_map$UBIGEO,
#   Infor_Distritos = Infor_Distritos$UBIGEO)
# ggvenn(lista_conjuntos, c("Peru_map", "Infor_Distritos"))


## Pob_Map_Ubi: Contiene de 1980 distritos del Peru
Pob_Map_Ubi = merge(Peru_map, Infor_Distritos %>% select(-c("departamento", "provincia", "distrito")), by="UBIGEO", all.y=T) # 1890   15
glimpse(Pob_Map_Ubi)


Pob_Map_Ubi$Identf = paste0(Pob_Map_Ubi$NOMBDEP, "_", Pob_Map_Ubi$NOMBPROV, "_", Pob_Map_Ubi$NOMBDIST) # Identificar el Distrito
Pob_Map_Ubi$DP = paste0(Pob_Map_Ubi$NOMBDEP, "-", Pob_Map_Ubi$NOMBPROV)


# ELEJIMOS LA ALTITUD Y LONGITUD SEGUN EL CRITERIO DE LIMA Y CALLAO <- Sirve para recordar como se calculo la distancia
# Pob_Map_Ubi$Distrito_LAT.LON = ""
# for (i in 1:nrow(Pob_Map_Ubi)) {
#   if(Pob_Map_Ubi[i,]$NOMBPROV %in% c("CALLAO", "LIMA")){
#     Pob_Map_Ubi[i,]$Distrito_LAT.LON = paste0(Pob_Map_Ubi[i,]$centroid_LAT, "+",
#                                               Pob_Map_Ubi[i,]$centroid_LON)
#   }else{
#     Pob_Map_Ubi[i,]$Distrito_LAT.LON = paste0(Pob_Map_Ubi[i,]$Munic_LAT, "+",
#                                               Pob_Map_Ubi[i,]$Munic_LOG)
#   }
# }
# glimpse(Pob_Map_Ubi)



#####################
#####################
Pob_Map_Ubi = merge(Pob_Map_Ubi, Porc_SIS, by="DP", all.x=T)
Pob_Map_Ubi = Pob_Map_Ubi %>% 
  select(c("UBIGEO", "Identf" , "NOMBDEP", "NOMBPROV", "NOMBDIST", "macroregion_inei", 
           "macroregion_minsa", "centroid_LAT", "centroid_LON", "Pbl_2023", "Pbl_2030", 
           "Pbl_2035", "Seg_SIS", "Seg_ESSD", "superficie","geometry"))  # Se retira: "DP", "Munic_LAT", "Munic_LOG", "superficie"
  

#####################
#####################
Casos_Nuevos = read.xlsx("02. BD\\06. Proyeccion Nuevos Casos.xlsx", sheet="CN")
Casos_Nuevos = Casos_Nuevos %>% 
  filter(Anho!=2020) %>% 
  select(-"Total.CN") %>% 
  pivot_longer(
    cols = -Anho, 
    names_to = "Cancer", 
    values_to = "CN")

Total_CN = read.xlsx("02. BD\\06. Proyeccion Nuevos Casos.xlsx", sheet="CN") %>% select(Anho,Total.CN)


Porc_Uso = read.xlsx("02. BD\\06. Proyeccion Nuevos Casos.xlsx", sheet="Porc_Uso")
Porc_Uso = Porc_Uso %>% 
  pivot_longer(
    cols = -Tipo, 
    names_to = "Cancer", 
    values_to = "Proc_Uso") %>%  
  pivot_wider(
    names_from = Tipo, 
    values_from = Proc_Uso,
    names_prefix = "Porc_Uso_") %>% 
  rename(P_Uso.RT.E=`Porc_Uso_RT Externa`, P_Uso.BQ=Porc_Uso_Braquiterapia)

Casos_RT = merge(Casos_Nuevos, Porc_Uso) %>% 
  mutate(
    Casos_RT.E=CN*P_Uso.RT.E,
    Casos_BQ=CN*P_Uso.BQ)

Total_CN.RT = Casos_RT %>% 
  group_by(Anho) %>% 
  summarise(
    C_RT.E=round(sum(Casos_RT.E), 0),
    C_BQ=round(sum(Casos_BQ), 0))


########## TAB IMPORTANTE!!! 
Tb_Conv = merge(merge(Pob_Map_Ubi %>% 
                  data.frame() %>% 
                  summarise(
                    "2023"=sum(Pbl_2023),
                    "2030"=sum(Pbl_2030),
                    "2035"=sum(Pbl_2035)) %>% 
                  gather(Anho, T_PBL), Total_CN), Total_CN.RT) %>% 
  mutate(
    ind_CN=Total.CN/T_PBL,
    ind_C_RT.E=C_RT.E/T_PBL,
    ind_C_BQ=C_BQ/T_PBL)

Tb_Conv = merge(Tb_Conv, data.frame(Anho=c("2023", "2030", "2035"), Ajust_SIS_RT=c(263573, 398482, 539169))) %>% 
  mutate(ind_Ajust_SIS_RT=Ajust_SIS_RT/T_PBL)



######## DATOS DE INCIDENCIA
dat_TEE.23 = data.table(Pob_Map_Ubi)[, c("UBIGEO", "NOMBDIST", "Seg_SIS", "Seg_ESSD", "Pbl_2023")]
dat_TEE.30 = data.table(Pob_Map_Ubi)[, c("UBIGEO", "NOMBDIST", "Seg_SIS", "Seg_ESSD", "Pbl_2030")]
dat_TEE.35 = data.table(Pob_Map_Ubi)[, c("UBIGEO", "NOMBDIST", "Seg_SIS", "Seg_ESSD", "Pbl_2035")]

dat_TEE.23 = dat_TEE.23 %>% rename(PBL="Pbl_2023") %>% mutate(Anho=2023)
dat_TEE.30 = dat_TEE.30 %>% rename(PBL="Pbl_2030") %>% mutate(Anho=2030)
dat_TEE.35 = dat_TEE.35 %>% rename(PBL="Pbl_2035") %>% mutate(Anho=2035)

dat_TEE_aux = rbind(dat_TEE.23, dat_TEE.30, dat_TEE.35) 



Aplc_RE = 20
Aplc_BQ = 4
dat_TEE_aux = dat_TEE_aux %>% 
  mutate(
    Pbl_Onc=case_when(
      Anho==2023~round(PBL*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_CN, 0),
      Anho==2030~round(PBL*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_CN, 0),
      Anho==2035~round(PBL*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_CN, 0)),
    
    A_Onc_R=case_when(
      Anho==2023~round(Aplc_RE*PBL*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_RT.E, 0),
      Anho==2030~round(Aplc_RE*PBL*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_RT.E, 0),
      Anho==2035~round(Aplc_RE*PBL*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_RT.E, 0)),
    A.SIS_R=case_when(
      Anho==2023~round(Aplc_RE*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_RT.E, 0),
      Anho==2030~round(Aplc_RE*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_RT.E, 0),
      Anho==2035~round(Aplc_RE*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_RT.E, 0)),
    A.EsD_R=case_when(
      Anho==2023~round(Aplc_RE*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_RT.E, 0),
      Anho==2030~round(Aplc_RE*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_RT.E, 0),
      Anho==2035~round(Aplc_RE*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_RT.E, 0)),
    A.PV_R = A_Onc_R - (A.SIS_R + A.EsD_R),
    A.SIS_R = ifelse(A.PV_R<0, A.SIS_R+A.PV_R, A.SIS_R),
    A.PV_R = ifelse(A.PV_R<0, 0, A.PV_R),
    ###### 
    Aj.Ap.SIS_R=case_when(
      Anho==2023~round(PBL*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_Ajust_SIS_RT, 0),
      Anho==2030~round(PBL*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_Ajust_SIS_RT, 0),
      Anho==2035~round(PBL*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_Ajust_SIS_RT, 0)),
    
    
    
    
    A_Onc_B=case_when(
      Anho==2023~round(Aplc_BQ*PBL*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_BQ, 0),
      Anho==2030~round(Aplc_BQ*PBL*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_BQ, 0),
      Anho==2035~round(Aplc_BQ*PBL*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_BQ, 0)),
    A.SIS_B=case_when(
      Anho==2023~round(Aplc_BQ*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_BQ, 0),
      Anho==2030~round(Aplc_BQ*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_BQ, 0),
      Anho==2035~round(Aplc_BQ*PBL*Seg_SIS*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_BQ, 0)),
    A.EsD_B=case_when(
      Anho==2023~round(Aplc_BQ*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2023, ]$ind_C_BQ, 0),
      Anho==2030~round(Aplc_BQ*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2030, ]$ind_C_BQ, 0),
      Anho==2035~round(Aplc_BQ*PBL*Seg_ESSD*Tb_Conv[Tb_Conv$Anho==2035, ]$ind_C_BQ, 0)),
    A.PV_B = A_Onc_B - (A.SIS_B + A.EsD_B),
    A.SIS_B = ifelse(A.PV_B<0, A.SIS_B+A.PV_B, A.SIS_B),
    A.PV_B = ifelse(A.PV_B<0, 0, A.PV_B)) 

dat_TEE = dat_TEE_aux %>% 
  mutate(
    Pbl_Onc=case_when(
    Anho==2023 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Pbl_Onc+14,
    Anho==2030 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Pbl_Onc+10,
    Anho==2035 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Pbl_Onc+13,
    TRUE~Pbl_Onc),
    
    Aj.Ap.SIS_R=case_when(
      Anho==2023 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Aj.Ap.SIS_R-1,
      Anho==2030 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Aj.Ap.SIS_R-21,
      Anho==2035 & NOMBDIST=="SAN JUAN DE LURIGANCHO"~Aj.Ap.SIS_R+13,
      TRUE~Aj.Ap.SIS_R)) 



Pob_Map_Ubi = merge(select(Pob_Map_Ubi, -c("macroregion_minsa", "Pbl_2023", "Pbl_2030", "Pbl_2035", "Seg_SIS", "Seg_ESSD")),
                    select(dat_TEE, -c("NOMBDIST", "Seg_SIS", "Seg_ESSD")), 
                    by="UBIGEO", all.y=T) %>% 
  rename(mcrrgn_=macroregion_inei)

Pob_Map_Ubi$mcrrgn_ = str_replace_all(Pob_Map_Ubi$mcrrgn_, "METROPOLITANA", "MET")



st_write(Pob_Map_Ubi, "04. BD - Resultado usando R\\01. Porc ENDES\\Pob_Map_Ubi_MULTIPOLYGON.shp")


#################################################################################################
stf = st_read("04. BD - Resultado usando R\\01. Porc ENDES\\Pob_Map_Ubi_MULTIPOLYGON.shp")
dim(stf)
glimpse(stf)
names(stf)




