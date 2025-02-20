
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

dat_inf = read.xlsx("Analisis de posibles IPRESS.xlsx", sheet="Hoja1")

Pob_Map_Ubi = st_read("01. LIMITE_DISTRITAL_INEI_geogpsperu\\Dpto_Maps\\DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")
Pob_Map_Ubi = Pob_Map_Ubi %>% 
  select(NOMBDEP, geometry) %>% 
  mutate(centroid = map(geometry, st_centroid), 
         coords = map(centroid, st_coordinates), 
         centroid_LAT = map_dbl(coords, 2), 
         centroid_LON = map_dbl(coords, 1)) %>% # Calculo de centroide
  select(-c("centroid", "coords"))


Pob_Map_Ubi_Inf = merge(Pob_Map_Ubi, dat_inf) %>% 
  mutate(Nom_Cant=case_when(
    RE=="Si"~paste0(NOMBDEP, " (", Eq_No.Comp, ")"),
    RE=="No"~NOMBDEP),
    Apl.2030=ifelse(NOMBDEP=="LIMA", NA, Apl.2030))


ggplot(Pob_Map_Ubi_Inf, aes(fill=RE))+geom_sf()+geom_text(aes(x=centroid_LON, y=centroid_LAT, label=Nom_Cant), size=2.5)
ggplot(Pob_Map_Ubi_Inf, aes(fill=Apl.2030))+geom_sf()+geom_text(aes(x=centroid_LON, y=centroid_LAT, label=Nom_Cant), size=2.5)



n_dig=12
suma=18
choose(suma+n_dig-1, n_dig-1) # 34 597 290









