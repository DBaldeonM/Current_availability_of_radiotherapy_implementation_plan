

library(data.table)
library(openxlsx)
library(dplyr)
library(stringi)
library(naniar)

rm(list=ls())
setwd("C:\\Users\\Prospectiva01\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")

##############################################################
######################### FUNCIONES ##########################
accentless <- function(s) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOaeiouAEIOUycC",
    s);
}
##############################################################


## Poblacion es la BD de la estimacion de la poblacion (2018-2022) contiene 1890 distritos
Poblacion = read.xlsx("04. BD - Resultado usando R\\01. Poblacion INEI 2018-2022 y proyectada.xlsx")
Poblacion = data.table(Poblacion)

## TB_Ubigeo: BD que contiene la informacion de la latitud y longitud de 1893 distritos
Ubigeo_ND = read.csv("02. BD\\03. TB_UBIGEOS.csv")
Ubigeo_ND = data.table(Ubigeo_ND)
Ubigeo_ND = Ubigeo_ND %>% select(c("UBIGEO", "departamento", "provincia", "distrito", 
                                   "macroregion_inei", "macroregion_minsa", 
                                   "superficie", "Munic_LAT", "Munic_LOG"))
dim(na.omit(Ubigeo_ND))
vis_miss(Ubigeo_ND)

## Si hay ubigeos que no representen lo mismo
# w = merge(Ubigeo_ND, Poblacion, by="UBIGEO", all=T)
# View(w[UBIGEO%in%c(setdiff(Poblacion$UBIGEO, Ubigeo_ND$UBIGEO), setdiff(Ubigeo_ND$UBIGEO, Poblacion$UBIGEO)), ])


BD_Verif = merge(Ubigeo_ND, Poblacion, by="UBIGEO", all=T) # AGREGANDO POBLACION
BD_Verif[, REGION:=accentless(REGION)]
BD_Verif[, PROVINCIA:=accentless(PROVINCIA)]
BD_Verif[, DISTRITO:=accentless(DISTRITO)]


Dep = BD_Verif[departamento!=REGION,] 
table(Dep$departamento, Dep$REGION)# Sin problema

Prov = BD_Verif[provincia!=PROVINCIA,]
table(Prov$provincia, Prov$PROVINCIA)

Dist = BD_Verif[distrito!=DISTRITO,]
Dist[, c("distrito", "DISTRITO")] 
Dist[distrito=="CHUCATAMANI", ] # OBS: CHUCATAMANI y HEROES ALBARRACIN


########################################################
########################################################
Infor_Distritos = merge(Ubigeo_ND, 
                        Poblacion[, c("UBIGEO", "Anho_2023", "Anho_2030", "Anho_2035")], by="UBIGEO") # AGREGANDO POBLACION
Infor_Distritos = rename(Infor_Distritos, "Pbl_2023"="Anho_2023", "Pbl_2030"="Anho_2030", "Pbl_2035"="Anho_2035")
dim(Infor_Distritos) # 1890
dim(na.omit(Infor_Distritos)) # 1871
glimpse(Infor_Distritos)
vis_miss(Infor_Distritos)


## Informacion de 1889 distritos (Poblacion, latitud. lingitud, superficie)
write.xlsx(Infor_Distritos, "04. BD - Resultado usando R\\02. Infor_Distritos y poblacion proy.xlsx")




