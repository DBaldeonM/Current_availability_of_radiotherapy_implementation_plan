
library(data.table)
library(openxlsx)
library(dplyr)

rm(list=ls())
setwd("C:\\Users\\Prospectiva01\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")


Proy_pob = read.xlsx("02. BD\\02. Proyeccion de Poblacion-statisticstimes.xlsx")


dat = read.xlsx("02. BD\\01. Poblacion Peru-Distritos.xlsx", sheet="BD_Limpia")
dat = data.table(dat)
dat[, UBIGEO:=as.numeric(UBIGEO)]
dat[, Anho_2018:=as.numeric(Anho_2018)]
dat[, Anho_2019:=as.numeric(Anho_2019)]
dat[, Anho_2020:=as.numeric(Anho_2020)]
dat[, Anho_2021:=as.numeric(Anho_2021)]
dat[, Anho_2022:=as.numeric(Anho_2022)]
summary(dat)



dat = dat[order(dat$UBIGEO), ]
Poblacion = data.table()
for (ubi in dat$UBIGEO) {
  if(ubi %% 10000 == 0){
    REGION = dat[UBIGEO==ubi, DEPPORVDIST]
    
  }else{
    if(ubi %% 100 == 0){
      PROVINCIA = dat[UBIGEO==ubi, DEPPORVDIST]
      dat[UBIGEO==ubi, DEPPORVDIST]
      Poblacion = rbind(Poblacion, data.frame(REGION, PROVINCIA, dat[dat$UBIGEO-ubi>0 & dat$UBIGEO-ubi<100, ]))
    }
  }
}

Poblacion = rename(Poblacion, "DISTRITO"="DEPPORVDIST")

Poblacion = Poblacion %>% 
  mutate(Anho_2023=round(Anho_2022*Proy_pob[Proy_pob$ANHO==2023, ]$POBLACION/sum(Anho_2022)),
         Anho_2030=round(Anho_2022*Proy_pob[Proy_pob$ANHO==2030, ]$POBLACION/sum(Anho_2022)),
         Anho_2035=round(Anho_2022*Proy_pob[Proy_pob$ANHO==2035, ]$POBLACION/sum(Anho_2022)))


colSums(Poblacion %>% select(Anho_2022, Anho_2023,Anho_2030, Anho_2035))

Proy_pob %>% filter(ANHO %in% c(2022, 2023, 2030, 2035))



## Poblacion es la BD de la estimacion de la poblacion (2018-2022) contiene 1890 distritos
write.xlsx(Poblacion, "04. BD - Resultado usando R\\01. Poblacion INEI 2018-2022 y proyectada.xlsx")


