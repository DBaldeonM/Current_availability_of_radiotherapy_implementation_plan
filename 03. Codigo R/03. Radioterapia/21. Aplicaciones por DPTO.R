
rm(list=ls())
setwd("F:\\05. Inestigaciones RON\\02. Tratamiento de Radioterapia")
#########################################################################################
# CAMBIO DE CARPETA:
Carp = "02. Apl SIS no cubiertas por PV\\" # BD mapa: % pbl SIS cambiada
Anho_Map = 2030

source("03. Codigo R\\02. Funciones y Get Data\\02. Llamado de BD - Radioterapia IPRESS_ONC.r")
Pob_Map_Ubi = Pob_Map_Ubi %>% filter(Anho==Anho_Map)
xl = data.table(Pob_Map_Ubi)[, .(pbl_RE=sum(Apl_P.SIS_RE)), keyby=.(mcrrgn_m,NOMBDEP)]

eq = IPRESS_ONC_30[INSTITUCION=="MINSA", .(Eqp=sum(Equipos_RE)), keyby=Departamento]

ww = merge(xl, eq, by.x="NOMBDEP", by.y="Departamento", all.x=T)

write.xlsx(ww, "Pbl 2030.xlsx")

sum(ww$pbl_RE)
