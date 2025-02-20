

####### Llamar Bases de Datos
IPRESS_ONC = read.xlsx("02. BD\\07. IPRESS Oncologicas.xlsx", sheet="RTE_ALL")
IPRESS_ONC = data.table(IPRESS_ONC)
IPRESS_ONC = IPRESS_ONC[order(IPRESS_ABREV)]
IPRESS_ONC = IPRESS_ONC[!IPRES_ONC%in%c("NO", "Plan Fortalecimiento"), ]
IPRESS_ONC = select(IPRESS_ONC, -c("MINSA", "INEN", "IPRES_ONC", "ORD_CAT", 
                                   "Nombre.del.establecimiento.ORIGINAL", "NIVEL",
                                   "Acelerador.Lineal", "Unidad.de.Cobalto"))
IPRESS_ONC = IPRESS_ONC[INSTITUCION=="GOBIERNO REGIONAL", INSTITUCION:="MINSA"]



#### 2023
IPRESS_ONC_23 = read.xlsx("02. BD\\07. IPRESS Oncologicas.xlsx", sheet="RTE_ALL_2023")
IPRESS_ONC_23 = data.table(IPRESS_ONC_23)
IPRESS_ONC_23 = IPRESS_ONC_23[order(IPRESS_ABREV)]
IPRESS_ONC_23 = IPRESS_ONC_23[Equipos_RE>0, ]
IPRESS_ONC_23 = select(IPRESS_ONC_23, -c("MINSA", "INEN", "IPRES_ONC", "ORD_CAT", 
                                         "Nombre.del.establecimiento.ORIGINAL", "NIVEL",
                                         "Acelerador.Lineal", "Unidad.de.Cobalto"))
IPRESS_ONC_23 = IPRESS_ONC_23[INSTITUCION=="GOBIERNO REGIONAL", INSTITUCION:="MINSA"]




#### 2030
IPRESS_ONC_30 = read.xlsx("02. BD\\07. IPRESS Oncologicas.xlsx", sheet="RTE_ALL_2030")
IPRESS_ONC_30 = data.table(IPRESS_ONC_30)
IPRESS_ONC_30 = IPRESS_ONC_30[order(IPRESS_ABREV)]
IPRESS_ONC_30 = IPRESS_ONC_30[Equipos_RE>0, ]
IPRESS_ONC_30 = select(IPRESS_ONC_30, -c("MINSA", "INEN", "IPRES_ONC", "ORD_CAT", 
                                        "Nombre.del.establecimiento.ORIGINAL", "NIVEL",
                                        "Acelerador.Lineal", "Unidad.de.Cobalto"))
IPRESS_ONC_30 = IPRESS_ONC_30[INSTITUCION=="GOBIERNO REGIONAL", INSTITUCION:="MINSA"]



#### 2035
IPRESS_ONC_35 = read.xlsx("02. BD\\07. IPRESS Oncologicas.xlsx", sheet="RTE_ALL_2035")
IPRESS_ONC_35 = data.table(IPRESS_ONC_35)
IPRESS_ONC_35 = IPRESS_ONC_35[order(IPRESS_ABREV)]
IPRESS_ONC_35 = IPRESS_ONC_35[Equipos_RE>0, ]
IPRESS_ONC_35 = select(IPRESS_ONC_35, -c("MINSA", "INEN", "IPRES_ONC", "ORD_CAT", 
                                         "Nombre.del.establecimiento.ORIGINAL", "NIVEL",
                                         "Acelerador.Lineal", "Unidad.de.Cobalto"))
IPRESS_ONC_35 = IPRESS_ONC_35[INSTITUCION=="GOBIERNO REGIONAL", INSTITUCION:="MINSA"]




## Pob_Map_Ubi: Contiene de 1890 distritos del Peru
Pob_Map_Ubi = st_read(paste0("04. BD - Resultado usando R\\", Carp, "Pob_Map_Ubi_MULTIPOLYGON.shp"))
lookup = c(NOMBPROV="NOMBPRO", NOMBDIS="NOMBDIS", 
           centroid_LAT="cnt_LAT", centroid_LON="cnt_LON", 
           Apl_P.Onc_RE="A_Onc_R", Apl_P.SIS_RE="A_SIS_R",
           Apl_P.EsD_RE="A_EsD_R", Apl_P.PV_RE="A_PV_R",
           
           Apl_P.Onc_BQ="A_Onc_B", Apl_P.SIS_BQ="A_SIS_B",
           Apl_P.EsD_BQ="A_EsD_B", Apl_P.PV_BQ="A_PV_B",
           Aj.Ap.SIS_R="A_A_SIS")

Pob_Map_Ubi = rename(Pob_Map_Ubi, all_of(lookup))


origin_ID = Pob_Map_Ubi$Identf
destination_ID_23 = IPRESS_ONC_23$IPRESS_ABREV
destination_ID_30 = IPRESS_ONC_30$IPRESS_ABREV
destination_ID_35 = IPRESS_ONC_35$IPRESS_ABREV


Time = read.table("05. API Matrix Distance - RQ//Time_ALL_COMPLETADO_RQ.txt", encoding="UTF-8")



if(length(setdiff(rownames(Time), origin_ID))==0 & length(setdiff(origin_ID, rownames(Time)))==0){
  print("BIEN!!!!")
}else{
print("REVISAR!!!")
}




## AJUSTAMOS LOS RESULTADOS A LOS NECESITADOS
Time_23 = Time[, destination_ID_23]
Time_30 = Time[, destination_ID_30]
Time_35 = Time[, destination_ID_35]
# Distance = Distance[, destination_ID]
# Status = Status[, destination_ID]

rm(destination_ID_23, destination_ID_30, destination_ID_35, lookup, origin_ID)

