
library(dplyr)
library(data.table)
library(stringr)
library(sf)

rm(list=ls())
setwd("C:\\Users\\Prospectiva01\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")


Ubig = read.csv("02. BD\\03. TB_UBIGEOS.csv", sep=",", encoding = "UTF-8")
Ubig = select(Ubig, c("UBIGEO", "departamento", "provincia")) # 1893
glimpse(Ubig)

Cong_Endes = read.csv("02. BD\\04. Identifica conglomerado.csv", sep=";")
Cong_Endes = distinct(select(Cong_Endes, c("HHID", "UBIGEO"))) # 37350
glimpse(Cong_Endes)


Cong_DPD = merge(Cong_Endes, Ubig, all.x=T) # 37350
Cong_DPD = select(Cong_DPD, -"UBIGEO") # Obtienes el HHID relacionado con el Dpto y Prov
vis_miss(Cong_DPD)



######
Endes = read.csv("02. BD\\05. ENDES 2022.csv", sep=";")
Endes = data.table(Endes)
Endes = Endes %>% 
  select(c("HHID", "QSRESULT", "QS26", "QS27")) %>% # 34301
  mutate(Class_QS27=case_when(
    is.na(QS27) | QS27==" "~"SIS",
    QS27%in%c("A")~"SIS",
    QS27%in%c("B")~"EsSalud",
    TRUE~"Privado"))
glimpse(Endes)

# Solo el 91.85% de las encuestas esta clasificada como completa
sprintf("%.2f%%",xtabs(~QSRESULT, Endes, addNA=T)*100/nrow(Endes)) 


xtabs(~QSRESULT+QS26, Endes, addNA=T)
xtabs(~QS26+QS27, Endes, addNA=T)


# El 85.11% de la poblacion que ha respondido la encuesta como completa indica que tiene un seguro de salud
# el 14.89% indique que no tiene seguro de salud
sprintf("%.2f%%",xtabs(~QS26, Endes %>% filter(QSRESULT==1), addNA=T)*100/nrow(Endes %>% filter(QSRESULT==1))) 


# Para las personas que respondieron completo la encuesta 
# el 79.95% son de SIS (SIS: 65.06% y No Seguro: 14.89%); 17.93% son de EsSalud y el 2.12% es privado
sprintf("%.2f%%",xtabs(~Class_QS27, Endes %>% filter(QSRESULT==1), addNA=T)*100/nrow(Endes %>% filter(QSRESULT==1))) 


#####################################
Endes = Endes %>% filter(QSRESULT==1)



Endes_DPD = merge(Endes, Cong_DPD, by="HHID", all.x=T) # 34301
unique(Endes_DPD$Class_QS27)
table(Endes_DPD$Class_QS27)/nrow(Endes_DPD) 
# ANHO 2022: 
#     No tiene: 0.1488875
#     SIS: 0.6506173
#     EsSalud: 0.1792935
#     OTROS: 0.02120164 (2.12%)



resumen = Endes_DPD[ , .(Seg_SIS=sum(Class_QS27=="SIS")/.N,
                         Seg_ESSD=sum(Class_QS27=="EsSalud")/.N), keyby=.(departamento, provincia)]


Est.Nacional_SIS = sum((Endes$Class_QS27=="SIS"))/nrow(Endes)
Est.Nacional_ESSD= sum((Endes$Class_QS27=="EsSalud"))/nrow(Endes)


resumen = merge(Ubig %>% select(-UBIGEO) %>% distinct(), resumen, by=c("departamento", "provincia"), all.x=T) %>% 
  mutate(Seg_SIS=ifelse(is.na(Seg_SIS), Est.Nacional_SIS, Seg_SIS),
         Seg_ESSD=ifelse(is.na(Seg_ESSD), Est.Nacional_ESSD, Seg_ESSD))

write.xlsx(resumen, "04. BD - Resultado usando R\\03. Porcentaje Asegurados SIS Y ESSALUD.xlsx")

