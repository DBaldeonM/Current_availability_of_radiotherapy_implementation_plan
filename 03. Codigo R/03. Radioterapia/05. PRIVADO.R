
source("03. Codigo R\\02. Funciones y Get Data\\01. Llamado de BD - Radioterapia Anho y Carpeta ENDES.r")

if (Anho==2023) {
  IPRESS_ONC = IPRESS_ONC_23
  Medida_Usar = Time_23
  Pob_Map_Ubi = Pob_Map_Ubi %>% filter(Anho==2023)
  
}else{
  if(Anho==2030) {
    IPRESS_ONC = IPRESS_ONC_30
    Medida_Usar = Time_30
    Pob_Map_Ubi = Pob_Map_Ubi %>% filter(Anho==2030)
    
  }else{
    if (Anho==2035) {
      IPRESS_ONC = IPRESS_ONC_35
      Medida_Usar = Time_35
      Pob_Map_Ubi = Pob_Map_Ubi %>% filter(Anho==2035)
      
    }
  }
}

IPRESS_ONC[, Pct_Limite:=Equipos_RE*240*60]
IPRESS_ONC$Pob_IPRESS_RT = 0

################## ESSALUD
IPRESS_ONC = IPRESS_ONC[INSTITUCION%in%c("PRIVADO"),]
##################

Medida_Usar$IPRESS_AUX = NA
Medida_Usar = Medida_Usar[, c(IPRESS_ONC[, IPRESS_ABREV], "IPRESS_AUX")]

Aux_Cal = data.frame(Medida_Usar)
Relacion = data.table()
for (i in 1:nrow(Aux_Cal)) {
  min_AC = min(Aux_Cal, na.rm=T)
  if(min_AC==Inf){
    break
  }else{
    Aux_row = rowSums(Aux_Cal==min_AC, na.rm = TRUE)
    Aux_row = names(Aux_row[Aux_row!=0])[1] # Identificamos la fila con el menor valor
    Aux_col = colSums(Aux_Cal[Aux_row,]==min(Aux_Cal[Aux_row,], na.rm=T), na.rm = TRUE)
    Aux_col = names(Aux_col[Aux_col!=0])[1] # Identificamos la columna con el menor valor
    
    Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_AC))
    
    Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Aux_row), ] # Retiramos la fila
    
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Apl_P.PV_RE
    IPRESS_ONC[IPRESS_ABREV==Aux_col, Pob_IPRESS_RT:=Pob_IPRESS_RT+Poblc]
    
    Aux_Cal = Aux_Cal[, setdiff(colnames(Aux_Cal), IPRESS_ONC[Pob_IPRESS_RT>Pct_Limite, IPRESS_ABREV])]
    
    # if(Aux_col=="IREN.SUR"){
    #   print(min_AC)
    #   print(Poblc)
    #   print(Aux_row)
    #   print(IPRESS_ONC[, c("IPRESS_ABREV", "Pct_Limite", "Pob_IPRESS_RT")])
    # }
  }
}

RT_Ext_PRIVADO = merge(Pob_Map_Ubi, Relacion, by="Identf", all.x=TRUE)
RT_Ext_PRIVADO = merge(RT_Ext_PRIVADO, 
                       rename(select(IPRESS_ONC, c("IPRESS_ABREV", "INSTITUCION", "Categoria", "Equipos_RE")), 
                              Prox=IPRESS_ABREV), by="Prox", all.x=TRUE)

Aux_TB_RE = data.table(RT_Ext_PRIVADO)[, .(Distritos=.N,
                                         Apl_Sis_Onc_RE=sum(Apl_P.PV_RE),
                                         Equip_RE=unique(Equipos_RE)), keyby=.(Prox, Categoria)]
Aux_TB_RE[, Apl_Posibles_RE:=Equip_RE*240*60]
Aux_TB_RE = Aux_TB_RE[, c("Prox", "Categoria", "Equip_RE", "Apl_Posibles_RE", "Distritos", "Apl_Sis_Onc_RE")]



Tb_01 = data.table(RT_Ext_PRIVADO)[is.na(Prox)==F, .(Distritos_At=.N,
                                                   Apl_Sis_Onc_RE_At=sum(Apl_P.PV_RE)), keyby=.(mcrrgn_)]
Tb_02 = data.table(RT_Ext_PRIVADO)[, .(Distritos=.N,
                                     Apl_Sis_Onc_RE=sum(Apl_P.PV_RE)), keyby=.(mcrrgn_)]
Tb_merge = merge(Tb_02, Tb_01, all.x=T)
Tb_merge[, Proc_pbl_At:=Apl_Sis_Onc_RE_At/Apl_Sis_Onc_RE]



####### MAPA
Col_R.ES = c("HACVP"="#3A60CA", "HNERM"="#2D7CA3", "HNCASE"="#36ACC8", 
             "INEN"="#7CAE00", "IREN.CENTRO"="#9590FF", "IREN.NORTE"="#35A2FF", 
             "IREN.SUR"="#FF62BC", "HG"="#D89000",
             "HBT"="#4d8e6f", "HRL"="#ffcc00", "HRL.FSAI"="#339900",
             "Aliada"="#FA62DB", "Auna"="#EA8331", "CACH"="#FF6A98", "CMSJ"="#39B600", "COR"="#F8766D", 
             "CRO"="#00C3C8", "CRP"="#00BB4E", "CSG"="#00B496", "CSP"="#C09B00", 
             "CTDO"="#00BF7D", "Oncorad"="#A3A500","Oncosur"="#837DFF", "CSPA"="#DDB509", "Oncosalud"="#BE7E38")


p_RE.Ext_PRIVADO = ggplot(RT_Ext_PRIVADO) + geom_sf(aes(fill=Prox)) + 
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values = Col_R.ES, name = "Institution") + 
  labs(x = "", y = "", fill = "Institution") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(color = "black"),  # Eje X en negro
        axis.text.y = element_text(color = "black"),  # Eje Y en negro
        legend.title = element_text(face = "bold", size = 12),   # Título de la leyenda en negrita
        panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) +   
  annotate("text", x = -80, y = 0,label = "C", size = 13, family = "Calibri") 

LC_PRIVADO = RT_Ext_PRIVADO %>% filter(NOMBPROV%in%c("LIMA", "CALLAO"))
p_RE.Ext_PRIVADO_LC = ggplot(LC_PRIVADO) + geom_sf(aes(fill=Prox)) + 
  geom_text_repel(data=LC_PRIVADO, aes(x=centroid_LON, y=centroid_LAT, label=NOMBDIS),
                  size=2, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                  segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0) %>% filter(Provincia%in%c("LIMA", "CALLAO")), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_R.ES, name="IPRESS") + 
  labs(title="IPRESS PRIVADO CON ATENCION EN RADIOTERAPIA EXTERNA", subtitle=paste0("LIMA METROPOLITANA Y CALLAO ", Anho, ": Equipos activos"), x="", 
       y="", fill="IPRESS - RE") + theme_minimal()

write.xlsx(list(Aux_TB_RE=Aux_TB_RE, Tb_merge=Tb_merge), 
           paste0("09. Modelo RT\\",Carp,"Anho ",Anho,"\\03. PRIVADO RE.xlsx"))
