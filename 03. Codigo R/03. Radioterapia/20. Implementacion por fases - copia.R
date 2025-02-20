

IPRESS_ONC[, Pct_Limite:=Equipos_RE*240*60] # Aplicaciones por Equipo
IPRESS_ONC$Pob_IPRESS_RT = 0


Medida_Usar = Medida_Usar[, IPRESS_ONC[, IPRESS_ABREV]]
Aux_Cal = data.frame(Medida_Usar)
Aux_Cal$Aux_IPRESS = NA

L.identf_HNHU = c("LIMA_LIMA_SAN JUAN DE LURIGANCHO", "LIMA_LIMA_EL AGUSTINO")
Relacion = data.table(Identf=L.identf_HNHU, Prox="HNHU", Medida_API=c(1703, 206))


Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), L.identf_HNHU), 
                  setdiff(colnames(Aux_Cal), "HNHU")]

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
    
    Poblc = Pob_Map_Ubi[Pob_Map_Ubi$Identf==Aux_row,]$Apl_P.SIS_RE
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

RT_Ext_MINSA = merge(Pob_Map_Ubi, Relacion, by="Identf", all.x=TRUE)
RT_Ext_MINSA = merge(RT_Ext_MINSA, 
                     rename(select(IPRESS_ONC, -c("IPRESS_LAT.LON", "Identf", "Pob_IPRESS_RT", "Pct_Limite", "UBIGEO")), 
                            Prox=IPRESS_ABREV), by="Prox", all.x=TRUE)

Aux_TB_RE = data.table(RT_Ext_MINSA)[, .(Distritos=.N,
                                         Apl_Sis_Onc_RE=sum(Apl_P.SIS_RE),
                                         Equip_RE=unique(Equipos_RE)), keyby=.(Prox, Categoria)]
Aux_TB_RE[, Apl_Posibles_RE:=Equip_RE*240*60]
Aux_TB_RE = Aux_TB_RE[, c("Prox", "Categoria", "Equip_RE", "Apl_Posibles_RE", "Distritos", "Apl_Sis_Onc_RE")]



Tb_01 = data.table(RT_Ext_MINSA)[is.na(Prox)==F, .(Distritos_At=.N,
                                                   Apl_Sis_Onc_RE_At=sum(Apl_P.SIS_RE)), keyby=.(mcrrgn_m)]
Tb_02 = data.table(RT_Ext_MINSA)[, .(Distritos=.N,
                                     Apl_Sis_Onc_RE=sum(Apl_P.SIS_RE)), keyby=.(mcrrgn_m)]
Tb_merge = merge(Tb_02, Tb_01, all.x=T)
Tb_merge[, Proc_pbl_At:=Apl_Sis_Onc_RE_At/Apl_Sis_Onc_RE]



####### MAPA
Col_R.ES = c("HACVP"="#3A60CA", "HNERM"="#2D7CA3", "HNCASE"="#36ACC8", 
             "INEN"="#7CAE00", "IREN.CENTRO"="#9590FF", "IREN.NORTE"="#35A2FF", "HT"="#EA8331",
             "IREN.SUR"="#FF62BC", "HG"="#D89000",
             "HBT"="#4d8e6f", "HRL"="#ffcc00", "HRL.FSAI"="#339900",
             "HMA"="#EA8331", "HNHU"="#F8766D", "HNCH"="#00BFC4")


p_RE.Ext_MINSA = ggplot(RT_Ext_MINSA) + geom_sf(aes(fill=Prox)) + 
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_R.ES, name="IPRESS") +
  labs(title="IPRESS MINSA CON ATENCION EN RADIOTERAPIA EXTERNA", x="", 
       y="", fill="IPRESS - RE") + theme_minimal()

LC_MINSA = RT_Ext_MINSA %>% filter(NOMBPROV%in%c("LIMA", "CALLAO"))
p_RE.Ext_MINSA_LC = ggplot(LC_MINSA) + geom_sf(aes(fill=Prox)) + 
  geom_text_repel(data=LC_MINSA, aes(x=centroid_LON, y=centroid_LAT, label=NOMBDIS),
                  size=2, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                  segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0) %>% filter(Provincia%in%c("LIMA", "CALLAO")), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_R.ES, name="IPRESS") +
  labs(title="IPRESS MINSA CON ATENCION EN RADIOTERAPIA EXTERNA", subtitle="LIMA METROPOLITANA Y CALLAO",x="", 
       y="", fill="IPRESS - RE") + theme_minimal()

p_RE.Ext_MINSA_LC2 = ggplot(LC_MINSA) + geom_sf(aes(fill=Prox)) + 
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0) %>% filter(Provincia%in%c("LIMA", "CALLAO")), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=7, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_R.ES, name="IPRESS") +
  labs(title="IPRESS MINSA CON ATENCION EN RADIOTERAPIA EXTERNA", subtitle="LIMA METROPOLITANA Y CALLAO",x="", 
       y="", fill="IPRESS - RE") + theme_minimal()


pdf(paste0("09. Modelo RT\\", fase, "01. ALCANCE DE IPRESS MINSA RT Ext - Est Recalculado.pdf"), width=20, height=10)
map_gg.IPRESS = ggarrange(p_RE.Ext_MINSA, p_RE.Ext_MINSA_LC, p_RE.Ext_MINSA_LC2, nrow=1)
print(map_gg.IPRESS)
dev.off()

write.xlsx(list(Aux_TB_RE=Aux_TB_RE, Tb_merge=Tb_merge), paste0("09. Modelo RT\\", fase,"01. Estimacion del Alcance MINSA - Est Recalculado.xlsx"))

