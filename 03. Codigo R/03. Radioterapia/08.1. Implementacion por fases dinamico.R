
IPRESS_ONC[, Pct_Limite:=Equipos_RE*240*60]
IPRESS_ONC$Pob_IPRESS_RT = 0


################## MINSA
IPRESS_ONC = IPRESS_ONC[INSTITUCION%in%c("GOBIERNO REGIONAL", "MINSA"),]
##################
Time$IPRESS_AUX = NA
Medida_Usar = Time[, c(IPRESS_ONC[, IPRESS_ABREV], "IPRESS_AUX")]
Aux_Cal = data.frame(Medida_Usar)
Aux_Cal$Aux_IPRESS = NA

## AGREGAMOS LOS DISTRITOS AL HNHU
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
                     rename(select(IPRESS_ONC, -c("UBIGEO", "Pob_IPRESS_RT", "Pct_Limite", "COD_UNICO")), 
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
Col_R.ES = c("INEN"="#7CAE00", "HMA"="#EA8331", "HNCH"="#00BFC4", "HNHU"="#F8766D",
             "HRL.FSAI"="#339900", "HT"="#768E00", "HAP.CSR"="#08CE9A", "HRP"="#C77CFF",
             "IREN.CENTRO"="#9590FF", "HRI"="#BF825D",
             "HBT"="#4d8e6f", "HRL"="#ffcc00", "IREN.NORTE"="#35A2FF",
             "IREN.SUR"="#FF62BC", "HG"="#D89000", "HRHDE"="#C77CFF", "HALC"="#36ACC8", "HADC"="#00BF7D")


p_RE.Ext_MINSA = ggplot(RT_Ext_MINSA) + geom_sf(aes(fill=Prox)) + 
  geom_label_repel(data=IPRESS_ONC %>% filter(Equipos_RE>0), 
                   aes(x=LONGITUD, y=LATITUD, label=IPRESS_ABREV, fill=IPRESS_ABREV),
                   size=3.4, min.segment.length=0, arrow = arrow(length = unit(0.005, "npc")),
                   segment.inflect=T, segment.square=T, segment.size=0.4, max.overlaps=Inf) +
  scale_fill_manual(values=Col_R.ES, name="IPRESS") + 
  labs(title="IPRESS MINSA - RADIOTERAPIA EXTERNA", subtitle=paste0("Equipos activos - ", Anho),
       x=paste0("[",k,"] - ", ac), y="", fill="IPRESS - RE") + theme_minimal() + theme(legend.position='none')



