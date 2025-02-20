
############################################################################ 
################################# FUNCIONES################################# 
############################################################################

Mas_Cerca_IPRESS = function(vec){
  if(sum(!is.na(vec))==0){
    return(NA)
  }else{
    return(destination_ID[which.min(vec)])
  }
}


### tranforma de Numero a tiempo D - HH:MM
tiempo_HM = function(Med_tiempo){
  if(is.na(Med_tiempo)==T){
    res = NA
  }else{
    Med_dias = Med_tiempo %/% 86400
    Med_horas = (Med_tiempo - Med_dias*24*3600) %/% 3600
    Med_min = (Med_tiempo - Med_dias*24*3600 - Med_horas*3600)%/% 60
    
    Med_min_0 = ifelse(Med_min>=10, paste0(Med_min), paste0("0", Med_min))
    Med_horas_0 = ifelse(Med_horas>=10, paste0(Med_horas), paste0("0", Med_horas))
    
    if(Med_dias==0){
      res = paste0(Med_horas_0,"h ",Med_min_0, "m")
    }else{
      res = paste0(Med_dias,"d - ",Med_horas_0,"h ",Med_min_0, "m")
    }
  }
  return(res)
}


tiempo_MS = function(Med_tiempo){
  Med_tiempo = round(Med_tiempo, 0)
  if(is.na(Med_tiempo)==T){
    res = NA
  }else{
    Med_min = Med_tiempo%/% 60
    Med_s = Med_tiempo - Med_min*60
    
    Med_min_0 = ifelse(Med_min>=10, paste0(Med_min), paste0("0", Med_min))
    Med_s_0 = ifelse(Med_s>=10, paste0(Med_s), paste0("0", Med_s))
    
    res = paste0(Med_min_0, "m ", Med_s_0, "s")
  }
  return(res)
}


Modelo_01_DIRIS = function(Medida_Usar){
  Aux_Cal = data.frame(Medida_Usar)
  Relacion = data.table()
  for (i in 1:nrow(Aux_Cal)) {
    min_distancia = min(Aux_Cal, na.rm=T)
    Aux_row = rowSums(Aux_Cal==min_distancia, na.rm = TRUE)
    Aux_row = names(Aux_row[Aux_row!=0])[1] # Identificamos la fila con el menor valor
    Aux_col = colSums(Aux_Cal[Aux_row,]==min(Aux_Cal[Aux_row,], na.rm=T), na.rm = TRUE)
    Aux_col = names(Aux_col[Aux_col!=0])[1] # Identificamos la columna con el menor valor
    
    if (is.na(Aux_row)==F){
      Relacion = rbind(Relacion, data.frame(Identf=Aux_row, Prox=Aux_col, Medida_API=min_distancia))
      Aux_Cal = Aux_Cal[setdiff(rownames(Aux_Cal), Aux_row), ] # Retiramos la fila
    }
  }
  return(Relacion)
}

##########################
Sum_Dig = function(numero) {
  digitos = strsplit(as.character(numero), split = "")[[1]]
  return(sum(as.numeric(digitos)))
}

agregarCeros = function(numero, cant) {
  return(sprintf(paste0("%0", cant,"d"), numero))
}

Separar = function(numero) {
  numero = numero[2]
  digitos = strsplit(as.character(numero), split = "")[[1]]
  return(as.numeric(digitos))
}

