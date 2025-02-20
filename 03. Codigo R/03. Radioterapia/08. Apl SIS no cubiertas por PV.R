
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
#########################################################################
# CAMBIO DE CARPETA:
Carp = "01. Porc ENDES\\"
#########################################################################

Anho = 2023
source("03. Codigo R\\03. Radioterapia\\07. MINSA - Apl SIS no Cub PV.r")
p_RE.Ext_MINSA_2023 = p_RE.Ext_MINSA
p_RE.Ext_MINSA_LC_2023 = p_RE.Ext_MINSA_LC

Anho = 2030
source("03. Codigo R\\03. Radioterapia\\07. MINSA - Apl SIS no Cub PV.r")
p_RE.Ext_MINSA_2030 = p_RE.Ext_MINSA
p_RE.Ext_MINSA_LC_2030 = p_RE.Ext_MINSA_LC

Anho = 2035
source("03. Codigo R\\03. Radioterapia\\07. MINSA - Apl SIS no Cub PV.r")
p_RE.Ext_MINSA_2035 = p_RE.Ext_MINSA
p_RE.Ext_MINSA_LC_2035 = p_RE.Ext_MINSA_LC


pdf(paste0("09. Modelo RT\\",Carp,"01. ALCANCE DE IPRESS y LMyC Ext.pdf"), width=20, height=10)
print(ggarrange(p_RE.Ext_MINSA_2023, p_RE.Ext_MINSA_2030, p_RE.Ext_MINSA_2035, nrow=1, common.legend=TRUE, legend="bottom"))
print(ggarrange(p_RE.Ext_MINSA_LC_2023, p_RE.Ext_MINSA_LC_2030, p_RE.Ext_MINSA_LC_2035, nrow=1, common.legend=TRUE, legend="bottom"))
dev.off() 

