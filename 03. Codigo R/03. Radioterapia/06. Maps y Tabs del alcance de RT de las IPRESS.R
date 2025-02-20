
#install.packages("showtext")
library(showtext)
font_add("Calibri", regular = "calibri.ttf", bold = "calibrib.ttf")
showtext_auto()

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
setwd("C:\\Users\\Asus\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")
#########################################################################
# CAMBIO DE CARPETA:
Carp = "01. Porc ENDES\\"
Anho = 2023
#########################################################################
source("03. Codigo R\\03. Radioterapia\\03. MINSA.r")
source("03. Codigo R\\03. Radioterapia\\04. ESSALUD.r")
source("03. Codigo R\\03. Radioterapia\\05. PRIVADO.r")


# pdf(paste0("09. Modelo RT\\",Carp,"Anho ",Anho,"\\01. ALCANCE DE IPRESS RT Ext_______.pdf"), width=20, height=10)
# map_gg.IPRESS <- plot_grid(p_RE.Ext_MINSA, p_RE.Ext_ESSALUD, p_RE.Ext_PRIVADO, nrow = 1, align = "h", axis = "tb")
# print(map_gg.IPRESS)
# dev.off() 

pdf(paste0("09. Modelo RT\\",Carp,"Anho ",Anho,"\\01. ALCANCE DE IPRESS RT Ext.pdf"), width=20, height=10)
map_gg.IPRESS = ggarrange(p_RE.Ext_MINSA, p_RE.Ext_ESSALUD, p_RE.Ext_PRIVADO, nrow=1)
print(map_gg.IPRESS)
dev.off() 

pdf(paste0("09. Modelo RT\\",Carp,"Anho ",Anho,"\\02. ALCANCE DE IPRESS RT LMyC Ext.pdf"), width=20, height=10)
map_gg.IPRESS_LMyC = ggarrange(p_RE.Ext_MINSA_LC, p_RE.Ext_ESSALUD_LC, p_RE.Ext_PRIVADO_LC, nrow=1)
print(map_gg.IPRESS_LMyC)
dev.off()


















