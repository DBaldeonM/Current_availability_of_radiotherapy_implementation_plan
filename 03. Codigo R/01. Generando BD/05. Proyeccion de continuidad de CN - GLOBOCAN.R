
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)


setwd("C:\\Users\\Asus\\Dropbox\\01. Inestigaciones RON\\02. Tratamiento de Radioterapia")

data = read.xlsx("02. BD\\06. Proyeccion Nuevos Casos.xlsx", sheet="CN")
data$Total.CN = NULL
data = data %>%
  pivot_longer(cols = -c(Anho), 
               names_to = "Tipo_cancer", 
               values_to = "Casos_nuevos")
  


predictions <- data.frame()
cancer_types <- unique(data$Tipo_cancer)

for (cancer in cancer_types) {
  data_cancer <- data[data$Tipo_cancer == cancer, ]
  model <- lm(log(Casos_nuevos) ~ Anho, data = data_cancer)
  
  new_data <- data.frame(Anho = 2020:2050)
  new_data$Casos_nuevos_pred <- round(exp(predict(model, new_data)), 0)
  new_data$Tipo_cancer <- cancer
  
  predictions <- rbind(predictions, new_data)
}


data <- data %>%
  mutate(Tipo_cancer = recode(Tipo_cancer,
                              "Cabeza.y.Cuello" = "Head and Neck",
                              "Colorectal" = "Colorectal",
                              "Estomago" = "Gastric",
                              "Mama" = "Breast",
                              "Pulmon" = "Lung",
                              "Cervical" = "Cervix",
                              "Cuerpo.Uterino" = "Endometrium",
                              "Linfoma.No.Hodgkin" = "Non-Hodgkin",
                              "Prostata" = "Prostate",
                              "Tiroides" = "Thyroid"))

predictions <- predictions %>%
  mutate(Tipo_cancer = recode(Tipo_cancer,
                              "Cabeza.y.Cuello" = "Head and Neck",
                              "Colorectal" = "Colorectal",
                              "Estomago" = "Gastric",
                              "Mama" = "Breast",
                              "Pulmon" = "Lung",
                              "Cervical" = "Cervix",
                              "Cuerpo.Uterino" = "Endometrium",
                              "Linfoma.No.Hodgkin" = "Non-Hodgkin",
                              "Prostata" = "Prostate",
                              "Tiroides" = "Thyroid"))

plot <- ggplot(data %>% filter(Anho != 2023 & Anho <= 2040), aes(x = Anho, y = Casos_nuevos, color = Tipo_cancer)) +
  geom_point(aes(color = Tipo_cancer), size = 2) +  # Puntos de datos originales
  geom_line(data = predictions %>% filter(Anho <= 2040), aes(x = Anho, y = Casos_nuevos_pred, color = Tipo_cancer), size = 1) +  # Líneas de predicción (20% más gruesas)
  labs(x = "Year",
       y = "New Cases") +
  scale_x_continuous(breaks = seq(min(data$Anho), max(data$Anho), by = 5)) +  # Eje x cada 5 años
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 9.5),  # Aumentar el tamaño del texto de la leyenda en 10%
        plot.title = element_blank(),  # Eliminar título del gráfico
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "black"),  # Etiquetas del eje x en negro
        axis.text.y = element_text(color = "black"),
        panel.border = element_rect(color = "#DFDFDF", fill = NA, size = 1)) 

# Guardar el gráfico como PDF
ggsave("001 Grafico_proyeccion_casos.pdf", plot = plot, width = 12, height = 6, device = 'pdf')



predictions_T = predictions %>%
  pivot_wider(names_from = Tipo_cancer, 
              values_from = Casos_nuevos_pred)
  
write.xlsx(predictions_T, "04. BD - Resultado usando R\\04. Proyeccion de continuidad CN - Reg Exp.xlsx")
