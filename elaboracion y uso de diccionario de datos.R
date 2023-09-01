# Codigo usado para crear las tablas de diccionario con los nombres largos
# de los campos

rm(list =ls())
load(file = "./datos_output/datos_para_analisis_2022.RData")

datos <- datos_para_analisis_2022 ; rm(datos_para_analisis_2022)
avra_datos_originales <- datos[["originales"]]
avra_catastro <- datos[["Fianzas_viviendas"]]

# Creo un dataframe con una columna que contenga los nombres de los campos
# y otra columna vacía para añadir las etiquetas

campos_avra <- data.frame(
  campo = names(avra_datos_originales),
  etiqueta = character(length(names(avra_datos_originales)))
)

campos_avra_catastro <- data.frame(
  campo = names(avra_catastro),
  etiqueta = character(length(names(avra_catastro)))
)

camino <- paste0("./datos_output/")

write_xlsx(campos_avra, paste0(camino,"campos_avra_a_completar.xlsx"))
write_xlsx(campos_avra_catastro, paste0(camino,"campos_avra_catastro_a_completar.xlsx"))

# En excel completo la información de los campos de cada tabla.
# Añado los campos etiqueta, origen y comentarios
# El campo origen es el que puedo usar en los gráficos 
# Los ficheros con la información completada se deben salvar con los nombres
# campos_avra_catastro_completados.xlsx y campos_avra_completados.xlsx








library("readxl")
#campos <- read_excel("./datos_output/campos_avra_completados.xlsx")     
diccionario_campos <- read_excel("./datos_output/campos_avra_catastro_completados.xlsx")                     


# Función que devuelve la etiqueta del campo a partir de su nombre
Etiqueta <- function(campo_input) {
  fila <- subset(diccionario_campos, campo == campo_input)
  
  if (nrow(fila) == 0) {
    mensaje <- paste("El campo", campo_input, "no se encontró")
    return(mensaje)
  }
  
  valor_salida <- fila$etiqueta
  return(valor_salida)
}


Etiqueta("num_habitaciones")


# Gráfico de barras con etiquetas de altura
variable <- "sexo_arrendatario"

ggplot(datos, aes(x =!!sym(variable))) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            color = "cornsilk4",
            vjust = "inward",# hjust = "inward",
            family = "sans", size = 9/.pt) +
  labs(x = "", 
       y = "Frecuencia",
       title = Etiqueta(variable)) +
  theme_minimal()


