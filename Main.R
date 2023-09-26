# A partir de los datos de avra genera 3 tablas con los datos que casan con
# 1 vivienda, los que casan con mas de 1 y los que no casan

# Quitamos la notación científica
options(scipen = 999) # Para que no use notación científica
options(digits = 2)  # Para que muestre solo 2 decimales

#BORRAR todos los ELEMENTOS en memoria
rm(list = ls())

# paquetes y directorio de trabajo ----
#instala y carga los paquetes necesarios
# paquetes_necesarios = c("readxl","RPostgres","sf","tidyverse","writexl","glue") # c( "ggplot2","classInt") 
# for (paq in paquetes_necesarios){
#   if (!(paq %in% rownames(installed.packages()))){
#     install.packages(paq, dependencies = T)}
#   library(paq, character.only = T)
# }
# rm(paq, paquetes_necesarios)

# alternativa
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl,RPostgres,sf,tidyverse,writexl,glue,here)

#########################################################################

# source("Funciones.R")
# avra2018_2022 <- carga_datos_entrada()
# save.image(file = "./datos_output/datos_avra.Rdata")

load("./datos_output/datos_avra.Rdata")
source("Funciones.R")

#########################################################################
# ejecutar un año
# avra_2022 <- avra2018_2022 %>%
#   filter(as.character(format(avra2018_2022$FECHA_DEVENGO, "%Y")) == "2022")
# #
# # # Añade la información del catastro a cada vivienda del registro de fianzas
#  avra_catastro_2022 <- añade_campos_catastro(avra_2022)
# 
# # Salva en el directorio datos_output la información en y xlsx
# salva_tablas_avra_catastro(avra_catastro_2022)
# # Salva en el directorio datos_output la información en .Rdata
# save(avra_catastro_2022, file = "./datos_output/avra_catastro_2022.RData")
# 
# # Prepara para el análisis, solo con la tabla de vivendas del registro con
# # los datos del catastro conectados, genera campos calculados, FILTRA los registros
# # válidos para el análisis, añade campos de POTA y secciones censales y crea factores
#
# load("./datos_output/avra_catastro_2022.RData")
# datos_para_analisis_2022 <- preparacion_datos(avra_catastro_2022)
# 
# # Salva en el dirctorio datos_output la información
# # Salva en el dirctorio datos_output la información
# save(datos_para_analisis_2022,
#      file = "./datos_output/datos_para_analisis_2022.RData")
# write_xlsx(datos_para_analisis_2022[[1]],
#            glue("./datos_output/avra_catastro_2022_8_datos_para_analisis.xlsx"))
# write_xlsx(datos_para_analisis_2022[[2]],
#            glue("./datos_output/avra_catastro_2022_8b_resumen_del_filtrado.xlsx"))



#load("avra_catastro_2018.Rdata")
# #########################################################################

# Los ficheros .RData generados en el proceso completo son:
# 
#  datos.Rdata .................. Contiene los datos de excel crudos,
#                                 sin ningún proceso
#  
#  avra_catastro_xxxx.RData ..... Contiene los datos conectados con la información
#                                 de catastro. Genera varias tablas: con los datos
#                                 originales, con los datos conectados y segregando
#                                 según el número de enlaces con catastro. Tb genera
#                                 dos tablas con información de resumen de enlaces
#                                 
#  datos_para_analisis_xxxx.RData Contiene solo la tabla de viviendas que enlazan
#                                 con una vivienda en catastro. Le añade campos,
#                                 elimina registros no válidos. También contiene
#                                 una tabla resumen con información del número de
#                                 registros eliminados.

 

 
# #########################################################################
## Función que hace el proceso completo 
# Parte de los datos ya leidos y genera el conjunto de tablas

proceso_completo <- function(anyo_sel) {
avra_anyo <- avra2018_2022 %>% 
  filter(as.character(format(avra2018_2022$FECHA_DEVENGO, "%Y")) == anyo_sel)

# Añade la información del catastro a cada vivienda del registro de fianzas
avra_catastro_anyo <- añade_campos_catastro(avra_anyo)

# Salva en el directorio datos_output la información en y xlsx
salva_tablas_avra_catastro_alt(avra_catastro_anyo, anyo_sel)

# Salva en el directorio datos_output la información en .Rdata
nombre_df <- glue("avra_catastro_{anyo_sel}")
assign(nombre_df, avra_catastro_anyo )
save(list = nombre_df, file = glue("./datos_output/avra_catastro_{anyo_sel}.RData"))

# Prepara para el análisis, solo con la tabla de vivendas del registro con
# los datos del catastro conectados, genera campos calculados, FILTRA los registros
# válidos para el análisis, añade campos de POTA y secciones censales y crea factores
datos_para_analisis_anyo <- preparacion_datos(avra_catastro_anyo)

# Salva en el directorio datos_output la información
nombre_df <- glue("datos_para_analisis_{anyo_sel}")
assign(nombre_df, datos_para_analisis_anyo )
save(list = nombre_df, file = glue("./datos_output/datos_para_analisis_{anyo_sel}.RData"))

write_xlsx(datos_para_analisis_anyo[[1]], 
            glue("./datos_output/avra_catastro_{anyo_sel}_8_datos_para_analisis.xlsx"))
write_xlsx(datos_para_analisis_anyo[[2]], 
           glue("./datos_output/avra_catastro_{anyo_sel}_8b_resumen_del_filtrado.xlsx"))


#Lee los dataframes construidos
# load(file = glue("./datos_output/avra_catastro_{anyo_sel}.RData"))
# load(file = glue("./datos_output/datos_para_analisis_{anyo_sel}.RData"))
}

################################################################################
# Ejecutar un año elegido en una variable
anyo <- "2022"
proceso_completo(anyo)


################################################################################
# # ejecutar varios años en una lista
for (anyo_sel in seq(2018,2022, 1)){  #c(2017,2018)
 proceso_completo (anyo_sel)
 #print(anyo_sel)
}











#>Hace el proceso completo salvo la primera parte de conexión de los datos con 
#>catastro

proceso_completo_auxiliar <- function(anyo_sel) {
  print(glue("Procesando año {anyo_sel}"))
  
  load(file = glue("./datos_output/avra_catastro_{anyo_sel}.RData"))
  
  # Prepara para el análisis, solo con la tabla de vivendas del registro con
  # los datos del catastro conectados, genera campos calculados, FILTRA los registros
  # válidos para el análisis, añade campos de POTA y secciones censales y crea factores
  avra_catastro_anyo <- get(glue("avra_catastro_{anyo_sel}"))
  datos_para_analisis_anyo <- preparacion_datos(avra_catastro_anyo)
  
  # Salva en el directorio datos_output la información
  nombre_df <- glue("datos_para_analisis_{anyo_sel}")
  assign(nombre_df, datos_para_analisis_anyo )
  save(list = nombre_df, file = glue("./datos_output/datos_para_analisis_{anyo_sel}.RData"))
  
  write_xlsx(datos_para_analisis_anyo[[1]], 
             glue("./datos_output/avra_catastro_{anyo_sel}_8_datos_para_analisis.xlsx"))
  write_xlsx(datos_para_analisis_anyo[[2]], 
             glue("./datos_output/avra_catastro_{anyo_sel}_8b_resumen_del_filtrado.xlsx"))
  
  
  #Lee los dataframes construidos
  # load(file = glue("./datos_output/avra_catastro_{anyo_sel}.RData"))
  # load(file = glue("./datos_output/datos_para_analisis_{anyo_sel}.RData"))
}

for (anyo_sel in seq(2018,2022, 1)){  #c(2017,2018)
  proceso_completo_auxiliar(anyo_sel)
  #print(anyo_sel)
}







































################################################################################
# # Recopila los datos de todos los años y genera un df datos_analisis
datos_analisis <- data.frame()
for (anyo_sel in seq(2018,2022, 1)){  #c(2017,2018)
  #browser()
  print(glue("Procesando año {anyo_sel}"))
  #file <- glue("./datos_output/datos_para_analisis_{anyo_sel}.RData")
  file <- here("datos_output",glue("datos_para_analisis_{anyo_sel}.RData"))
  load(file)
  eval(parse(text = glue("contenedor <- datos_para_analisis_{anyo_sel}")))
  aux <- contenedor[["datos"]] ;# print(nrow(aux))
  if (nrow(datos_analisis) == 0 ) {
    datos_analisis <- aux
  } else {
    datos_analisis <- bind_rows(datos_analisis,aux )
  }
}

datos_analisis <- datos_analisis %>% 
  mutate(anyo = lubridate::year(datos_analisis$fecha_devengo)) %>% 
  relocate(anyo, .before = 1)

save(datos_analisis, 
     file = "./datos_output/datos_para_analisis_todos.RData")

#load(file = "./datos_output/datos_para_analisis_todos.RData")


################################################################################
# # Elabora el documento Publicacion analisis exploratorio

library(rmarkdown)

# Ruta al archivo Rmd 
ruta_archivo_rmd <- "./Publicacion analisis exploratorio/Genera_informe_MsWord.Rmd"

# Ruta de salida para el documento generado
ruta_salida <- "./Publicacion analisis exploratorio/Informe Analisis Exploratorio/"

# Parámetros 
params <- list(anyoID = "2022")

# Renderiza el archivo Rmd con los parámetros
render(input = ruta_archivo_rmd, 
       output_format = "html_document", 
       output_file = ruta_salida, 
       params = params)


# Para hacer todos los informes

# Ruta al archivo Rmd 
ruta_archivo_rmd <- "./Publicacion analisis exploratorio/Genera_informe_MsWord.Rmd"
# Ruta de salida para el documento generado
ruta_salida <- "./Publicacion analisis exploratorio/Informe Analisis Exploratorio/"
for (anyo_sel in seq(2018,2022, 1)){ 
  # Parámetros 
  params <- list(anyoID = anyo_sel)
  
  # Renderiza el archivo Rmd con los parámetros
  render(input = ruta_archivo_rmd, 
         output_format = "html_document", 
         output_file = ruta_salida, 
         params = params)
}






# Nota a meter en algun sitio
# La superficie asignada a cada vivienda por el resumen de datos IECA Catastro
# se calcula, dentro del fichero tipo 14 de construcciones, sumando la superficie
# total del local de cada una de las construcciones que conforman cada vivienda.
# Este dato se toma del campo “84_stl” de la hoja 14.


# Las estadisticas referidas al alquiler de vivienda elaboradas por el Estado:
# Sistema estatal de referencia del precio del alquiler de vivienda (Ministerio) y
# Índice de Precios de la Vivienda en Alquiler (INE) son elaboradas a partir de la
# información suministrada por el Modelo 100 de hacienda. Por tanto solo incluye
# información sobre alquileres realizados por personas físicas. Además dichas 
# estadísticas filtran la información referida a alquileres entre familiares por lo
# que cabe esperar un número muy reducido de situaciones de precio de alquiler a
# precios por debajo del mercado.
# La estadística AVRA no tiene criterios para separar este tipo de situaciones.
