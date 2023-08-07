# A partir de los datos de avra genera 3 tablas con los datos que casan con
# 1 vivienda, los que casan con mas de 1 y los que no casan


#BORRAR todos los ELEMENTOS en memoria
rm(list = ls())

# paquetes y directorio de trabajo ----
#instala y carga los paquetes necesarios
paquetes_necesarios = c("readxl","RPostgres","sf","dplyr","writexl") # c( "ggplot2","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)


#########################################################################

# source("Funciones.R")
# avra2018_2022 <- carga_datos_entrada()
# save.image(file = "datos.Rdata")

load("datos.Rdata")
source("Funciones.R")

#########################################################################
# ejecutar un año
avra_2022 <- avra2018_2022 %>% 
  filter(as.character(format(avra2018_2022$FECHA_DEVENGO, "%Y")) == 2022)

# Añade la información del catastro a cada vivienda del registro de fianzas
avra_catastro_2022 <- añade_campos_catastro(avra_2022)

# Salva en el directorio datos_output la información en y xlsx
salva_tablas_avra_catastro(avra_catastro_2022)
# Salva en el directorio datos_output la información en .Rdata
save(avra_catastro_2022, file = "./datos_output/avra_catastro_2022.RData")

# Prepara para el análisis, genera campos calculados, FILTRA los registros
# válidos para el análisis, añade campos de POTA y secciones censales y crea factores
datos_para_analisis_2022 <- preparacion_datos(avra_catastro_2022)

# Salva en el dirctorio datos_output la información
save(datos_para_analisis_2022, file = "./datos_output/datos_para_analisis_2022.RData")
write_xlsx(datos_para_analisis_2022[[1]], 
           "./datos_output/_8_datos_para_analisis_2022.xlsx")
write_xlsx(datos_para_analisis_2022[[2]], 
           "./datos_output/_8b_resumen_del_filtrado.xlsx")

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
# # ejecutar varios años en una lista
# # la cosa se complica al intentar guardar los excel mejor hacer 1 a 1
# # probar con {{x}}
# avra2018_2022 <- avra2018_2022 %>% 
#     filter(as.character(format(avra2018_2022$FECHA_DEVENGO, "%Y")) == 2019) %>% 
#     filter(CODIGO_INE == "41102")
# 
# lista_tablas_avra <- split(avra2018_2022,
#                      paste0("avra_", as.character(format(avra2018_2022$FECHA_DEVENGO, "%Y"))))
# 
# lista_tablas_avra <- lista_tablas_avra[1]
# #lo siguiente es lo que genera nombres de tablas raros
# lista_avra_catastro <- lapply(lista_tablas_avra, añade_campos_catastro)
# 
# lapply(lista_avra_catastro, salva_tablas_avra_catastro)






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
