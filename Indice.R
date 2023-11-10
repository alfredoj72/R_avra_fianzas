# Índice

# Directorios:
# borrar- cosas que no me atrevo a borrar
# capas_in-  capas shp, este directorio no se usa en el codigo
# codigo obsoleto- codigo que no se usa en la version definitiva pero es válido
# datos_aux- capas shp y algunos ficheros que se usan en el analisis
# datos_input- ficheros originales de avra
# datos_output- ficheros con información de avra ya limpia, ficheros para BADEA
# html_aux- ficheros en formato html auxiliares para su uso en los popup de los mapas
# informes_sintesis- ficheros Rmd que generan los informes por barrios y municipios
# Out_informes-
# Publicacion analisis exploratorio

archivos_R <- list.files(path = ".", pattern = "\\.R$", full.names = TRUE)
archivos_R

# --------- DIRECTORIO FICHEROS ---------------
# Indice- este fichero
# Main- Principal
# Funciones- Funciones auxiliares usadas en Main y en los informes
# generaSalidaBadea_xx.R- Genera cubos de información para incorporar a BADEA
# pintar las tablas- probaturas inconclusas para pintar tablas bonitas
# mapas ggplot- probaturas de mapas con ggplot
# mapas tmap- probaturas de mapas con tmap
# mapas leaflet- probaturas de mapas con leaflet
# Mapas Calor- hacer mapas de calor a partir de la información de los testigos

# --------- DIRECTORIO Publicacion analisis exploratorio ---------------
# Genera_informe_MsWord- Genera informe en formato word, para ello usa los ficheros
# carga_datos_y_funciones.R, 00.1_completitud, 00.2_resumen_inicial y
# 00.3_analisis_univariante


# --------- DIRECTORIO CODIGO OBSOLETO ---------------
# Contiene código válido pero que no se usa en las salidas definitivas

# AnalisisExploratorio.R
# Contiene el análisis realizado sobre todas las variables. Es previo a la concrección
# de las variables a usar y a la concrección de las categorías que se van a usar

# AnalisisExploratorio_anterior.R
# Son restos de código de un análisis exploratorio que realicé previo al fichero
# AnalisisExploratio.R

# Comprueba testigos.R
# Contiene código que permite conocer cuantos registros relativos a situaciones 
# con 10 o más registros se obtendrían. Queda obsoleto por generaSalidaBadea_XX.R 

# elaboracion y uso de diccionario de datos.R
# Codigo usado para crear las tablas de diccionario con los nombres largos
# de los campos

# Estilos ggplot particulares.R
# Definición de varios estilos personalizados para emplear en salidas ggplot

# generaSalidaBadea_xx.R 
# versiones previas del fichero último generaSalidaBadea_04.R actualmente.

