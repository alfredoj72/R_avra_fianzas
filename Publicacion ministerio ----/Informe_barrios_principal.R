# Quitamos la notación científica
options(scipen = 999) # Para que no use notación científica
options(digits = 2)  # Para que muestre solo 2 decimales

# BORRAR todos los ELEMENTOS en memoria
rm(list = ls())
 
# carga paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, flextable,here , sf, readxl,sf, glue, RColorBrewer, 
               ggplot2)

#################  Carga información en memoria    ################

# inicio con datos para el analisis
load(here("datos_output","datos_para_analisis_todos.RData"))

datos <- st_drop_geometry(datos_analisis) %>% 
  filter(anyo == 2022)
rm(datos_analisis)


## Lee las funciones
source(here("Publicacion ministerio","Funciones_publicacion_barrios.R"))



### Definición de funciones y parámetros flextable ####
# Establezco parámetros por defecto para tablas

init_flextable_defaults() #reinicia los valores por defecto del paq flextable
set_flextable_defaults(
  big.mark = ".",
  decimal.mark = ",",
  digits = 1,
  # theme_fun = theme_box,
  font.family = "calibri",  
  line_spacing = 1.3,
  font.color = "grey8", # "#333333",
  font.size = 11,
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4,
  text.align = "c",
  border.color = "grey60", # "#333333",
  padding = 4,
  na_str = ""
)

# define el estilo del borde
border_style1 = officer::fp_border(color="grey60", width=2)
border_style2 = officer::fp_border(color="grey60", width=0.5)

# --------------------------------------------------------------------------
# Fabrico los listados de barrios por cada municipio
t <- genera_resumen_barrio(datos)

# t_flex <- resumen_a_flex(t)  # listado único de todos los municipios seguidos
# t_flex

lista_tablas_por_municipio <- split(t, ~ nombre)
lista_flex_barrios <- lapply(lista_tablas_por_municipio, resumen_a_flex)

# --------------------------------------------------------------------------
# Fabrico los mapas de barrios por municipio 
datos_barrio <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre", 
                                                 "barrio.codigo", "barrio.nombre"),
                                        list(character(0))) %>% 
  filter(!is.na(barrio.nombre)) %>% 
  filter(n >=10)


etiquetas <- c("Menos 2","2 a menos de 4","4 a menos de 6",
               "6 a menos de 8","8 a menos de 10","10 o más")

datos_barrio <- datos_barrio %>% 
  mutate(f.renta_m2 = cut(datos_barrio$preciom2_M,
                          breaks = c(0,2,4,6,8,10, Inf),
                          right = FALSE,    #Intervalos cerrados por la izquierda
                          include.lowest = TRUE,  # Para que incluya el valor máximo
                          dig.lab = 10,
                          labels = etiquetas))


# Si no existe el archivo que contiene las capas con los atributos, lo crea
if (!file.exists(here("datos_output","capas_para_mapas.Rdata"))) {
  source(here("Funciones.R"))
  # Ejecuta el script para crear las capas y añadir los campos
  Pasar_capas_shp_a_R()
} 

load(file = here("datos_output","capas_para_mapas.Rdata"))

barrios_sf <-  left_join(barrios_sf, 
                         datos_barrio, 
                         by = c("barrio.codigo" = "barrio.codigo") )


lista_barrios_por_municipio <- split(barrios_sf, ~municipio )
lista_mapas_barrios <- lapply(lista_barrios_por_municipio, crear_mapa_barrio)
legend <- cowplot::get_legend(lista_mapas_barrios[[1]])
# ggdraw(plot_grid(legend))


lista_mapas_barrios <- lapply(lista_mapas_barrios, function(mapa) {
  mapa + theme(legend.position = "none")
})

# ahora hacer el  Rmd

rmarkdown::render(
  input = here("Publicacion ministerio","Informe_barrios.Rmd"),
  output_file = here("Out_informes", 
                     glue("Publicacion_ministerio_{Sys.Date()}.pdf")) ) 






