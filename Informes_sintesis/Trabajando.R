
#> Hacer informe html con tablas flextable y mapas leaflet
#> hacer informe html con tablas interactivas y mapas leaflet
#> Hacer informe provincias + municipios PDF
#> Hacer informe provincias + municipios html


rm(list = ls())


# carga paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, flextable,here , sf, readxl,sf, glue, RColorBrewer, 
               ggplot2, cowplot, leaflet)

## Lee las funciones
#source(here("Informes_sintesis","Funciones_para_publicaciones.R"))

# Lee los datos
load(here("datos_output","datos_para_analisis_todos.RData"))

# datos <- st_drop_geometry(datos_analisis) %>% 
#   filter(anyo == 2022)

source(here("Informes_sintesis", "Elaboracion_contenidos_municipios.R"))

# Elaboración de tablas y mapas






rmarkdown::render(
  input = here("Informes_sintesis","kk.Rmd"),
  output_file = here("Out_informes", 
                     glue("Informe_kk_{Sys.Date()}.html")) ) 



