# PRINCIPAL

# Quitamos la notación científica
options(scipen = 999) # Para que no use notación científica
options(digits = 2)  # Para que muestre solo 2 decimales

# BORRAR todos los ELEMENTOS en memoria
rm(list = ls())

# Comprueba que las capas están preparadas y si no lo están las fabrica

if (!file.exists("./capas/andalucia.shp") | !file.exists("./capas/provincias.shp")) {
  source("Crea_capas_pruebas.R")
}

if (!file.exists("./capas/ambito_exterior_viso.shp")) {
  source("Crea_capas_viso.R")
}

rmarkdown::render(
  input = "pruebas_proyeccion_leaflet.Rmd",
  output_file = "pruebas_proyeccion_leaflet.html")

rmarkdown::render(
  input = "MapaVisoRiosHTML.Rmd",
  output_file = "MapaVisoRiosHTML.html"
)


rmarkdown::render(
  input = "pruebas_proyeccion_plot.Rmd",
  output_file = "pruebas_proyeccion_plot.html")

#NO ME CONVENCE LA PROYECCION 4326, EN QGIS LA VEO DISTINTA


rmarkdown::render(
  input = "pruebas_proyeccion_ggplot.Rmd",
  output_file = "pruebas_proyeccion_ggplot.html")
