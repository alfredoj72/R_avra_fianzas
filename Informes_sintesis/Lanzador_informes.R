# Quitamos la notación científica
options(scipen = 999) # Para que no use notación científica
options(digits = 2)  # Para que muestre solo 2 decimales

# BORRAR todos los ELEMENTOS en memoria
rm(list = ls())
library(here) 
library(glue)



# CORRECTO: Elabora informe de barrios
rmarkdown::render(
  input = here("Informes_sintesis","Informe_barrios_salida_pdf.Rmd"),
  output_file = here("Out_informes",
                     glue("Informe_barrios_{Sys.Date()}.pdf")) )



#Elabora informe de provincias
rmarkdown::render(
  input = here("Informes_sintesis","Informe_provincias_salida_html.Rmd"),
  output_file = here("Out_informes", 
                     glue("Informe_provincias_{Sys.Date()}.html")) ) 



#Elabora informe de municipios con mapa y listado regional
rmarkdown::render(
  input = here("Informes_sintesis","Informe_municipios_salida_regional_html.Rmd"),
  output_file = here("Out_informes", 
                     glue("Informe_municipios_regional_{Sys.Date()}.html")) ) 

rmarkdown::render(
  input = here("Informes_sintesis","Informe_municipios_salida_html.Rmd"),
  output_file = here("Out_informes", 
                     glue("Informe_municipios_{Sys.Date()}.html")) ) 
