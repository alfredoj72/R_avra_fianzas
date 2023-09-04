#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse","tmap", "readxl","leaflet") # c( "flextable","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)

# # Si voy a hacer mapas solo con datos de AVRA, sin los de catastro
# # Al intentar hacer mapas con datos de catastro dará error
# rm(list =ls())
# load("./datos_output/avra_catastro_2022.RData")
# datos <- avra_catastro_2022 ; rm(avra_catastro_2022)
# avra_datos_originales <- datos[["originales"]]
# # avra_catastro <- datos[["avra_catastro"]]
# # tabla_frecuencias  <- datos[["tabla_frecuencias"]]
# # tabla_frecuencias_final  <- datos[["tabla_frecuencias_final"]]
# # Fianzas_casan_1_vivienda <- datos[["Fianzas_casan_1_vivienda"]]
# # Fianzas_no_casan_catastro <- datos[["Fianzas_no_casan_catastro"]]
# # Fianzas_casan_distintas_viviendas <- datos[["Fianzas_casan_distintas_viviendas"]]
# # Fianzas_casan_distintas_viviendas_case <- datos[["Fianzas_casan_distintas_viviendas_case"]]
# datos <- avra_datos_originales
# 
# rm(avra_datos_originales)


rm(list =ls())
load("./datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["datos"]]
datos <- st_drop_geometry(datos)


# Si no existe el archivo que contiene las capas con los atributos, lo crea
if (!file.exists("datos_output/datos_para_mapas.Rdata")) {
  source("Funciones.R")
  # Ejecuta el script para crear las capas y añadir los campos
  crea_capas_y_campos()
} 

# Carga las capas
load(file = "datos_output/datos_para_mapas.Rdata")


## Con tmap ########


tmap_mode("view")   # interactivo. Para mapas fijos usar tmap_mode("plot")

provincia_sf$id_casos <- paste("Nº testigos:", provincia_sf$casos)
#Mapa provincias
mapa_prov <-
  tm_shape(provincia_sf) + 
  tm_fill(id = "id_casos",         # Se muestra el número de casos al pasar el cursor
          popup.vars = FALSE,    #No se activa información al picar 
          col="casos",
          title = "Nº de alquileres",
          group = "Registro de fianzas") + 
  tm_borders(lwd = 0.1)+
  tm_text("provincia", size = 1,
          group = "Nombres provincias") +
  
  # Indicamos un estilo. Existen varios:
  # white, gray, natural, bw, classic, cobalt, albatross, beaver
  tm_style("white", frame.lwd= 0 )+ 
  
  # Indicamos un formato. Existen varios predefinidos
  # World, World_wide, NLD, NLD_wide
  tm_format("World_wide", title="Registro de Fianzas") +
  tm_credits("Fuente: Registro de Fianzas. AVRA.", position = c("right", "BOTTOM"))
mapa_prov

# elimino el campo creado para no dejar basura en la capa
provincia_sf <- provincia_sf %>% select(-id_casos)



# tm_shape(provincia_sf, name = "Nº testigos" ) +
#   tm_polygons(col = "casos",
#           style = "cont",
#           border.col = "gray", lwd = 0.5
#           )
# 
# tm_shape(provincia_sf, name = "Nº testigos" ) +
#   tm_bubbles(col = "casos", size = "casos",
#               style = "cont"
#   ) +
#   tm_borders(lwd=0.2, col = "gray")


# # Modo continuo
# tm_shape(municipio_sf %>% filter(casos > 10), name = "Nº testigos" ) +
#   tm_fill(col = "casos",
#           style = "cont") +
#   tm_shape(municipio_sf, name = "Límite municipal")+
#   tm_borders(col = "gray", lwd = 0.2)+
#   tm_shape(provincia_sf, name = "Límite Provincial")+
#   tm_borders( col = "gray", lwd = 0.5)+
#   # Indicamos un formato. Existen varios predefinidos
#   # World, World_wide, NLD, NLD_wide
#   tm_format("World_wide", title="Alquileres registrados") 
# 
# 
# # Intervalos discretos  
# tm_shape(municipio_sf %>% filter(casos > 10)) +
#   tm_fill(col = "casos", style = "jenks",
#           colorNA = "white", textNA ="Sin datos") +
#   tm_shape(municipio_sf )+
#   tm_borders(col = "gray", lwd = 0.2)+
#   tm_shape(provincia_sf)+
#   tm_borders( col = "gray", lwd = 0.5)


####  Mapas de coropletas municipio ####

# Creo variable para ser usada al pasar cursor sobre municipio
municipio_sf$id_casos <- paste( municipio_sf$nombre,"-",  
                                municipio_sf$casos, "casos")

paleta = c("white", rev(hcl.colors(4, "YlOrBr")))

mapa_municipios_corop <-
tm_shape(municipio_sf) +
  tm_fill(id = "id_casos",      # Se muestra el número de casos al pasar el cusor
          popup.vars = FALSE,    #No se activa información al picar 
          col = "casos", palette = paleta, title="Nº de Casos",
          colorNA = "white", textNA ="Sin datos",
          style = "fixed",
          interval.closure = "right",
          breaks = c(-Inf,10, 100, 500, 1000, Inf) ,
          labels = c("0-10","11 - 100", "101 - 500", "501 - 1.000", "1.000 +"),
          group = "Registro de fianzas"
  ) +
  tm_layout(legend.format = list(text.separator = "-")) +
  
  tm_shape(municipio_sf )+
  tm_borders(col = "gray", lwd = 0.2,
             group = "Registro de fianzas")+
  
  tm_shape(provincia_sf)+
  tm_borders( col = "gray", lwd = 0.5,
              group = "Registro de fianzas") +
  
  tm_layout("Registro de Fianzas",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.bg.color = "white",
            legend.bg.alpha = 1)

print(mapa_municipios_corop )

# Elimino variables auxiliares para no dejar basura
municipio_sf <- municipio_sf %>% select(-id_casos)

## fin  ########################## 


####  Mapas de simbolos  ####

# Creo variable para ser usada al pasar cursor sobre municipio
municipio_sf$id_casos <- paste( municipio_sf$nombre,"-",  
                                municipio_sf$casos, "casos")

# Creo una  variable para que todos los municipios con < 10 casos salgan del mismo tamaño
municipio_sf$casos_aux <- ifelse(municipio_sf$casos<10, 0.0001, municipio_sf$casos)

paleta = c("white", rev(hcl.colors(4, "YlOrBr")))

mapa_municipios_bolas <- 
  tm_shape(municipio_sf) +
   tm_bubbles(id = "id_casos",       #Se muestra el número de casos al pasar el cusor
             popup.vars = FALSE,    #No se activa información al picar 
             title.col = "Nº de Casos",
             col = "casos",
             palette = paleta,
             size = "casos_aux" ,
             #size = "casos",
             scale = 7, alpha = 0.6,
             style = "fixed",
             interval.closure = "right",
             breaks = c(-Inf,10, 100, 500, 1000, Inf) ,
             labels = c("0 - 10", "11 - 100", "101 - 500", "501 - 1.000", "1.000 +"),
             group = "Registro de fianzas"
            # style = "prety"
  ) +
  tm_shape(provincia_sf )+
   tm_borders(lwd=0.2, col = "gray",
              group = "Registro de fianzas") +
  tm_layout("Registro de Fianzas",
              legend.title.size = 1,
              legend.text.size = 0.6,
              legend.bg.color = "white",
              legend.bg.alpha = 1)

print(mapa_municipios_bolas)

# Elimino variables auxiliares para no dejar basura
municipio_sf <- municipio_sf %>% select(-id_casos,-casos_aux)

###############  fin  ########################## 

# #Mapa 2, solo ejemplo. Fabrica 1 mapa por cada variable, en este 
# mapa_aux <- tm_shape(provincia_sf) +
#   tm_fill(col=c("casos","provincia"), title = c("Nº de alquileres","Provincias" )) +
#   tm_layout(main.title = "Main Title",
#             main.title.position = "center",
#             main.title.color = "blue",
#             title = c("Title 1", "Title 2"),
#             title.color = "red",
#             panel.labels = c("Panel Label 1", "Panel Label 2"),
#             panel.label.color = "purple",
#             legend.text.color = "brown")
# 
# mapa_aux

# Con tmap_options() puede conocer todos los parámetros activos
#tmap_options()




#######################################################################
####  Mapas de coropletas barrios ####   Testigos

# Creo variable para ser usada al pasar cursor sobre municipio
barrios_sf$id_casos <- paste( barrios_sf$nombre,"-",  
                              barrios_sf$casos, "casos")

paleta = c("white", rev(hcl.colors(4, "YlOrBr")))
max <- max(barrios_sf$casos, rm.na = TRUE)
mapa_barrios_corop <-
  # tm_shape(municipio_sf )+      # ESto lo hace pesado y lento
  # tm_borders(col = "gray", lwd = 3,
  #            group = "Registro de fianzas")+
  tm_basemap(NULL) +  # (NULL) +
  
  tm_tiles("OpenStreetMap", alpha = 0.35, 
           group = "Fondo: OpenStreetMap") +
  
  tm_shape(municipio_barrio_sf )+
  tm_borders(col = "thistle", lwd = 2.5,
             group = "Registro de fianzas")+
  #tm_facets(by = "nombre", ncol = 2) +
  
  tm_shape(barrios_sf) +
  tm_fill(id = "id_casos",      # Se muestra el número de casos al pasar el cusor
          popup.vars = FALSE,    #No se activa información al picar 
          col = "casos", palette = paleta, title="Nº de Casos",
          colorNA = "white", textNA ="Sin datos",
          style = "fixed",
          interval.closure = "left",
          breaks = c(0,10, 50, 100, 300, max) ,
          labels = c("0-9","10 - 49", "50 - 99", "100 - 299", "300 +"),
          group = "Registro de fianzas"
  ) + 
  tm_borders(col = "thistle", lwd = 2,
             group = "Registro de fianzas")+
  tm_layout(legend.format = list(text.separator = "-"))  +
  
  tm_facets(by = "municipio", ncol = 2) 

tmap_mode("view")
print(mapa_barrios_corop)

# tmap_mode("plot") # no la opción plot no consigo que los mapas se vean bien
# print(mapa_barrios_corop + tm_facets( 'municipio',free.scales = T))

# Elimino variables auxiliares para no dejar basura
barrios_sf <- barrios_sf %>% select(-id_casos)

# fin mapa coropletas casos


#######################################################################
####  Mapas de coropletas barrios ####   Precio_m2

# questionr::icut(barrios_sf, "mediana_renta_m2")
# me inspiro en 5 cortes con metodo cuantíles: 3,6,9,12

# Creo variable para ser usada al pasar cursor sobre municipio
barrios_sf$id_casos <- paste( barrios_sf$nombre,"-",
                              round(barrios_sf$mediana_renta_m2,1)
                              , "€/m2")

barrios_sf$id_casos2 <- paste( barrios_sf$nombre,"- Sin datos (< 10 testigos)")       

paleta = c(rev(hcl.colors(6, "YlOrBr")))
max <- max(barrios_sf$mediana_renta_m2, na.rm = TRUE)
mapa_barrios_corop <-
  # tm_shape(municipio_sf )+      # ESto lo hace pesado y lento
  # tm_borders(col = "gray", lwd = 3,
  #            group = "Registro de fianzas")+
  tm_basemap(NULL) +  # (NULL) +
  
  tm_tiles("OpenStreetMap", alpha = 0.35, 
           group = "Fondo: OpenStreetMap") +
  
  tm_shape(municipio_barrio_sf )+
  tm_borders(col = "thistle", lwd = 2.5,
             group = "Registro de fianzas")+
  #tm_facets(by = "nombre", ncol = 2) +
  
  tm_shape(barrios_sf %>% filter(casos < 10)) +
  tm_fill(id = "id_casos2", 
          col = "gray95")+
  
  tm_shape(barrios_sf %>% filter(casos >= 10)) +
  tm_fill(id = "id_casos",      # Se muestra el número de casos al pasar el cusor
          popup.vars = FALSE,    #No se activa información al picar 
          col = "mediana_renta_m2", 
          palette = paleta, 
          title="Mediana Precio mensual €/m2",
          colorNA = "gray95", textNA ="Sin datos",
          # style = "pretty",
          # n=5,
          interval.closure = "left",
          
          breaks = c(0,2, 4, 6, 8, 10, max) ,
          labels = c("[0-2)","[2 - 4)", "[4 - 6)", "[6 - 8)","[8 - 10)", "[10 +)"),
          
          # opción mia
          # breaks = c(0,3, 6, 9, 12, max) ,
          # labels = c("[0-3)","[3 - 6)", "[6 - 9)", "[9 - 12)", "[12 +)"),
          group = "Registro de fianzas"
  ) + 
  tm_borders(col = "thistle", lwd = 2,
             group = "Registro de fianzas")+
  tm_layout(legend.format = list(text.separator = "-")) +
  
  tm_facets(by = "municipio", ncol = 2) 


print(mapa_barrios_corop)
# Elimino variables auxiliares para no dejar basura
barrios_sf <- barrios_sf %>% select(-id_casos)


## FIN mapa coropletas renta_m2


#######################################################################
####  Mapas de coropletas barrios ####   Precio

# usar para encontrar los mejores intervalos
# questionr::icut(barrios_sf, "mediana_renta")

# Creo variable para ser usada al pasar cursor sobre municipio
barrios_sf$id_casos <- paste( barrios_sf$nombre,"-",
                              round(barrios_sf$mediana_renta,1)
                              , "€/mes")

barrios_sf$id_casos2 <- paste( barrios_sf$nombre,"- Sin datos (< 10 testigos)")       

paleta = c(rev(hcl.colors(5, "YlOrBr")))
max <- max(barrios_sf$mediana_renta, na.rm = TRUE)
mapa_barrios_corop <-
  # tm_shape(municipio_sf )+      # ESto lo hace pesado y lento
  # tm_borders(col = "gray", lwd = 3,
  #            group = "Registro de fianzas")+
  tm_basemap(NULL) +  # (NULL) +
  
  tm_tiles("OpenStreetMap", alpha = 0.35, 
           group = "Fondo: OpenStreetMap") +
  
  tm_shape(municipio_barrio_sf )+
  tm_borders(col = "thistle", lwd = 2.5,
             group = "Registro de fianzas")+
  #tm_facets(by = "nombre", ncol = 2) +
  
  tm_shape(barrios_sf %>% filter(casos < 10)) +
  tm_fill(id = "id_casos2", 
          col = "gray95")+
  
  tm_shape(barrios_sf %>% filter(casos >= 10)) +
  tm_fill(id = "id_casos",      # Se muestra el número de casos al pasar el cusor
          popup.vars = FALSE,    #No se activa información al picar 
          col = "mediana_renta", 
          palette = paleta, 
          title="Mediana Precio mensual €/mes",
          colorNA = "gray95", textNA ="Sin datos",
          # style = "pretty",
          # n=5,
          interval.closure = "left",
          breaks = c(0,300, 500, 700, 900, 1200,  max) ,
          labels = c("[0-300)","[300 - 500)", "[500 - 700)", "[700 - 900)",
                     "[900 - 1.200)","[1.200 +)"),
          group = "Registro de fianzas"
  ) + 
  tm_borders(col = "thistle", lwd = 2,
             group = "Registro de fianzas")+
  tm_layout(legend.format = list(text.separator = "-")) +
  
  tm_facets(by = "municipio", ncol = 2) 


print(mapa_barrios_corop)
# Elimino variables auxiliares para no dejar basura
barrios_sf <- barrios_sf %>% select(-id_casos)


## FIN mapa coropletas precio renta


# Para conseguir que en cada facet salgan los barrios de un municipio y el 
# contorno del municipio debo quedarme solo con los municipios presentes en 
# la capa de barrios ya que si los pongo todos el tamaño se dispara y el 
# resultado se hace muy lento
# Voy a quedarme solo con los municipios que hay en la capa de barrios

# tomo la lista de municipios presentes en la capa de barrios
aux_mun <- unique(barrios_sf$cod_mun)
aux_mun <-  data.frame(cod_mun = aux_mun)

# me quedo solo con los municipios de la tabla anterior
municipio_barrio_sf <- municipio_sf %>% 
  right_join( aux_mun, by = c("cod_mun" = "cod_mun"))


# ------------------------------------------------------------------
# Defino función que hace mapas de coropletas sub-municipal 
# con facetas por municipio

coropletas_facetado <- function(capa, c_filtro, c_mapa, c_id1, c_id2, titulo,
                                roturas, etiquetas){
  
  paleta = c(rev(hcl.colors(5, "YlOrBr")))
  #max <- max(capa[[c_mapa]], na.rm = TRUE)
  
  m <-
    # tm_shape(municipio_sf )+      # ESto lo hace pesado y lento
    # tm_borders(col = "gray", lwd = 3,
    #            group = "Registro de fianzas")+
    tm_basemap(NULL) +  # (NULL) +
    
    tm_tiles("OpenStreetMap", alpha = 0.35, 
             group = "Fondo: OpenStreetMap") +
    
    tm_shape(municipio_barrio_sf )+
    tm_borders(col = "thistle", lwd = 2.5,
               group = "Registro de fianzas")+
    #tm_facets(by = "nombre", ncol = 2) +
    
    tm_shape(capa %>% filter({{c_filtro}} < 10 | is.na({{c_filtro}}))) +
    tm_fill(id = c_id2, 
            col = "gray95")+
    
    tm_shape(capa %>% filter({{c_filtro}} >= 10)) +
    tm_fill(id = c_id1,      # Se muestra el número de casos al pasar el cusor
            popup.vars = FALSE,    #No se activa información al picar 
            col = c_mapa, 
            palette = paleta, 
            title= titulo,
            colorNA = "gray95", textNA ="Sin datos",
            # style = "pretty",
            # n=5,
            interval.closure = "left",
            breaks = c(roturas) ,
            labels = etiquetas,
            group = "Registro de fianzas"
    ) + 
    tm_borders(col = "thistle", lwd = 2,
               group = "Registro de fianzas")+
    tm_layout(legend.format = list(text.separator = "-")) +
    
    tm_facets(by = "municipio", ncol = 2) 
  
  return(m)
  
} #fin funcion


#----------------------------------------------------------------------------
# mapa coropletas renta  todas las viviendas
# Creo variable para ser usada al pasar cursor sobre municipio
barrios_sf$id_casos1 <- paste( barrios_sf$nombre,"-",
                               round(barrios_sf$mediana_renta,1), "€/mes")

barrios_sf$id_casos2 <- paste( barrios_sf$nombre,"- Sin datos (< 10 testigos)")  

max <- max(barrios_sf$mediana_renta, na.rm = TRUE)
mapa <- coropletas_facetado(barrios_sf, 
                            c_filtro = casos, 
                            c_mapa = "mediana_renta",
                            c_id1 = "id_casos1",
                            c_id2 = "id_casos2",
                            titulo ="Mediana Precio €/mes",
                            roturas = c(0,300, 500, 700, 900, 1200, max),
                            etiquetas = c("[0-300)","[300 - 500)", "[500 - 700)", 
                                          "[700 - 900)","[900 - 1.200)","[1.200 +)") )

print(mapa)
barrios_sf <- barrios_sf %>% select(- c(id_casos1, id_casos2))

#----------------------------------------------------------------------------
# mapa coropletas renta_m2 todas las viviendas
barrios_sf$id_casos1 <- paste( barrios_sf$nombre,"-",
                               round(barrios_sf$mediana_renta_m2,1), "€/m2")

barrios_sf$id_casos2 <- paste( barrios_sf$nombre,"- Sin datos (< 10 testigos)")  

max <- max(barrios_sf$mediana_renta_m2, na.rm = TRUE)
mapa <- coropletas_facetado(barrios_sf, 
                            c_filtro = casos, 
                            c_mapa = "mediana_renta_m2",
                            c_id1 = "id_casos1",
                            c_id2 = "id_casos2",
                            titulo ="Mediana Precio €/m2 mes",
                            roturas = c(0,2, 4, 6, 8, 10, max) ,
                            etiquetas = c("[0-2)","[2 - 4)", "[4 - 6)", 
                                          "[6 - 8)","[8 - 10)", "[10 +)") )
print(mapa)
barrios_sf <- barrios_sf %>% select(- c(id_casos1, id_casos2))


#----------------------------------------------------------------------------
# mapa coropletas renta_m2 viviendas plurifamiliares
barrios_sf$id_casos1 <- paste( barrios_sf$nombre,"-",
                               round(barrios_sf$Plurifamiliar_mediana_renta_m2,1),
                               "€/m2")

barrios_sf$id_casos2 <- paste( barrios_sf$nombre,"- Sin datos (< 10 testigos)")  

max <- max(barrios_sf$Plurifamiliar_mediana_renta_m2, na.rm = TRUE)
mapa <- coropletas_facetado(barrios_sf, 
                            c_filtro = Plurifamiliar_casos, 
                            c_mapa = "Plurifamiliar_mediana_renta_m2",
                            c_id1 = "id_casos1",
                            c_id2 = "id_casos2",
                            titulo ="Mediana Precio €/m2 mes. V plurifamiliares.",
                            roturas = c(0,2, 4, 6, 8, 10, max) ,
                            etiquetas = c("[0-2)","[2 - 4)", "[4 - 6)", 
                                          "[6 - 8)","[8 - 10)", "[10 +)") )
print(mapa)
barrios_sf <- barrios_sf %>% select(- c(id_casos1, id_casos2))


#----------------------------------------------------------------------------
# mapa coropletas renta_m2 union de barrios todas las viviendas
barrios_union_sf$id_casos1 <- paste( barrios_union_sf$distrito,"-",
                                     round(barrios_union_sf$mediana_renta_m2,1), "€/m2")

barrios_union_sf$id_casos2 <- paste( barrios_union_sf$distrito,"- Sin datos (< 10 testigos)")  

max <- max(barrios_union_sf$mediana_renta_m2, na.rm = TRUE)
mapa <- coropletas_facetado(barrios_union_sf, 
                            c_filtro = casos, 
                            c_mapa = "mediana_renta_m2",
                            c_id1 = "id_casos1",
                            c_id2 = "id_casos2",
                            titulo ="Macrobarrios. Mediana Precio €/m2 mes",
                            roturas = c(0,2, 4, 6, 8, 10, max) ,
                            etiquetas = c("[0-2)","[2 - 4)", "[4 - 6)", 
                                          "[6 - 8)","[8 - 10)", "[10 +)") )
tmap_mode("view")
print(mapa)
barrios_union_sf <- barrios_union_sf %>% select(- c(id_casos1, id_casos2))

#----------------------------------------------------------------------------

# con la funcion coropletas_facetado Puedo hacer mapas de distrito,
# seecciones, barrios y macrobarrios.
# tengo datos de casos, mediana de (precio, precio_m2 y superficie) para
# cada una de las unidades territoriales inframunicipal, ADEMAS,
# para el caso de los barrios he atributos distinguiendo si son unifamiliares o 
# colectivas y si son de menos o más de 90 m2







####### pasando de tmap a leaflet #### NO SIRVE CON FACETS

source("mapas tmap.R")
# parto de la idea de que ya tengo los 3 mapas creados con tmap:
# mapa_prov, mapa_municipios_corop y mapa_municipios_bolas

# Buscando capas podría usar de fondo. Buscar con Qgis



#### Mapa de municipios
library(leaflet)
mapa <- tmap_leaflet(mapa_municipios_corop) %>% 
  
  addWMSTiles("http://www.callejerodeandalucia.es/servicios/base/wms?",
              # layers = c("contexto_andalucia", "nucleos_poblacion", "batimetria","CDAU_base", "CDAU_toponimia"),
              layers = c("contexto_andalucia","CDAU_base"),
              options = WMSTileOptions(format = "image/jpeg",
                                       transparent = FALSE),
              attribution = "Callejero Digital de Andalucía Unificado.IECA",
              group = "Base CDAU") %>%
  
  addWMSTiles("http://www.ign.es/wms-inspire/ign-base",
              layers = c("Callejero"),
              # options = WMSTileOptions(format = "image/jpeg",
              #                          transparent = FALSE),
              # attribution = "Base Topográfica.IGN",
              group = "IGN") %>%
  
  addLayersControl(baseGroups = c("Base CDAU","IGN"),
                   overlayGroups = c( "Registro de fianzas"),
                   options = layersControlOptions(collapsed = F))

mapa 
# OTRAS WMS: OJO en portátil todo va bien. En pc sobremesa solo alguna de las capas de fondo. openstreetmap si va bien
# busco wms de base para usar en mapas regionales
# http://www.ideandalucia.es/services/mta400v_2016/wms  mta400v_2016
# https://www.ign.es/wms-inspire/ign-base callejero
# https://www.ign.es/wms-inspire/pnoa-ma  OI.OrthoimageCoverage


# Si uso tmap_leflet los mápas son más fáciles de construir y no tengo que 
# proyectar las capas para suministrarlas a leaflet pero no puedo construir
# leyendas de simbología de puntos.


################## Fin de uso tmap_leaflet ##################