
#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse","flextable","ggplot2","tmap") # c( "ggplot2","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)


## Construccion de los datos de partida para tmap ########
# parto
rm(list =ls())
load("./datos_output/avra_catastro_2022.RData")
datos <- avra_catastro_2022 ; rm(avra_catastro_2022)
avra_datos_originales <- datos[["originales"]]
# avra_catastro <- datos[["avra_catastro"]]
tabla_frecuencias  <- datos[["tabla_frecuencias"]]
tabla_frecuencias_final  <- datos[["tabla_frecuencias_final"]]
# Fianzas_casan_1_vivienda <- datos[["Fianzas_casan_1_vivienda"]]
# Fianzas_no_casan_catastro <- datos[["Fianzas_no_casan_catastro"]]
# Fianzas_casan_distintas_viviendas <- datos[["Fianzas_casan_distintas_viviendas"]]
# Fianzas_casan_distintas_viviendas_case <- datos[["Fianzas_casan_distintas_viviendas_case"]]
datos <- avra_datos_originales

rm(avra_datos_originales, tabla_frecuencias, tabla_frecuencias_final)

##Cargar las capas y añadir datos   #

# ## Carga de capas shp ubicadas en directorio local
#  provincia_sf <- st_read(dsn = "./capas_in/13_01_Provincia.shp")
#  municipio_sf <- st_read(dsn = "./capas_in/13_01_TerminoMunicipal.shp")

# Carga de capas servidas a través de servicios WFS
# Especifica la URL del servicio WFS
tipo <- "WFS"
url_wfs <- "http://www.ideandalucia.es/services/DERA_g13_limites_administrativos/wfs?"
peticion <- "request=GetCapabilities"
orden <- paste(tipo,":",url_wfs,peticion, sep = "")

# Obtén la lista de capas disponibles en el WFS
capas_disponibles <- st_layers(orden)

# Muestra la lista de capas
print(capas_disponibles["name"])

# # Añadir las capas indicando el nombre completo
# name_capa <- "DERA_g13_limites_administrativos:g13_01_Provincia"
# provincia_sf <- st_read(dsn = orden, layer = name_capa)
#
# name_capa <- "DERA_g13_limites_administrativos:g13_01_TerminoMunicipal"
# municipio_sf <- st_read(dsn = orden, layer = name_capa)

# Añadir las capas buscando texto dentro de su nombre
# type = 6 devuelve geometría de tipo MULTIPOLYGON
lista_capas <- capas_disponibles[[1]]
name_provincia <- lista_capas[grepl("Provincia", lista_capas)]
provincia_sf <- st_read(dsn = orden, layer = name_provincia, type = 6)

name_municipio <- lista_capas[grepl("Municipal", lista_capas)]
municipio_sf <- st_read(dsn = orden, layer = name_municipio, type = 6)

# plot(provincia_sf)
rm(tipo,url_wfs,peticion,orden,capas_disponibles,lista_capas,name_provincia,name_municipio)

# # Simplificar los polígonos para acelerar los proceso
# municipio_sf <- st_simplify(municipio_sf, dTolerance = 1)
# provincia_sf <- st_simplify(provincia_sf, dTolerance = 1)
# # No lo uso porque no veo diferencia de tiempo y sin embargo si se producen algunos
# # cambios en el comportamiento de la capa ya que pasa de MULTIPOLYGON a GEOMETRY

# Obtener resúmenes de datos para provincias y municipios

datos_provincia <- datos %>%
  group_by(provincia_806) %>%
  summarise(casos = n(), .groups = "drop")

datos_provincia <- datos_provincia %>% 
  mutate(codigo = c("04","11","14","18","21","23","29","41"))

datos_municipio <- datos %>%
  group_by(codigo_ine) %>%
  summarise(casos = n(), .groups = "drop")

# Añadir los datos numéricos a las capas para poder representarlos en mapas

provincia_sf <- provincia_sf %>% 
  left_join (datos_provincia, by = c("codigo" = "codigo")) # %>% 
# mutate(etiqueta = paste(provincia,"\n",casos))


municipio_sf <- municipio_sf %>% 
  left_join (datos_municipio, by = c("cod_mun" = "codigo_ine"))     %>%
  mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0

save.image(file = "datos_para_mapas.Rdata")



## Con tmap ########

rm(list =ls())
load(file = "datos_para_mapas.Rdata")

tmap_mode("view")   # interactivo. Para mapas fijos usar tmap_mode("plot")

provincia_sf$id_casos <- paste("Nº testigos:", provincia_sf$casos)
#Mapa provincias
mapa_prov <-
  tm_shape(provincia_sf) + 
  tm_fill(id = "id_casos",         # Se muestra el número de casos al pasar el cusor
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

#cambiar el campo que usa como etiqueta al poner el cursos sobre la provincia!!!!!

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

# Creo una  variable para que todos los municipios con > 10 casos salgan del mismo tamaño
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







####### pasando de tmap a leaflet ####

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
# OTRAS WMS: OJO en portátil todo va bien. En pc sobremesa no
# busco wms de base para usar en mapas regionales
# http://www.ideandalucia.es/services/mta400v_2016/wms  mta400v_2016
# https://www.ign.es/wms-inspire/ign-base callejero
# https://www.ign.es/wms-inspire/pnoa-ma  OI.OrthoimageCoverage


# Si uso tmap_leflet los mápas son más fáciles de construir y no tengo que 
# proyectar las capas para suministrarlas a leaflet pero no puedo construir
# leyendas de simbología de puntos.


################## Fin de uso tmap_leaflet ##################