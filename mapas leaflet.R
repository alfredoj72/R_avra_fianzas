#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse","leaflet", "readxl") # c( "flextable","classInt") 
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



## Con leaflet ########




########### CONTRUYENDO MAPAS CON LEAFLET DESDE 0, SIN PARTIR DE TMAP  #####


# Leaflet precisa que las capas esten en WGS84 sin proyectar
# tmap_leaflet() hace la conversión de proyección al vuelo 

# Tomo un punto central interior de cada provincia y municipio
centro_provincia <- provincia_sf %>% 
  mutate(point_within = st_point_on_surface(geometry)) %>% 
  as.data.frame() %>%  # Convierte a data.frame, los campos de geometria no tienen efect
  select(-geometry) %>%    # Elimina el campo con la geometría de polígonos
  st_as_sf()           # Genera geometría y la toma por defecto de point_within

centro_municipio <- municipio_sf %>% 
  mutate(point_within = st_point_on_surface(geometry)) %>% 
  as.data.frame() %>%  # Convierte a data.frame, los campos de geometria no tienen efecto
  select(-geometry) %>%    # Elimina el campo con la geometría de polígonos
  st_as_sf()           # Genera geometría y la toma por defecto de point_within


# A leaflet le tengo que dar las capas sin proyectar.
provincia_sf_g <-   st_transform(provincia_sf, crs = 4326)
municipio_sf_g <-   st_transform(municipio_sf, crs = 4326)
centro_provincia <- st_transform(centro_provincia, crs = 4326)
centro_municipio <- st_transform(centro_municipio, crs = 4326)
 
centro_provincia <- centro_provincia %>% 
  mutate(lng=st_coordinates(centro_provincia)[,1],
         lat=st_coordinates(centro_provincia)[,2]) 

centro_municipio <- centro_municipio %>% 
  mutate(lng=st_coordinates(centro_municipio)[,1],
         lat=st_coordinates(centro_municipio)[,2]) 

# # pa comprobar la capa
# leaflet(provincia_sf_g)  %>%  # Creamos un objeto leaflet
#   addTiles()  %>%  # Le añadimos un fondo bonito. VER LISTADO EN
#   addProviderTiles("Esri.WorldTerrain") %>%   # VER LISTADO EN
#   addPolygons() # Le aniadimos los poligonos


#  Por provincia colores graduados  ####
# 1. Paleta de colores. 
pal_colores <- colorNumeric(palette = "YlOrBr", domain = provincia_sf_g$casos)

# 2. Popup 
popup <- paste0("<b>Provincia: </b>", provincia_sf_g$provincia, "<br>", 
                "<b>Casos: </b>", provincia_sf_g$casos, "<br>")

leaflet() %>%  # Creamos un objeto leaflet
  addProviderTiles("CartoDB.Positron") %>%  # Le aniadimos un fondo bonito
  addPolygons(data = provincia_sf_g,
              color = "gray88", #color de la línea de contorno
              weight = 2,       # ancho del trazo en pixeles
              smoothFactor = 0.5,  # Simplificación de las líneas
              opacity = 1.0,
              fillColor = pal_colores(provincia_sf_g$casos), #aplica la paleta
              fillOpacity = 0.6,
              
              #Ventana emergente cuando se pica un elementos
              popup = popup, 
              
              #Etiqueta cuando pasa el cursor sobre un poligono
              label = str_glue("{provincia_sf_g$provincia} - Depósitos {provincia_sf_g$casos}"),
              
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), #highlight cuando pasas el cursor
              
              layerId = ~provincia_sf_g$provincia #????
  ) %>% 
  addLegend(position = "bottomleft", pal= pal_colores, 
            values = provincia_sf_g$casos,
            title = "Registro de Alquileres")



#  Por municipios colores graduados  ####

muncipios_con_color <- municipio_sf_g %>% filter(casos >10)

# 1. Paleta de colores. 
pal_colores <- colorNumeric(palette = "YlOrBr", 
                            domain = muncipios_con_color$casos,
                            na.color = "white")

# 2. Popup 
popup <- paste0("<b>Provincia: </b>", municipio_sf_g$nombre, "<br>", 
                "<b>Casos: </b>", municipio_sf_g$casos, "<br>")

leaflet() %>%  # Creamos un objeto leaflet
  addProviderTiles("CartoDB.Positron") %>%  # Le aniadimos un fondo bonito
  
  # Fondo blanco
  addPolygons(data = provincia_sf_g,
              fillColor = "white", fillOpacity  = 1.0,
              stroke  = F, smoothFactor = 0.5,
              group = "Registro de fianzas") %>% 
  
  # Los municipios con su color
  addPolygons(data = muncipios_con_color,
              stroke = FALSE,
              smoothFactor = 0.5,  # Simplificación de las líneas
              opacity = 1.0,
              fillColor = pal_colores(muncipios_con_color$casos), #aplica la paleta
              fillOpacity = 1,
              group = "Registro de fianzas") %>% 
  
  # La etiqueta, la ventana emergente y el resaltado 
  addPolygons(data = municipio_sf_g ,
              color = "gray", #color de la línea de contorno
              weight = 0.1,       # ancho del trazo en pixeles
              smoothFactor = 0.5,  # Simplificación de las líneas
              #opacity = 1.0,
              #fillColor = pal_colores(municipio_sf_g$casos), #aplica la paleta
              fillOpacity = 0,
              
              #Ventana emergente cuando se pica un elementos
              popup = popup, 
              
              #Etiqueta cuando pasa el cursor sobre un poligono
              label = str_glue("{municipio_sf_g$nombre} - Casos: {municipio_sf_g$casos}"),
              
              #highlight cuando pasas el cursor
              highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                  bringToFront = TRUE),
              group = "Registro de fianzas") %>%
 
  # La línea de provincias
  addPolylines(data = provincia_sf_g,
              color = "gray", #color de la línea de contorno
              weight = 0.2,       # 
              fillOpacity  = 0, smoothFactor = 0.5,
              group = "Registro de fianzas") %>% 
  
  addLegend(position = "bottomleft", pal= pal_colores, opacity =1,
            values = municipio_sf_g$casos,
            title = "Registro de fianzas")  %>% 
  
  addLayersControl(overlayGroups = c( "Registro de fianzas"),
                   options = layersControlOptions(collapsed = F))



#  Por municipios grupos manuales   ####

# Construyo campo auxiliar para hacer el mapa
breaks <- c(-Inf, 10,100,500, 1000, Inf) # El máximo no llegaa 5100
municipio_sf_g$casos_aux <- cut(municipio_sf_g$casos, 
                                breaks = breaks,
                                labels = c("0-10","11 - 100", "101 - 500", "501 - 1.000", "1.000 +"))

# 1. Paleta de colores. 
pal <- colorFactor(palette = c("white", rev(hcl.colors(4, "YlOrBr"))),
                   levels = levels(municipio_sf_g$casos_aux))

# 2. Popup 
popup <- paste0("<b>Provincia: </b>", municipio_sf_g$nombre, "<br>", 
                "<b>Casos: </b>", municipio_sf_g$casos, "<br>")

leaflet() %>%  # Creamos un objeto leaflet
  
  addProviderTiles("CartoDB.Positron") %>%  # Le aniadimos un fondo bonito
  
  addWMSTiles("http://www.ign.es/wms-inspire/ign-base",
              layers = c("Callejero"),
              # options = WMSTileOptions(format = "image/jpeg",
              #                          transparent = FALSE),
              # attribution = "Base Topográfica.IGN",
              group = "IGN") %>%
  
  addPolygons(data = municipio_sf_g,
              weight = 0,
              smoothFactor = 0.5,  # Simplificación de las líneas
              opacity = 1.0,
              fillColor = ~pal(casos_aux), #aplica la paleta
              fillOpacity = 1) %>% 
  
  # La etiqueta, la ventana emergente y el resaltado 
  addPolygons(data = municipio_sf_g ,
              color = "gray", #color de la línea de contorno
              weight = 0.1,       # ancho del trazo en pixeles
              smoothFactor = 0.5,  # Simplificación de las líneas
              #opacity = 1.0,
              #fillColor = pal_colores(municipio_sf_g$casos), #aplica la paleta
              fillOpacity = 0,
              
              #Ventana emergente cuando se pica un elementos
              popup = popup, 
              
              #Etiqueta cuando pasa el cursor sobre un poligono
              label = str_glue("{municipio_sf_g$nombre} - Casos: {municipio_sf_g$casos}"),
              
              #highlight cuando pasas el cursor
              highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  
  # La línea de provincias
  addPolylines(data = provincia_sf_g,
               color = "gray", #color de la línea de contorno
               weight = 0.2,       # 
               fillOpacity  = 0, smoothFactor = 0.5) %>% 
  
  addLegend(position = "bottomleft", pal= pal, opacity =1,
            values = municipio_sf_g$casos_aux,
            title = "Registro de Alquileres")



# Elimino variables auxiliares para no dejar basura
municipio_sf_g <- municipio_sf_g %>% select(-casos_aux)

# OTRAS WMS: OJO en portátil todo va bien. En pc sobremesa no
# busco wms de base para usar en mapas regionales
# http://www.ideandalucia.es/services/mta400v_2016/wms  mta400v_2016
# https://www.ign.es/wms-inspire/ign-base callejero
# https://www.ign.es/wms-inspire/pnoa-ma  OI.OrthoimageCoverage




#  Por municipios colores graduados círculos proporcionales ####

# La construcción de leyendas para tamaño de símbolos está escamente documentada
# La solución que he implementado no la he visto en ningún sitio, simplemente
# he tomado ideas de varios sitienos y he llevado a cabo la siguiente solución.
# La idea es tener controlado el tamaño en píxeles que muestran los elementos del
# mapa y el tamaño en pixeles que muestran los elementos de la leyenda.
# La función addCircleMarkers toma el tamaño en pixeles del campo radius
# La función addLegend con la opción colors expresa en html el tamaño de la bola
# (border-radius: 50% es una bola) en pixeles. El tamaño de la bola debe ser el 
# doble del radio.
# Por tanto se toma como radius = sqrt(sizes)/2
# y como leyenda los valores   sqrt(de los tamaños elegidos)
# Con esto se consigue que por ejemplo, un municipio con 5000 testigos tenga
# un radio de sqrt(5000)/2
# y en la leyenda el valor 5000 tenga tamaño sqrt(5000), con lo cual su radio que
# es la mitad del tamaño tiene sqrt(5000)/2



# Función para construir la leyenda
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border: 1px solid gray; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity,
                   title = "Nº de testigos"))
}

sizes_originales <- c(10, 250, 500, 1000, 5000) # Tamaños que se mostrarán en la leyenda
sizes_pix <- sqrt(sizes_originales)
labels <- as.character(sizes_originales)  # Etiquetas para la leyenda

# Creo variable para ser usada al pasar cursor sobre municipio
centro_municipio$id_casos <- paste( municipio_sf$nombre,"-",  
                                    municipio_sf$casos, "casos")

popup <- paste0("<b>Provincia: </b>", municipio_sf_g$nombre, "<br>", 
                "<b>Casos: </b>", municipio_sf_g$casos, "<br>")

# 1. Paleta de colores. 
pal_colores <- colorNumeric(palette = "YlOrBr", centro_municipio$casos)

m <- leaflet() %>%  # Creamos un objeto leaflet
  #addProviderTiles("CartoDB.Positron") %>%  # Le aniadimos un fondo bonito
  # Fondo blanco
  addPolygons(data = provincia_sf_g,
              fillColor = "white", fillOpacity  = 1.0,
              stroke  = TRUE, 
              color = "gray", #color de la línea de contorno
              weight = 0.2, 
              smoothFactor = 0.5,
              group = "Registro de fianzas") %>% 
  
  addCircleMarkers(data = centro_municipio,
                   lng = ~lng, lat  = ~lat,
                   radius = ~sqrt(casos)/2, #Area
                   fillColor = pal_colores(centro_municipio$casos),
                   fillOpacity = .4,
                   stroke = TRUE,
                   color = "gray", 
                   weight = 1,
                   #Ventana emergente cuando se pica un elementos
                   popup = popup, 
                   
                   #Etiqueta cuando pasa el cursor sobre un poligono
                   label = str_glue("{municipio_sf_g$nombre} - Casos: {municipio_sf_g$casos}"),
                   
                   #highlight cuando pasas el cursor
                   # highlightOptions = highlightOptions(color = "blue", weight = 2,
                   #                                     bringToFront = TRUE),
                   group = "Registro de fianzas") %>% 
  
  addLegendCustom(colors = pal_colores(sizes_originales), 
                  labels = labels, 
                  sizes = sizes_pix) 


m  

#  Con el paquete leaflegend ####
# El resultado no es optimo porque los tamaños de la leyenda no pueden ser
# especificados, no los puedes indicar, los calcula con la función pretty

#https://roh.engineering/posts/2021/05/map-symbols-and-size-legends-for-leaflet/

library("leaflegend")
availableShapes()  # Muestra los simbolos disponibles

leaflet() %>%  # Creamos un objeto leaflet
  
  addSymbolsSize(data = centro_municipio,
                 values = ~casos,
                 lat = ~lat, 
                 lng = ~lng,
                 shape = 'circle',
                 color = "gray", 
                 strokeWidth = 1,
                 fillColor = ~pal_colores(casos),
                 opacity = .4,
                 baseSize = .8) %>% 
  
  addLegendSize(
    data = centro_municipio,
    values =  ~casos,
    #values = c(10,100,1000,5000),
    #color = 'black',
    pal = pal_colores,
    title = 'Nº de testigos',
    shape = 'circle',
    orientation = 'vertical',
    opacity = .4,
    #fillOpacity = 1,
    breaks = 3,
    baseSize = 0.8,
    position = 'bottomright')








# Hacer otro con 4 o 5 tamaños de bolas?
# 



# Hacer facet con mapas leaflet??? NO HACER, HACER UN SHINY app















