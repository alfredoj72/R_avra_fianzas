# paquetes y directorio de trabajo ----
#instala y carga los paquetes necesarios
paquetes_necesarios = c("MASS","tmap","sf","tidyverse","leaflet","ggplot2") #"sp"

for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}

rm(paq, paquetes_necesarios)



rm(list =ls())
#load("datos3.RData")
load(file = "./datos_output/datos_para_analisis_2022.RData")
Fianzas_viviendas <- datos_para_analisis_2022[[1]]
# Fianzas_viviendas$x <- Fianzas_viviendas$coorx_25830
# Fianzas_viviendas$y <- Fianzas_viviendas$coory_25830
# 
# write.csv(Fianzas_viviendas, file = "datos.csv", row.names = FALSE)
# 
# ruta_salida <- "nombre_capa_puntos.shp"
# 
# # Guardar la capa de puntos como Shapefile
# st_write(Fianzas_viviendas, dsn = ruta_salida)


###########################################################################
#Elaboración de mapa de densidad con ggplot2
# ggplot(data= Fianzas_viviendas) + geom_sf(alpha = 0.1)
# 
# ggplot() + geom_sf(data = Fianzas_viviendas, size = 0.001, alpha = 0.2)+
#   stat_density2d(aes(x=Fianzas_viviendas$coorx_25830 , y=Fianzas_viviendas$coory_25830,
#                      fill = after_stat(level), alpha = after_stat(level)),
#                  geom = "polygon") +
#   scale_fill_gradient(low = "yellow", high = "red")+
#   theme_bw()


###########################################################################
# Defincición de función para el calculo del mapa de calor

calcula_mapa_calor <- function(datos, num_cortes){
  
  # Calcular la capa de densidades
  xmin <- min(datos$coorx_25830)
  ymin <- min(datos$coory_25830)
  xmax <- max(datos$coorx_25830)
  ymax <- max(datos$coory_25830)
  
  densidades <- kde2d(datos$coorx_25830, datos$coory_25830, n = 100, 
                      lims = c(xmin-1000,xmax+1000,ymin-1000,ymax+1000) )
  #image(densidades,col= colorRampPalette(c("white", "red"))(1250))
  
  # Obtener los contornos de densidad
  contornos <- contourLines(densidades, nlevels = num_cortes)
  
  # Convertir los contornos en una capa sf de líneas
  lineas <- lapply(contornos, function(contorno) {
    coords <- cbind(contorno$x, contorno$y)
    st_linestring(coords)
  })
  
  capa_lineas <- st_sfc(lineas, crs = 25830)
  
  # Crear un data frame con atributos de densidad
  atributos <- data.frame(densidad = unlist(lapply(contornos, function(contorno) contorno$level)))
  
  # Crear la capa sf con las líneas y los atributos
  # capa_sf <- st_sf(data = atributos, geometry = capa_lineas)
  
  # Visualizar la capa de líneas
  # plot(capa_sf)
  
  # Convertir la capa de líneas en una capa de polígonos
  capa_poligonos_sfc <- st_polygonize(capa_lineas)
  # capa_poligonos_sfc <- st_filter(capa_poligonos_sfc, st_length(capa_poligonos_sfc) > 0)
  
  #  veros_poligonos <- st_collection_extract(capa_poligonos_sfc, "POLYGON")
  
  # Componer la capa de poligonos con los atributos de las líneas
  capa_poligonos_sf <- st_sf(atributos, capa_poligonos_sfc)
  
  # Visualizar la capa de polígonos
  return(capa_poligonos_sf)
}

###########################################################################

# Definición de datos de entrada

datos_entrada <- Fianzas_viviendas
# datos_entrada <- Fianzas_viviendas %>% filter(cod_ine == "11012")
# datos_entrada <- Fianzas_viviendas %>% filter(grepl("^04\\d{3}", cod_ine))

cortes = 125

###########################################################################

# Elaboración de mapa de calor

# capa_mia <- calcula_mapa_calor(datos = datos_entrada, num_cortes = 5)
# plot(capa_mia)
# 
# capa_mia <- calcula_mapa_calor(datos = datos_entrada, num_cortes = 25)
# plot(capa_mia)

mapa_andalucia <- calcula_mapa_calor(datos = datos_entrada, num_cortes = cortes)
#plot(mapa_andalucia)


###########################################################################
# # Crear mapa con tmap
# library(tmap)
# 
# # Crear un mapa base con una capa WMS de OpenStreetMap
# mapa <- tm_basemap(server = "OpenStreetMap")
# # Escribir en la linea de comandos providers$ y se ven todos los proveedores
# 
# # Añadir una capa WMS de otra fuente
# url_wms <- "https://ows.terrestris.de/osm/service?"
# capa_wms <- tm_wms(url = url_wms, layers = "OSM-WMS", name = "Capa WMS")
# mapa_con_wms <- mapa + capa_wms
# 
# # Ver el mapa con la capa WMS
# tmap_mode("view")
# mapa_con_wms

###########################################################################

# Presentación del mapa de calor con leaflet 
mapa_andalucia$densidad <- mapa_andalucia$densidad * 1000000000000
mapa_aux <- tm_shape(mapa_andalucia) + 
  tm_fill(col="densidad") + 
  tm_legend(title = "Intensidad Alquileres", show = FALSE)

mapa_aux

mapa <- tmap_leaflet(mapa_aux,
                     options = leafletOptions(attributionControl = FALSE)) %>%
  addWMSTiles("http://www.callejerodeandalucia.es/servicios/base/wms?",
              # layers = c("contexto_andalucia", "nucleos_poblacion", "batimetria"), 
              layers = c("CDAU_base"), 
              options = WMSTileOptions(format = "image/jpeg",
                                       transparent = FALSE),
              attribution = "Callejero Digital de Andalucía Unificado.IECA",
              group = "CDAU") %>%
  addLayersControl(baseGroups = c("Base CDAU"),
                   options = layersControlOptions(collapsed = TRUE))

mapa

###########################################################################
# Salvado de mapa de calor como shape

st_write(mapa_andalucia, "andalucia_125cortes.shp", delete_layer = TRUE)





################################################################3

#profundización https://rpubs.com/jmourglia/495290
#añadir puntos con valor 


# LO SIGUIENTE ES MUYYY LENTO
# mapa2 <- tmap_leaflet(mapa_aux,
#                       options = leafletOptions(attributionControl = FALSE)) %>%
#   # addWMSTiles("http://www.callejerodeandalucia.es/servicios/base/wms?",
#   #             # layers = c("contexto_andalucia", "nucleos_poblacion", "batimetria"), 
#   #             layers = c("CDAU_base"), 
#   #             options = WMSTileOptions(format = "image/jpeg",
#   #                                      transparent = FALSE),
#   #             attribution = "Callejero Digital de Andalucía Unificado.IECA",
#   #             group = "CDAU") %>%
#   addMarkers(data=datos_entrada,
#              lng=~coorx_25830, lat=~coory_25830, 
#              label=~numero_documento,
#              popup = paste0("Nombre:", as.character(datos_entrada$numero_documento)),
#              clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
#              labelOptions = labelOptions(noHide = F,
#                                          direction = 'auto'))
# mapa2

# Mirar https://rpubs.com/Edimer/673994 
# interesante lo de añadir mapas de calor 


library(leaflet.extras) 



datos_entrada <- Fianzas_viviendas

# datos_entrada <- datos_entrada %>% filter (cod_ine == "41102")

# Transformar la capa de puntos a CRS 4326
datos_entrada_t <- st_transform(datos_entrada, crs = 4326)

# Obtener las coordenadas de latitud y longitud en dos campos nuevos
datos_entrada_t$latitud <- st_coordinates(datos_entrada_t)[, "Y"]
datos_entrada_t$longitud <- st_coordinates(datos_entrada_t)[, "X"]



heat_map <- leaflet() %>% addTiles() %>% 
  addHeatmap(data = datos_entrada_t, 
             lat = ~latitud, 
             lng = ~longitud ,
             minOpacity = 0.00001,
             max = 0.5,
             radius = 15,
             blur = 0)

heat_map

# creo funcion para ver muchas combinaciones de parámetros

f <- function(b, r, min, maxi) {
  titulo <- paste("blur = ", b, "; radio = ", r, "; min = ", min, "; max = ",maxi)
  cat(titulo)
  mapa <- leaflet() %>%
    addTiles() %>%
    addHeatmap(data = datos_entrada_t, 
               lat = ~latitud, 
               lng = ~longitud,
               minOpacity = min,
               max = maxi,
               radius = r,
               blur = b)
  
  return(mapa)
}
f(b= 20, r=15, min =0.005, maxi = 1)
f(b= 25, r=15, min =0.0000001, maxi = 1)


# Crear todas las combinaciones posibles de los valores de b y r
combinations <- expand.grid(blur_p = c(10), #  c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
                            r_p = c(10), # c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
                            max_p = c(0.4, 0.5, 0.6), # c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                            min_p = c(0.001, 0.05, 0.1, 0.2, 0.3, 0.4 ))

# Crear una lista para almacenar los mapas generados
heatmap_maps <- list()

# Recorrer todas las combinaciones y hacer las llamadas a la función
for (i in 1:nrow(combinations)) {
  b <- combinations$b[i]
  r <- combinations$r[i]
  ma <- combinations$max_p[i]
  mi <- combinations$min_p[i]
  
  heatmap_maps[[i]] <- f(b = b, r = r, min = mi, maxi = ma)
  cat("\nPresiona Enter para continuar...")
  
  print(heatmap_maps[[i]])
  respuesta <- readline()
}




##########################################################################
#hacer mapa con ggmap no me gusta demasiado el resultado final.
library(ggplot2)
library(ggmap)
library(RColorBrewer)

datos <- datos_entrada_t

xmin <- min(datos$longitud)-1
ymin <- min(datos$latitud)-1
xmax <- max(datos$longitud)+1
ymax <- max(datos$latitud)+1
map_bounds <- c(left = xmin, bottom = ymin, right = xmax, top = ymax)
coords.map <- get_stamenmap(map_bounds, zoom = 7, maptype = "terrain-background")
ggmap::ggmap(coords.map)
coords.map <- ggmap(coords.map, extent="device", legend="none")
coords.map
coords.map <- coords.map + stat_density2d(data=datos, aes(x=longitud, y=latitud, fill=..level.., alpha=..level..), geom="polygon")
coords.map
coords.map <- coords.map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
coords.map
coords.map <- coords.map + geom_point(data=datos, aes(x=longitud, y=latitud), fill="red", shape=21, alpha=0.8)
coords.map
coords.map <- coords.map + theme_bw()
coords.map

ggsave(filename="./coords.png")




#######################################################################
# Hacer el mapa de otra forma
# library("leaflet")            # Librería para crear mapas interactivos
 library("data.table")         # Librería para trabajar con datos tipo 'data.table'
# library("sp")                 # Librería para datos espaciales
# library("rgdal")              # Librería para leer y escribir datos espaciales en diferentes formatos
# library("KernSmooth")         # Librería para métodos de suavizado de kernel
# library("magrittr") 

datos <- datos_entrada_t
kde <- KernSmooth::bkde2D(data.table(datos)[ , list(longitud, latitud)],
                          bandwidth=c(.15, .15), gridsize = c(1000,1000))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
spgons_sf <- st_as_sf(spgons)

spgons_sf <- st_set_crs(spgons_sf,  st_crs(4326))


## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, alpha = 0.7)[LEVS]) %>%
  addWMSTiles("http://www.callejerodeandalucia.es/servicios/base/wms?",
              # layers = c("contexto_andalucia", "nucleos_poblacion", "batimetria"), 
              layers = c("CDAU_base"), 
              options = WMSTileOptions(format = "image/jpeg",
                                       transparent = FALSE),
              attribution = "Callejero Digital de Andalucía Unificado.IECA",
              group = "CDAU") 


st_write(spgons_sf, "andalucia_cortes.shp", delete_layer = TRUE)

















#cargar capas pota, distritos censales, secciones censales

# Seleccionar el atributo "campo" de la capa de polígonos
capa_poligonos_seleccionados <- capa_poligonos %>% 
  select(campo)

# Realizar la unión espacial solo con el atributo seleccionado
capa_puntos_con_campo <- st_join(capa_puntos, capa_poligonos_seleccionados)










# Mirar las posibilidades de ggdensity 
# https://jamesotto852.github.io/ggdensity/





















