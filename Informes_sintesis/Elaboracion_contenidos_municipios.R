

# --------------------------------------------------------------------------
# Fabrico el listado por provincias
t <- genera_resumen_provincias(datos)
listado_provincial <- resumen_a_flex(t)
#listado_provincial2 <- DT::datatable(t)
# Fabricar con DT

#para que es runtime:shiny::para que es css:style.css

# --------------------------------------------------------------------------
# Fabrico los listados de municipios por provincia
t_m <- genera_resumen_municipios(datos)

listado_municipios_andalucia <- resumen_a_flex(t_m)  # listado único de todos los municipios seguidos


lista_tablas_por_provincia <- split(t_m, ~ provincia_806)
lista_flex_provincias <- lapply(lista_tablas_por_provincia, resumen_a_flex)



# --------------------------------------------------------------------------
# Fabrico los mapas de municipios regional



datos_provincia <-  genera_cruce_variables(datos, c("anyo", "provincia_806"),
                                           list(character(0))) %>%
  filter(n >=10)

datos_municipio <-  genera_cruce_variables(datos, c("anyo", "provincia_806", 
                                                 "cod_ine"),
                                        list(character(0))) %>%
  filter(n >=10)


etiquetas <- c("Menos 2","2 a menos de 4","4 a menos de 6",
               "6 a menos de 8","8 a menos de 10","10 o más")

datos_provincia <- datos_provincia %>%
  mutate(f.renta_m2 = cut(datos_provincia$preciom2_M,
                          breaks = c(0,2,4,6,8,10, Inf),
                          right = FALSE,    #Intervalos cerrados por la izquierda
                          include.lowest = TRUE,  # Para que incluya el valor máximo
                          dig.lab = 10,
                          labels = etiquetas))

datos_municipio <- datos_municipio %>%
  mutate(f.renta_m2 = cut(datos_municipio$preciom2_M,
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

# A leaflet le tengo que dar las capas en EPSG 4326, sin proyectar.
provincia_sf <-   st_transform(provincia_sf, crs = 4326)
municipio_sf <-   st_transform(municipio_sf, crs = 4326)
centro_provincia <- st_transform(centro_provincia, crs = 4326)
centro_municipio <- st_transform(centro_municipio, crs = 4326)


provincia_sf <-  left_join(provincia_sf,
                           datos_provincia,
                           by = c("provincia" = "provincia_806") )

municipio_sf <-  left_join(municipio_sf,
                            datos_municipio,
                         by = c("cod_mun" = "cod_ine") )


#  Por provincia ####
# 1. Paleta de colores. 
pal_colores <- colorFactor(palette = "YlOrBr", domain = provincia_sf$f.renta_m2)

# 2. Popup 
popup <- paste0("<b>provincia_sf$provincia </b>", "<br>", 
                "<b>Precio: </b>", provincia_sf$preciom2_M, " €/m2 <br>")

mapa_regional_provincias <- leaflet(width = 830, height = 500) %>%  # Creamos un objeto leaflet  #width = 750, height = 450
  addProviderTiles("CartoDB.Positron",
                   options = providerTileOptions(minZoom=3, maxZoom=8)) %>%  # Le aniadimos un fondo bonito
  addPolygons(data = provincia_sf,
              color = "gray", #color de la línea de contorno
              weight = 1,       # ancho del trazo en pixeles
              smoothFactor = 0.5,  # Simplificación de las líneas
              opacity = 1,
              fillColor = pal_colores(provincia_sf$f.renta_m2), #aplica la paleta
              fillOpacity = 0.6,
              
              #Ventana emergente cuando se pica un elementos
              popup = popup, 
              
              #Etiqueta cuando pasa el cursor sobre un poligono
              label = str_glue("{provincia_sf$provincia} - {provincia_sf$preciom2_M} €/m2"),
              
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), #highlight cuando pasas el cursor
              
              layerId = ~provincia_sf$provincia #????
  ) %>% 
  addLegend(position = "bottomleft", pal= pal_colores, 
            values = provincia_sf$f.renta_m2,
            title = "Precio €/m2")


#  Por municipios ####



construir_mapa_municipios <- function(capa) {
  
  # 1. Paleta de colores. 
  pal_colores <- colorFactor(palette = "YlOrBr", 
                             domain = capa$f.renta_m2,
                             na.color = "white")

  popup <- ifelse(is.na(capa$f.renta_m2),
                  str_glue("<b>{capa$nombre}</b><br> - sin datos"),
                  str_glue("<b>{capa$nombre}</b><br>
                          <b>{capa$preciom2_M}</b> €/m2 <br>" )
  )
# 
#   
#   #Preparo la cartela para los municipios que no disponen información de alquiler
#   file.copy("./html_aux/cartela_datos_basicos_un_anyo_nodatos.html",
#             "./html_aux/cartela_datos_basicos_un_anyo_nodatos.txt")
#   
#   html_sin_datos <- readLines("./html_aux/cartela_datos_basicos_un_anyo_nodatos.txt")
#   file.remove("./html_aux/cartela_datos_basicos_un_anyo_nodatos.txt")
#   html_sin_datos <- paste(html_sin_datos, collapse = "\n")
#   
#   # sustituir { por {{ y } por }} para escaparlos
#   html_sin_datos <- gsub("\\{", "{{", html_sin_datos)
#   html_sin_datos <- gsub("\\}", "}}", html_sin_datos)
#   
#   html_sin_datos <- gsub("Nombre", "{capa$cod_mun} - {capa$nombre}", html_sin_datos)
#   
#   
#   #Preparo la cartela para los municipios que SI disponen información de alquiler
#   file.copy("./html_aux/cartela_datos_basicos_un_anyo.html",
#             "./html_aux/cartela_datos_basicos_un_anyo.txt")
#   html_con_datos <- readLines("./html_aux/cartela_datos_basicos_un_anyo.txt")
#   file.remove("./html_aux/cartela_datos_basicos_un_anyo.txt")
#   html_con_datos <- paste(html_con_datos, collapse = "\n")
#   
#   
#   html_con_datos <- gsub("\\{", "{{", html_con_datos)
#   html_con_datos <- gsub("\\}", "}}", html_con_datos)
#   
#   # sustituir los valores por los campos
#   html_con_datos <- gsub("Nombre", "{capa$cod_mun} - {capa$nombre}", html_con_datos)
#   html_con_datos <- gsub("rentam2_M", "{capa$preciom2_M}", html_con_datos)
#   html_con_datos <- gsub("rentam2_p", "[{capa$preciom2_p25} - {capa$preciom2_p75}]", html_con_datos)
#   html_con_datos <- gsub("rentames_M", "{capa$preciomes_M}", html_con_datos)
#   html_con_datos <- gsub("rentames_p", "[{capa$preciomes_p25} - {capa$preciomes_p75}]", html_con_datos)
#   html_con_datos <- gsub("superficie_M", "{capa$superficie_M}", html_con_datos)
#   html_con_datos <- gsub("superficie_p", "[{capa$superficie_p25} - {capa$superficie_p75}]", html_con_datos)
#   
#   popup <- ifelse(is.na(capa$f.renta_m2),
#                   str_glue(html_sin_datos),
#                   str_glue(html_con_datos )
#   )
 
  
  label <- ifelse(is.na(capa$f.renta_m2), 
                  str_glue("{capa$nombre} ({capa$provincia}) - sin datos") ,
                  str_glue("{capa$nombre} ({capa$provincia}) - {capa$preciom2_M} €/m2"))
  
  #coordenadas de contorno
  bounds <- st_bbox(capa) %>% as.character()
  
  mapa <- leaflet(width = 830, height = 500) %>%  # Creamos un objeto leaflet
    addProviderTiles("CartoDB.Positron") %>%  # Le añadimos un fondo bonito
    
    # Fondo blanco
    # addPolygons(data = provincia_sf,
    #             fillColor = "white", fillOpacity  = 1.0,
    #             stroke  = F, smoothFactor = 0.5,
    #             group = "Registro de fianzas") %>% 
    
    # Los municipios con su color
    addPolygons(data = capa,
                stroke = FALSE,
                smoothFactor = 0.5,  # Simplificación de las líneas
                opacity = 1.0,
                fillColor = pal_colores(capa$f.renta_m2), #aplica la paleta
                fillOpacity = 1,
                group = "Registro de fianzas") %>% 
    
    # La etiqueta, la ventana emergente y el resaltado 
    addPolygons(data = capa ,
                color = "gray", #color de la línea de contorno
                weight = 0.1,       # ancho del trazo en pixeles
                smoothFactor = 0.5,  # Simplificación de las líneas
                #opacity = 1.0,
                #fillColor = pal_colores(capa$casos), #aplica la paleta
                fillOpacity = 0,
                
                #Ventana emergente cuando se pica un elementos
                popup = popup, 
                
                #Etiqueta cuando pasa el cursor sobre un poligono
                label = label,
                
                #highlight cuando pasas el cursor
                highlightOptions = highlightOptions(color = "blue", weight = 1,
                                                    bringToFront = TRUE),
                group = "Registro de fianzas") %>%
    
    # La línea de provincias
    addPolylines(data = provincia_sf,
                 color = "gray", #color de la línea de contorno
                 weight = 0.2,       # 
                 fillOpacity  = 0, smoothFactor = 0.5,
                 group = "Registro de fianzas") %>% 
    
    addLegend(position = "bottomleft", pal= pal_colores, opacity =1,
              values = capa$f.renta_m2,
              na.label = "Sin datos",
              title = "Precio €/m2 mes")  %>% 
    
    addLayersControl(overlayGroups = c( "Registro de fianzas"),
                     options = layersControlOptions(collapsed = F)) %>% 
    
    # ajusta el area de visualización al contorno de la capa
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  return(mapa)
  
}



# #crear mapa regional leaflet
# 
# #crear mapa provincial leaflet
# lista_barrios_por_municipio <- split(barrios_sf, ~municipio )



# lista_mapas_barrios <- lapply(lista_barrios_por_municipio, crear_mapa_barrio)
# legend <- cowplot::get_legend(lista_mapas_barrios[[1]])
# # ggdraw(plot_grid(legend))
# 
# 
# lista_mapas_barrios <- lapply(lista_mapas_barrios, function(mapa) {
#   mapa + theme(legend.position = "none")
# })
