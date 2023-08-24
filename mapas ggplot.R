#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse","flextable","ggplot2","tmap") # c( "ggplot2","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)

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

#actualizar a algo que vaya a usar
theme_ams_map <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_bw(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      complete = TRUE
    )
}

#### Cargar las capas y añadir datos   ####

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
  left_join (datos_municipio, by = c("cod_mun" = "codigo_ine"))    %>%
  mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0

save.image(file = "datos_para_mapas.Rdata")


## Con ggplot2 ####

rm(list =ls())
load(file = "datos_para_mapas.Rdata")

ggplot() +
  geom_sf(data = municipio_sf, fill="white", color="grey85") +
  geom_sf(data = provincia_sf, fill="transparent", color="grey5") +
  theme_void()



ggplot() +
  geom_sf(data = municipio_sf, 
          aes(fill= casos),
          color="grey") +
  theme_void()

ggplot() +
  geom_sf(data = provincia_sf,
          aes(fill= casos),
          color="grey") +
  theme_void()+
  # Aquí aplicamos la escala de colores
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1, 
                       guide = "legend",
                       name="Número de casos",
                       na.value = "transparent",
                       n = 5)+
   labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    legend.position = c(0.8, 0.12)
  )

ggplot() +
  geom_sf(data = municipio_sf,
          aes(fill= casos),
          color="grey") +
  geom_sf(data = provincia_sf, fill="transparent", color="grey5") +
  theme_void()+
                                             # Aquí aplicamos la escala de colores
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1, 
                       breaks=c(0,10,100,1000,5000,8000),
                       guide = "legend",
                       name="Número de casos",
                       na.value = "transparent",
                       n = 6,
                       limits = c(10,NA)
                       ) + 
  # scale_fill_viridis(breaks=c(0,2000,3000,5000,6000,8000,9000,10000), 
  #                    name="Número de casos", 
  #                    guide = guide_legend( keyheight = unit(2, units = "mm"),
  #                                          keywidth=unit(4, units = "mm"), 
  #                    label.position = "right",
  #                    title.position = 'left',
  #                    nrow=10) ) +
  labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA)), #color de fondo de todo el gráfico
    #panel.background = element_rect(fill = "#f5f5f2", color = NA),  #color de fondo de la zona del mapa
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)), #color de fondo de la leyenda
    
    # plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    # plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    # plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.8, 0.12)
  )
# Al aplicar facet sobre el mapa no se puede conseguir hacer zoom a la provincia
# en cada uno de ellos. Si se puede resolver usando cowplot https://stackoverflow.com/questions/47678480/mapping-different-states-with-geom-sf-using-facet-wrap-and-scales-free#:~:text=The%20result%20using%20cowplot
# En tmap esto si es posible  sencillo.

# mapa de burbujas con ggplot2

#con ggplot no se pueden pintar burbujar sobre los poligonos. hay que obtener coordenadas
# Calcular centroides
# centro_provincia_sf <- provincia_sf %>% 
#   mutate(centroid = st_centroid(geom)) %>% 
#   as.data.frame() %>% 
#   select(-geom) %>% 
#   st_as_sf()
# 
# centro_municipio_sf <- municipio_sf %>% 
#   mutate(centroid = st_centroid(geom)) %>% 
#   as.data.frame() %>% 
#   select(-geom) %>% 
#   st_as_sf()


# Calcular centroides garantizando que son interiores al poligono
centro_provincia_s_sf <- provincia_sf %>% 
  mutate(point_within = st_point_on_surface(geom)) %>%
  as.data.frame() %>%
  select(-geom) %>%
  st_as_sf()


centro_municipio_s_sf <- municipio_sf %>% 
  mutate(point_within = st_point_on_surface(geom)) %>% 
  as.data.frame() %>%  # Convierte a data.frame, los campos de geometria no tienen efect
  select(-geom) %>%    # Elimina el campo con la geometría de polígonos
  st_as_sf() 

centro_municipio_s_sf <- centro_municipio_s_sf %>% 
  mutate(x=st_coordinates(centro_municipio_s_sf)[,1],
         y=st_coordinates(centro_municipio_s_sf)[,2]) 

  #st_drop_geometry()
  

ggplot() +
  geom_sf(data = provincia_sf, fill="white", color="grey") +
  geom_sf(data = centro_provincia_s_sf, color="red") +
    theme_void()

ggplot() +
  geom_sf(data = provincia_sf, fill="white", color="grey") +
  geom_sf(data = centro_municipio_s_sf, color="red") +
  theme_void()

ggplot() +
  geom_sf(data = provincia_sf,
          fill= "white",
          color="grey") +
  geom_sf(data = centro_provincia_s_sf, 
          aes (size = casos,color = casos, alpha=casos)
  )+
  theme_void()+
  scale_size(name="Nº casos",
             range=c(6,30))+

 # Aquí aplicamos la escala de colores
  scale_color_distiller(name="Nº casos",
                        palette = "YlOrBr", direction = 1, guide = "legend",
                        n = 5) + 
  
  scale_alpha_continuous( name="Nº casos",
                          range=c(0.7, .8))+
  labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    legend.position = c(1.02, 0.25)
    #legend.position = c(0.02, 0.25)
  )

ggplot() +
  geom_sf(data = provincia_sf,
          fill= "white",
          color="grey") +
  geom_sf(data = centro_municipio_s_sf,
          aes (size = casos,color = casos, alpha=casos)
  )+
  theme_void()+
  scale_size(name="Nº casos",
             range=c(2,15))+   #(6,30)
  
  # Aquí aplicamos la escala de colores
  scale_color_distiller(name="Nº casos",
                        palette = "YlOrBr", direction = 1, guide = "legend",
                        n = 5) + 
  
  scale_alpha_continuous( name="Nº casos",
                          range=c(0.7, .8))+
  labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    legend.position = c(1.02, 0.25)
    #legend.position = c(0.02, 0.25)
  )


ggplot() +
  geom_sf(data = arrange(centro_municipio_s_sf,desc(casos)), # se pintan antes las bolas más grandes
          aes (size = casos,color = casos, alpha=casos)
  )+
  theme_void()+
  scale_size(name="Nº casos",
             range=c(2,15))+   #(6,30) rango del tamaño de bola
  
  # Aquí aplicamos la escala de colores
  scale_color_distiller(name="Nº casos",
                        palette = "YlOrBr", direction = 1, guide = "legend",
                        n = 5) + 
  
  scale_alpha_continuous( name="Nº casos",
                          range=c(0.3, .8))+   #rango del porcentaje de transparencia
  labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    legend.position = c(0.02, 0.25)
    #legend.position = c(0.02, 0.25)
  ) +

  geom_sf(data = provincia_sf,     #Pinta las provincias
          fill= "transparent",
          color="grey") +

                                  # y ponemos estiquetas a los 25 municipios con más casos
  ggrepel::geom_text_repel( data = centro_municipio_s_sf %>% 
                     filter(!is.na(casos)) %>% 
                     arrange(casos) %>%
                     tail(25),
                   aes(x=x, y=y, label=nombre), 
                   size=2) 


# Pintar las bolas en los mapas con un filete exterior, para ello se usa el shape 21
# que si dispone de relleno y contorno
# Para ello se usa geom_sf, tambien se podría usar geom_point

ggplot() +
  # geom_point(data = arrange(centro_municipio_s_sf,desc(casos)), # se pintan antes las bolas más grandes
  #            aes (x=x,y=y, size = casos,fill = casos, alpha=casos), shape = 21, color = "saddlebrown"
  # )+
  geom_sf(data = arrange(centro_municipio_s_sf,desc(casos)), # se pintan antes las bolas más grandes
          aes (size = casos,fill = casos, alpha=casos), shape = 21, color = "black"
  )+
  theme_void()+
  scale_size(name="Nº casos",
             range=c(2,15))+   #(6,30) rango del tamaño de bola
  
  # Aquí aplicamos la escala de colores
  scale_fill_distiller(name="Nº casos",
                       palette = "YlOrBr", direction = 1, guide = "legend",
                       n = 5) + 
  
  scale_alpha_continuous( name="Nº casos",
                          range=c(0.3, .8))+   #rango del porcentaje de transparencia
  labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    legend.position = c(0.02, 0.25)
    #legend.position = c(0.02, 0.25)
  ) +
  
  geom_sf(data = provincia_sf,     #Pinta las provincias
          fill= "transparent",
          color="grey") +
  
  # y ponemos estiquetas a los 25 municipios con más casos
  ggrepel::geom_text_repel( data = centro_municipio_s_sf %>% 
                              filter(!is.na(casos)) %>% 
                              arrange(casos) %>%
                              tail(25),
                            aes(x=x, y=y, label=nombre), 
                            size=2.5) 



# Como se indican las escalas (R epidemiologia) https://epirhandbook.com/es/
# scale_AESTHETIC_METHOD().
# 
# La primera parte, scale_(), es fija.
# La segunda parte, la ESTÉTICA, debe ser la estética para la que deseas ajustar
# la escala (_fill_, _shape_, _color_, _size_, _alpha_…) -
#   las opciones aquí también incluyen _x_ e _y_.
# La tercera parte, el MÉTODO
# será _discrete(), continuous(), _date(), _gradient(), o _manual()
# dependiendo del tipo de la columna y de cómo se quiera controlar. 
# Hay otros, pero estos son los más utilizados.



