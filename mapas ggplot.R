#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse", "readxl") # c( "flextable","classInt") 
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

#actualizar a algo que vaya a usar
theme_ams_map <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_light(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      complete = FALSE
    )
}

tema_ams <- theme_bw() + theme(text = element_text(family = "Asap-Bold", color = "#25636e"), 
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), 
                               plot.caption=element_text(hjust=1,size=9,colour="grey30"),
                               plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
                               plot.title=element_text(size=12,face="bold", color = "red"),
                               axis.text.x = element_text(family = "Asap-Bold", color = "grey40"),
                               axis.text.y = element_text(family = "Asap-Bold", color = "grey40"), 
                               #legend.position = "none" # Removemos la leyenda. 
)

mapa + theme_ams_map()
mapa + theme_bw()
mapa
mapa + bbc_style()
mapa + tema_ams
mapa + theme_gppr()
mapa + theme_classic()
mapa + theme_linedraw()

# Si no existe el archivo que contiene las capas con los atributos, lo crea
if (!file.exists("datos_output/datos_para_mapas.Rdata")) {
  # Ejecuta el script para crear las capas y añadir los campos
  crea_capas_y_campos()
} 

# Carga las capas
load(file = "datos_output/datos_para_mapas.Rdata")


## Con ggplot2 ####

# ggplot() +
#   geom_sf(data = municipio_sf, fill="white", color="grey85") +
#   geom_sf(data = provincia_sf, fill="transparent", color="grey5") +
#   theme_void()
# 
# ggplot() +
#   geom_sf(data = municipio_sf, 
#           aes(fill= casos),
#           color="grey") +
#   theme_void()

max_valor <- max(provincia_sf$casos)
mapa <-
ggplot() +
  geom_sf(data =  provincia_sf, # provincia_sf   POTA_sf
          aes(fill= casos),
          color="grey") +
  theme_void()+
  # Aquí aplicamos la escala de colores
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1,
                       #breaks=c(0,500,3000,6000), 
                       guide = "legend",
                       name="Número de casos",
                       na.value = "transparent",
                       #n = 4,
                       limits = c(0,max_valor))+
   labs(
    title = "Registro de Fianzas de Alquiler",
    subtitle = "Número de testigos",
    caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    legend.position = c(0.8, 0.12)
  ) 


max_valor <- max(municipio_sf$casos, na.rm = TRUE)

ggplot() +
  geom_sf(data = municipio_sf,
          aes(fill= casos),
          color="grey") +
  geom_sf(data = provincia_sf, fill="transparent", color="grey5") +
  theme_void()+
                 # Aquí aplicamos la escala de colores
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1,  # Los colores más claros se muestran para valores más bajos
                       breaks=c(10,100,1000,4000), # valores para los colores de la leyenda
                       guide = "legend",  #indica que se muestre la leyenda
                       name="Número de casos", #Nombre que aparece en la leyenda
                       na.value = "transparent", # los valores NA los deja transparentes
                       n = 4,  #numero de colores en la leyenda
                       limits = c(10,max_valor) # indica el valor más bajo y más alto a los que asigna color (NA es el más alto)
                       ) +
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
    
    legend.position = c(0.8, 0.22)
  ) # +
 # facet_wrap(~ provincia)

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
  mutate(point_within = st_point_on_surface(geometry)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  st_as_sf()


centro_municipio_s_sf <- municipio_sf %>% 
  mutate(point_within = st_point_on_surface(geometry)) %>% 
  as.data.frame() %>%  # Convierte a data.frame, los campos de geometria no tienen efect
  select(-geometry) %>%    # Elimina el campo con la geometría de polígonos
  st_as_sf()            #Le crea geometria pero ahora usando los puntos

centro_municipio_s_sf <- centro_municipio_s_sf %>% 
  mutate(x=st_coordinates(centro_municipio_s_sf)[,1],  #calcula las coordenadas de cada punto
         y=st_coordinates(centro_municipio_s_sf)[,2]) 

  #st_drop_geometry()
  

# ggplot() +
#   geom_sf(data = provincia_sf, fill="white", color="grey") +
#   geom_sf(data = centro_provincia_s_sf, color="red") +
#     theme_void()
# 
# ggplot() +
#   geom_sf(data = provincia_sf, fill="white", color="grey") +
#   geom_sf(data = centro_municipio_s_sf, color="red") +
#   theme_void()

ggplot() +
  geom_sf(data = provincia_sf,
          fill= "white",
          color="grey") +
  geom_sf(data = centro_provincia_s_sf, 
          aes (size = casos,color = casos, alpha=casos)
  )+
  theme_void()+
  scale_size(name="Nº casos",
             range=c(6,25))+

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
                          range=c(0.7, .9))+
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




# Combinar varios zoom del mismo mapa en un solo gráfico.
# esto es SUPERLENTO

g <- purrr::map(barrios_sf$municipio,
                function(x) {
                  ggplot() +
                    geom_sf(data = filter(barrios_sf, municipio == x)) +
                    guides(fill = FALSE) +
                    ggtitle(x)
                })

g2 <- cowplot::plot_grid(plotlist = g) 

print(g2)



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



