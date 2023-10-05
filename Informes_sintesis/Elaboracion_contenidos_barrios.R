

# --------------------------------------------------------------------------
# Fabrico los listados de barrios por cada municipio
t <- genera_resumen_barrio(datos)

# t_flex <- resumen_a_flex(t)  # listado único de todos los municipios seguidos
# t_flex

lista_tablas_por_municipio <- split(t, ~ nombre)
lista_flex_barrios <- lapply(lista_tablas_por_municipio, resumen_a_flex)

# --------------------------------------------------------------------------
# Fabrico los mapas de barrios por municipio con ggplot2
datos_barrio <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre", 
                                                 "barrio.codigo", "barrio.nombre"),
                                        list(character(0))) %>% 
  filter(!is.na(barrio.nombre)) %>% 
  filter(n >=10)


etiquetas <- c("Menos 2","2 a menos de 4","4 a menos de 6",
               "6 a menos de 8","8 a menos de 10","10 o más")

datos_barrio <- datos_barrio %>% 
  mutate(f.renta_m2 = cut(datos_barrio$preciom2_M,
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

barrios_sf <-  left_join(barrios_sf, 
                         datos_barrio, 
                         by = c("barrio.codigo" = "barrio.codigo") )


lista_barrios_por_municipio <- split(barrios_sf, ~municipio )
lista_mapas_barrios <- lapply(lista_barrios_por_municipio, crear_mapa_barrio)
legend <- cowplot::get_legend(lista_mapas_barrios[[1]])
# ggdraw(plot_grid(legend))


lista_mapas_barrios <- lapply(lista_mapas_barrios, function(mapa) {
  mapa + theme(legend.position = "none")
})
