crea_capas_y_campos <- function(){
  
  # Leer capas en formato shapefiles 
  # ## Carga de capas shp ubicadas en directorio local
  provincia_sf <- st_read(dsn = "./datos_aux/13_01_Provincia.shp", quiet = TRUE)
  municipio_sf <- st_read(dsn = "./datos_aux/13_01_TerminoMunicipal.shp", quiet = TRUE)
  
  barrios_sf <- st_read(dsn = "./datos_aux/13_24_BarrioUrbano.shp", quiet = TRUE)
  
  # añado un campo identificador a la capa de barrios que no lo tiene
  barrios_sf <- barrios_sf %>% 
    group_by(cod_mun, municipio, nombre, distrito ) %>% 
    summarize(.groups = "drop") %>% 
    mutate(barrio.codigo = row_number(),
           barrio.nombre = nombre,
           barrio.distrito = distrito) %>% 
    select(cod_mun, municipio, barrio.codigo, barrio.nombre, barrio.distrito)
  
  # Añade al nombre del barrio el del distrito en aquellos casos que hay 2
  # barrios con el mismo nombre pero distinto distrito
  barrios_sf <- barrios_sf %>% 
    group_by(cod_mun, barrio.nombre) %>% 
    mutate(rep = n(),
           barrio.nombre = ifelse(rep > 1, 
                                  paste0(barrio.nombre,". ", barrio.distrito),
                                  barrio.nombre),
           rep = NULL
    )
  
  # hago un "dissolve" de barrios atendiendo al distrito
  # y añado cod_mun y municipio para que la capa resultado contenga dichos campos
  union_barrios_sf <- barrios_sf %>% 
    group_by(barrio.distrito, cod_mun, municipio) %>%
    summarize(.groups = "drop") 
  
  
  secciones_sf <- st_read(dsn = "./datos_aux/13_27_SeccionCensal.shp", quiet = TRUE)
  
  secciones_sf <- secciones_sf %>% 
    mutate(seccion.codigo = codigo,
           seccion.distrito = substr(codigo,1,7)) %>% 
    select(cod_mun, municipio, seccion.codigo, seccion.distrito)
  
  distritos_sf <- secciones_sf %>%
    group_by(seccion.distrito, cod_mun, municipio ) %>%
    summarize(.groups = "drop") 
  
  # La capa del POTA no tiene un campo código y los nombres de los ambitos
  # territoriales no coinciden con los del dataframe datos que procede de la 
  # tabla de adscripciones de municipios al POTA.
  # Por tanto si agrupo la información de los alquileres atendiendo al campo
  # nombre de la unidad territorial no lo puedo casar con la capa.
  # Es decir, no me sirve la capa de unidades del POTA del DERA. Tengo que
  # construirme una capa de unidades POTA con los nombres de unidades territoriales
  # que tengo en datos, para ello hago un "dissolve" de los municipios
  # En R, el dissolve se consigue simplemente agrupando en un objeto de tipo sf
  
  
  # obtengo la tabla de municipios con la adscripción al pota
  adsc_mun <- read_excel("datos_aux/adscripcion municipal definitiva CON POB Y SUP.xls",
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  #Añade un cero y se queda con los 5 últimos dígitos
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.unidad_territorial)
  
  # añado a los municipios la información de adscripción al POTA  
  municipio_sf <-  left_join(municipio_sf, 
                             adsc_mun, 
                             by = c("cod_mun" = "codigo_municipal") )
  
  rm(campos, codigo)
  
  # disuelvo los municipios un unidades territoriales del POTA
  POTA_sf <- municipio_sf %>% group_by(pota.unidad_territorial) %>% summarize() 
  
  
  # plot(provincia_sf)
  
  # # Simplificar los polígonos para acelerar los proceso
  # municipio_sf <- st_simplify(municipio_sf, dTolerance = 1)
  # provincia_sf <- st_simplify(provincia_sf, dTolerance = 1)
  # # No lo uso porque no veo diferencia de tiempo y sin embargo si se producen algunos
  # # cambios en el comportamiento de la capa ya que pasa de MULTIPOLYGON a GEOMETRY
  
  # Obtener resúmenes de datos para todos los niveles de información
  
  datos_provincia <- datos %>%
    group_by(provincia_806) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_provincia <- datos_provincia %>% 
    mutate(codigo = c("04","11","14","18","21","23","29","41"))
  
  datos_POTA <- datos %>%
    group_by(pota.unidad_territorial) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_municipio <- datos %>%
    group_by(codigo_ine) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  browser()
  
  datos_barrios <- datos %>%
    group_by(barrio.codigo) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_barrios_tipolog <- datos %>%
    group_by(barrio.codigo, f.tipolog ) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop") %>% 
    pivot_wider(names_from = f.tipolog,
                values_from = c(casos, mediana_renta_m2, 
                                mediana_renta, mediana_superf),
                names_glue = "{f.tipolog}_{.value}",)
  
  
  datos$tamanio <- ifelse(datos$stotalocal_14 >= 90, "Grande","Pequeña")
  datos_barrios_tamanio <- datos %>%
    group_by(barrio.codigo, tamanio ) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop") %>% 
    pivot_wider(names_from = tamanio,
                values_from = c(casos, mediana_renta_m2, 
                                mediana_renta, mediana_superf),
                names_glue = "{tamanio}_{.value}",)
  datos$tamanio <- NULL
  
  datos_secciones <- datos %>%
    group_by(seccion.codigo) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_barrios_union <- datos %>%
    group_by(barrio.distrito) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_distrito <- datos %>%
    group_by(seccion.distrito) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  
  
  
  # Añadir los datos numéricos a las capas para poder representarlos en mapas
  
  provincia_sf <- provincia_sf %>% 
    left_join (datos_provincia, by = c("codigo" = "codigo")) # %>% 
  # mutate(etiqueta = paste(provincia,"\n",casos))
  
  
  municipio_sf <- municipio_sf %>% 
    left_join (datos_municipio, by = c("cod_mun" = "codigo_ine"))    %>%
    mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0
  
  POTA_sf <- POTA_sf %>% 
    left_join (datos_POTA, by = c("pota.unidad_territorial" = "pota.unidad_territorial"))%>%
    mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0
  
  barrios_sf <- barrios_sf %>% 
    left_join (datos_barrios, by = c("barrio.codigo" = "barrio.codigo"))%>%
    left_join (datos_barrios_tipolog, by = c("barrio.codigo" = "barrio.codigo")) %>% 
    left_join (datos_barrios_tamanio, by = c("barrio.codigo" = "barrio.codigo")) %>% 
    mutate(casos = coalesce(casos, 0)) 
  
  secciones_sf <- secciones_sf %>% 
    left_join (datos_secciones, by = c("codigo" = "seccion.codigo"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  
  
  barrios_union_sf <- barrios_union_sf %>% 
    left_join (datos_barrios_union, by = c("distrito" = "barrio.distrito"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  distritos_sf <- distritos_sf %>% 
    left_join (datos_distrito, by = c("distrito" = "seccion.distrito"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  
  rm(datos_provincia, datos_municipio, datos_POTA, datos_barrios, 
     datos_barrios_union, datos_secciones, datos_distrito)
  
  
  save(provincia_sf, municipio_sf, POTA_sf, barrios_sf,
       secciones_sf, barrios_union_sf, distritos_sf,
       file = "datos_output/capas_con_datos_para_mapas.Rdata")
  
}


#> ############## FUNCION CARGAR CAPAS   ########################
#> Lee la información de las capas shape de provincias, municipios, barrios y
#> secciones. Se renombra los campos de dichas capas y se queda solo con los
#> códigos de identificación. A continuación crea las capas de unión de barrios,
#> distritos censales y unidades del pota.

Pasar_capas_shp_a_R <- function(){
  
  # Leer capas en formato shapefiles 
  # ## Carga de capas shp ubicadas en directorio local
  provincia_sf <- st_read(dsn = here("datos_aux","13_01_Provincia.shp"), quiet = TRUE)
  
  municipio_sf <- st_read(dsn = here("datos_aux","13_01_TerminoMunicipal.shp"), quiet = TRUE)
  barrios_sf <- st_read(dsn = here("datos_aux","13_24_BarrioUrbano.shp"), quiet = TRUE)
  secciones_sf <- st_read(dsn = here("datos_aux","13_27_SeccionCensal.shp"), quiet = TRUE)
  
  Secciones_sf <- Secciones_sf %>% 
    mutate(seccion.codigo = codigo,
           seccion.distrito = substr(codigo,1,7)) %>% 
    select(seccion.codigo, seccion.distrito)
  
  # añado un campo identificador a la capa de barrios que no lo tiene
  barrios <- barrios_sf %>% 
    group_by(cod_mun, nombre, distrito ) %>% 
    summarize(.groups = "drop") %>% 
    mutate(barrio.codigo = row_number(),
           barrio.nombre = nombre,
           barrio.distrito = distrito) %>% 
    select(barrio.codigo, barrio.nombre, barrio.distrito)
  
  # Añade al nombre del barrio el del distrito en aquellos casos que hay 2
  # barrios con el mismo nombre pero distinto distrito
  Barrios_sf <- Barrios_sf %>% 
    group_by(cod_mun, barrio.nombre) %>% 
    mutate(rep = n(),
           barrio.nombre = ifelse(rep > 1, 
                                  paste0(barrio.nombre,". ", barrio.distrito),
                                  barrio.nombre),
           rep = NULL
    )
  
  
  # hago un "dissolve" de barrios atendiendo al distrito
  # y añado cod_mun y municipio para que la capa resultado contenga dichos campos
  barrios_union_sf <- barrios_sf %>% 
    group_by(distrito, cod_mun, municipio) %>%
    summarize(.groups = "drop") 
  
  distritos_sf <- secciones_sf %>%
    group_by(distrito, cod_mun, municipio ) %>%
    summarize(.groups = "drop") 
  
  # La capa del POTA no tiene un campo código y los nombres de los ambitos
  # territoriales no coinciden con los del dataframe datos que procede de la 
  # tabla de adscripciones de municipios al POTA.
  # Por tanto si agrupo la información de los alquileres atendiendo al campo
  # nombre de la unidad territorial no lo puedo casar con la capa.
  # Es decir, no me sirve la capa de unidades del POTA del DERA. Tengo que
  # construirme una capa de unidades POTA con los nombres de unidades territoriales
  # que tengo en datos, para ello hago un "dissolve" de los municipios
  # En R, el dissolve se consigue simplemente agrupando en un objeto de tipo sf
  
  
  # obtengo la tabla de municipios con la adscripción al pota
  adsc_mun <- read_excel(here("datos_aux","adscripcion municipal definitiva CON POB Y SUP.xls"),
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  #Añade un cero y se queda con los 5 últimos dígitos
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.unidad_territorial)
  
  # añado a los municipios la información de adscripción al POTA  
  municipio_sf <-  left_join(municipio_sf, 
                             adsc_mun, 
                             by = c("cod_mun" = "codigo_municipal") )
  
  rm(campos, codigo)
  
  # disuelvo los municipios un unidades territoriales del POTA
  POTA_sf <- municipio_sf %>% group_by(pota.unidad_territorial) %>% summarize() 
  
  
  # plot(provincia_sf)
  
  # # Simplificar los polígonos para acelerar los proceso
  # municipio_sf <- st_simplify(municipio_sf, dTolerance = 1)
  # provincia_sf <- st_simplify(provincia_sf, dTolerance = 1)
  # # No lo uso porque no veo diferencia de tiempo y sin embargo si se producen algunos
  # # cambios en el comportamiento de la capa ya que pasa de MULTIPOLYGON a GEOMETRY
  
  
  save(provincia_sf, municipio_sf, POTA_sf, barrios_sf,
       secciones_sf, barrios_union_sf, distritos_sf,
       file = here("datos_output","capas_para_mapas.Rdata"))
  
}