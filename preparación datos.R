##########################################################################
# Prepara para el análisis,
# Solo procesa la tabla de viviendas que casan con 1 vivienda
# Realiza varias acciones: genera campos calculados, FILTRA los registros
# válidos para el análisis, añade campos de POTA y secciones censales y crea factores
# Usa solo los registros que casan con 1 vivienda
# Elimina los registros considerados errores
preparacion_datos_old <- function(datos_entrada){
  
  # Tomo el dataframe que voy a utilizar
  Fianzas_viviendas <- datos_entrada[["Fianzas_casan_1_vivienda"]]
  
  # # CREO CAMPO precio por m2
  # Fianzas_viviendas$renta_m2 <- Fianzas_viviendas$importe_de_la_renta /
  #   Fianzas_viviendas$stotalocal_14
  # # CREO CAMPO
  # # proporcion existente entre la superficie de la vivienda y la de la parcela
  # Fianzas_viviendas$tasa_superf <- Fianzas_viviendas$stotalocal_14 / 
  #   Fianzas_viviendas$sup_parcela
  # # CREO CAMPO superficie por número de habitaciones
  # Fianzas_viviendas <- Fianzas_viviendas %>%
  #   mutate(superf_hab = stotalocal_14/num_habitaciones)
  # 
  # # PASAR TODAS LAS COORDENADAS A HUSO 30
  # # Los datos que nos pasa el IECA a cada parcela le pone coordendas en el huso
  # # que le corresponde al municipio. Voy a pasarlas todas a huso 30 para ello
  # # convierto el dataframe en un objeto sfc y obtengo las coordenadas
  # 
  # # Separar los registros con coorx mayor de 630000 (tienen srs 25829)
  # Fianzas_viviendas_25830 <- Fianzas_viviendas %>% filter(coorx <= 630000)
  # Fianzas_viviendas_25829 <- Fianzas_viviendas %>% filter(coorx > 630000)
  # 
  # # Crear las capas sf para cada grupo
  # capa_sf_25830 <- st_as_sf(Fianzas_viviendas_25830, coords = c("coorx", "coory"), crs = 25830)
  # capa_sf_25829 <- st_as_sf(Fianzas_viviendas_25829, coords = c("coorx", "coory"), crs = 25829)
  # 
  # # Transformar las coordenadas de la capa en srs 25829 a srs 25830
  # capa_sf_25829_transformada <- st_transform(capa_sf_25829, crs = 25830)
  # 
  # # Unir las capas en una sola capa sf
  # Fianzas_viviendas <- rbind(capa_sf_25830, capa_sf_25829_transformada)
  # 
  # # Obtengo las coordenadas a partir de los elementos de la capa
  # coords_25830 <- st_coordinates(Fianzas_viviendas)
  # 
  # # Agregar las coordenadas al dataframe de la capa
  # Fianzas_viviendas$coorx_25830 <- coords_25830[, 1]
  # Fianzas_viviendas$coory_25830 <- coords_25830[, 2]
  # 
  # rm(Fianzas_viviendas_25829, Fianzas_viviendas_25830, capa_sf_25829_transformada,
  #    coords_25830, capa_sf_25829, capa_sf_25830)
  # 
  
  # Verificar el SRS de la capa sf final
  #print(st_crs(capa_sf_final))
  
  # ELIMINACION DE REGISTROS CONSIDERADOS NO VALIDOS PARA EL ANALISIS DE
  # EVOLUCION DE LAS FIANZAS RECOGIDAS EN EL REGISTRO DE FIANZAS
  
  # Eliminar los registros cuyos de tipologia de construcción no esté entre los
  # correctos
  borrados1 <- Fianzas_viviendas %>%
    filter(!tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131")) %>%
    nrow()
  print(paste("El número de registros eliminados por disponer tipo de construcción no válido es",borrados1))
  
  Fianzas_viviendas <- Fianzas_viviendas %>%
    filter(tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131"))
  
  # Eliminar los registros en los que el codigo INE asignado según la localización
  # de la parcela catatral (cod_ine) sea distinto del asignado por AVRA (codigo_ine)
  #
  # primero tengo que arreglar los codigos de AVRA a los que le falta el 0
  # indicar a paco que esto debería venir arreglado en la tabla que me pasa el
  # tb indicar que hay rc con menos de 20 dígitos que debería cribar
  
  # Cambiar el campo cod_ine agregando un "0" por delante cuando la longitud sea 4
  Fianzas_viviendas <- Fianzas_viviendas %>% 
    mutate(codigo_ine = ifelse(nchar(codigo_ine) == 4, 
                               paste0("0", codigo_ine),
                               codigo_ine))
  
  borrados2 <- Fianzas_viviendas %>%
    filter(codigo_ine != cod_ine) %>%
    nrow()
  
  print(paste("El número de registros eliminados por incongruencia entre el código de municipio asignado por AVRA y el código de municipio correspondiente a la referencia catastral es",borrados2))
  
  Fianzas_viviendas <- Fianzas_viviendas %>%
    filter(codigo_ine == cod_ine)
  
  #Eliminar registros cuya superficie sea inferior a 25.5 metros cuadrados
  
  borrados3 <- datos %>%
    filter(stotalocal_14 <= 25.5) %>%
    nrow()
  
  print(paste("El número de registros eliminados por tamaño inferior al exigido para vivienda es",borrados3))
  
  datos <- datos %>% 
    filter(stotalocal_14 > 25.5)
  
  
  # un tipo de error que hay que evitar es el de referencias catastrales de avra
  # que en catastro casen pero se refieran a bienes inmuebles que contengan más de 
  # 1 vivienda. El IECA ha separado los bienes inmuebles en más de una vivienda 
  # cuando no existe division horizontal en dicho inmueble, pero si existe división
  # horizontal no separa sus bienes inmuebles en más viviendas auqnue en la tabla 14
  # tengan más direcciones. Estas situaciones deben ser en las que asociado a una
  # renta de vivienda "normal" encontremos una vivienda excesivamente grande.
  # analicemos pues el tamaño en relación a la renta de alquiler, es decir la renta_m2
  
  # 5907109TG3450N0001XY es un ejemplo para AVRA de un bien inbmueble que debería
  # contener varias viviendas en los datos IECA Catastro y solo contiene 1.
  
  # Sería deseable que en el modelo 806 se recogiera un check para indicar si el
  # alquiler se refiere a la totalidad del biene inmueble para el que se recoge
  # la referencia catastral o solo a una parte.
  
  # El corte por los percentiles 0.025 y 0.975 lo he elegido porque obtengo
  # valores de la variable asumibles como correctos y tiene una interpretación fácil:
  #   Quedarte con el 95% central de observaciones
  
  # Q025 <- quantile(Fianzas_viviendas$renta_m2, 0.025, na.rm = TRUE)
  # Q975 <- quantile(Fianzas_viviendas$renta_m2, 0.975, na.rm = TRUE)
  # 
  # borrados <-  Fianzas_viviendas %>%
  #    filter(renta_m2 < Q025 | renta_m2 > Q975) %>%
  #    nrow()
  # 
  # print(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados))
  # 
  # Fianzas_viviendas <- Fianzas_viviendas %>%
  #   filter(renta_m2 > Q025 & renta_m2 < Q975)
  # 
  # rm(Q025, Q975, borrados)
  
  # Hago una versión actualizada en la que se eliminan todos los registros cuya
  # referencia catastral queda en cualquiera de las 2 colas
  
  Q025 <- quantile(Fianzas_viviendas$renta_m2, 0.025, na.rm = TRUE)
  Q975 <- quantile(Fianzas_viviendas$renta_m2, 0.975, na.rm = TRUE)
  
  ref_cat_a_borrar <-  Fianzas_viviendas %>%
    filter(renta_m2 < Q025 | renta_m2 > Q975) %>% 
    distinct(referencia_catastral)
  
  
  borrados4 <- inner_join(Fianzas_viviendas, 
                          ref_cat_a_borrar,
                          by = c("referencia_catastral" = "referencia_catastral")) %>% 
    nrow()
  
  
  Fianzas_viviendas <- anti_join(Fianzas_viviendas, 
                                 ref_cat_a_borrar,
                                 by = c("referencia_catastral" = "referencia_catastral"))
  
  print(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados3))
  
  tabla_borrados <- data.frame(
    Tipo = c("Tipo de construcción no válido",
             "Incongruencia Cod_INE AVRA y Cod_INE recuperado de referencia catastral",
             "Superficie de inferior a mínima permitida en vivienda",
             "Valores extremos en renta/m2 ", "Sirven para el análisis"),
    casos = c(borrados1, borrados2, borrados3,borrados4,nrow(Fianzas_viviendas))
  )
  
  rm(ref_cat_a_borrar, borrados1, borrados2, borrados3, borrados4, Q025, Q975)
  
  
  # INCORPORACIÓN DE INFORMACIÓN DE ADSCRIPCIÓN DE CADA MUNICIPIO A 
  # LA ESTRUCTURA DEL POTA
  # Se añaden los campos que indican la jerarquía del municipio y
  # la unidad territorial a la que pertenece el municipio
  
  adsc_mun <- read_excel("datos_aux/adscripcion municipal definitiva CON POB Y SUP.xls",
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.unidad_territorial)
  
  Fianzas_viviendas <-  left_join(Fianzas_viviendas, 
                                  adsc_mun, 
                                  by = c("cod_ine" = "codigo_municipal") )
  rm(campos, codigo, adsc_mun)
  
  # INCORPORACIÓN DE INFORMACIÓN DEL BARRIO Y LA SECCION EN LA QUE SE ENCUENTRA
  # CADA VIVIENDA  
  
  # Se añaden los campos nombre del barrio y nombre del distrito de la capa de barrios
  # Según el IECA la capa de Barrios contiene una delimitación aproximada de los
  # Distritos y Barrios de las grandes ciudades andaluzas
  # Tambien se añade el codigo de sección censal de la capa de secciones censales
  # que si recoge todos los municipios de Andalucia
  
  
  # Leer los shapefiles
  Barrios_sf <- st_read(dsn = "datos_aux/13_24_BarrioUrbano.shp", quiet = TRUE)
  Secciones_sf <- st_read(dsn = "datos_aux/13_27_SeccionCensal.shp", quiet = TRUE)
  
  Barrios_sf <- Barrios_sf %>% 
    mutate(barrio.nombre = nombre,
           barrio.distrito = distrito) %>% 
    select(barrio.nombre, barrio.distrito)
  
  Secciones_sf <- Secciones_sf %>% 
    mutate(seccion.codigo = codigo) %>% 
    select(seccion.codigo)
  
  Fianzas_viviendas <- st_join(Fianzas_viviendas, Barrios_sf)
  Fianzas_viviendas <- st_join(Fianzas_viviendas, Secciones_sf)
  
  rm(Barrios_sf, Secciones_sf)
  # FALTA SUSTITUIR NA POR VALORES Y DAR ORDEN A LOS FACTORES
  #Declaro cuáles son los factores. OJO FALTAN ALGUNOS AUN
  
  
  
  
  
  Fianzas_viviendas <- Fianzas_viviendas %>%
    mutate (sexo_arrendador = factor(sexo_arrendador),
            tipo_persona_arrendador = factor(tipo_persona_arrendador),
            tipo_entidad_arrendador = factor(tipo_entidad_arrendador),
            sexo_arrendatario = factor(sexo_arrendatario),
            nacionalidad_arrendatario = factor(nacionalidad_arrendatario),
            municipio_806 = factor(municipio_806),
            provincia_806 = factor(provincia_806),
            cod_postal_806 = factor(cod_postal_806),
            tipo_de_arrendamiento = factor(tipo_de_arrendamiento),
            tip_const4d_14 = factor(tip_const4d_14),
            cod_ine = factor(cod_ine),
            tipviv = factor(tipviv),
            seccion.codigo = factor(seccion.codigo),
            pota.unidad_territorial = factor(pota.unidad_territorial),
            pota.jerarquia = factor(pota.jerarquia),
            barrio.nombre = factor(barrio.nombre),
            barrio.distrito = factor(barrio.distrito))
  
  # Añado a los resultados la tabla con todo el proceso, el resumen de datos borrados
  # y la tabla de datos originales
  originales  <- datos_entrada[["originales"]]
  return(list(Fianzas_viviendas = Fianzas_viviendas,
              tabla_borrados = tabla_borrados,
              originales = originales))
}


preparacion_datos <- function(datos_entrada){
  datos <- datos_entrada[["Fianzas_casan_1_vivienda"]]
  
  # Eliminar los registros cuyos de tipologia de construcción no esté entre los
  # correctos
  borrados1 <- datos %>%
    filter(!tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131")) %>%
    nrow()
  print(paste("El número de registros eliminados por disponer tipo de construcción no válido es",borrados1))
  
  datos <- datos %>%
    filter(tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131"))
  
  
  datos <- datos %>% 
    mutate(codigo_ine = ifelse(nchar(codigo_ine) == 4, 
                               paste0("0", codigo_ine),
                               codigo_ine))
  
  borrados2 <- datos %>%
    filter(codigo_ine != cod_ine) %>%
    nrow()
  
  print(paste("El número de registros eliminados por incongruencia entre el código de municipio asignado por AVRA y el código de municipio correspondiente a la referencia catastral es",borrados2))
  
  datos <- datos %>%
    filter(codigo_ine == cod_ine)
  
  
  #Eliminar registros cuya superficie sea inferior a 25.5 metros cuadrados
  
  borrados3 <- datos %>%
    filter(stotalocal_14 <= 25.5) %>%
    nrow()
  
  print(paste("El número de registros eliminados por tamaño inferior al exigido para vivienda es",borrados3))
  
  datos <- datos %>% 
    filter(stotalocal_14 > 25.5)
  
  
  # Alquileres que corresponden a Referencias Catastrales que se repiten
  # Si una referencia catastral se repite es porque se ha alquilado más de una
  # ven en un año o porque en realidad la referencia catastral corresponde
  # a un bien inmueble que contiene muchas viviendas no separadas individualmente
  
  # Tras un análisis considerando los factores:
  # * se repite la referencia catastral en los registros de avra
  # * existe o no división horizontal 
  # * tipología de vivienda
  
  # Decido eliminar del estudio (1/2):
  # Cuando se repite la ref cat  y no hay división horizontal elimino todos los 
  # registros en los que tampoco el IECA ha encontrado viviendas ( o dicho de otra
  # forma, salvo aquellos en los que el IECA ha detectado la existencia de más de 
  # una vivienda)
  
  
  datos <- datos %>% 
    mutate(n_bi = coalesce(n_bi, n_bi_sindh))
  
  borrados4 <- datos %>%
    filter(avra_rc_repet > 1 & n_bi == 1 & nviv == 1) %>%
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de RC repetidas y referidas edificios sin división horizontal y viviendas única",borrados4))
  
  datos <- datos %>% 
    filter(!(avra_rc_repet > 1 & n_bi == 1 & nviv == 1))
  
  # Decido eliminar del estudio (2/2):
  # Cuando se repite la ref cat, hay división horizontal y la tipología de 
  # vivienda es colectiva y sin embargo el número de viviendas es 1 (es decir, 
  # aunque hay división horizontal solo se ha detectado una vivienda y el resto es
  # garaje o comercial, etc ). En eses caso elimino dichos registros
  
  borrados5 <- datos %>%
    filter(avra_rc_repet > 1 & 
             n_bi > 1 &
             substr(tip_const4d_14, 1, 3) == "011" &
             nviv == 1) %>% 
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de RC repetidas ubicadas en edificios de viviendas colectivas con división horizontal pero con solo una vivienda es",borrados5))
  
  datos <- datos %>% 
    filter(!(avra_rc_repet > 1 & 
               n_bi > 1 &
               substr(tip_const4d_14, 1, 3) == "011" &
               nviv == 1))
  
  
}