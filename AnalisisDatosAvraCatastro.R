# Usa solo los registros que casan con 1 vivienda
# Elimina los registros considerados errores
# Realiza un analisis descriptivo de las variables.


paquetes_necesarios = c("dplyr", "ggplot2","classInt","sf","readxl") #c("readxl","RPostgres","sf","dplyr","writexl")

for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}


rm(list =ls())

#Cargo los datos y me quedo con las viviendas que han casado con una vivienda
#del catastro (1 registro avra - 1 vivienda catastro)
load("datos2.RData")

#Me quedo solo con la tabla que voy a analizar
Fianzas_viviendas <- Fianzas_casan_1_vivienda

rm(avra_catastro, avra_datos_originales, Fianzas_casan_distintas_viviendas,
   Fianzas_casan_distintas_viviendas_case, Fianzas_no_casan_catastro,
   Fianzas_casan_1_vivienda)



# CREO CAMPO precio por m2
Fianzas_viviendas$renta_m2 <- Fianzas_viviendas$importe_de_la_renta /
  Fianzas_viviendas$stotalocal_14
# CREO CAMPO
# proporcion existente entre la superficie de la vivienda y la de la parcela
Fianzas_viviendas$tasa_superf <- Fianzas_viviendas$stotalocal_14 / 
  Fianzas_viviendas$sup_parcela
# CREO CAMPO superficie por número de habitaciones
Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate(superf_hab = stotalocal_14/num_habitaciones)

# PASAR TODAS LAS COORDENADAS A HUSO 30
# Los datos que nos pasa el IECA a cada parcela le pone coordendas en el huso
# que le corresponde al municipio. Voy a pasarlas todas a huso 30 para ello
# convierto el dataframe en un objeto sfc y obtengo las coordenadas


# Separar los registros con coorx mayor de 630000 (tienen srs 25829)
Fianzas_viviendas_25830 <- Fianzas_viviendas %>% filter(coorx <= 630000)
Fianzas_viviendas_25829 <- Fianzas_viviendas %>% filter(coorx > 630000)

# Crear las capas sf para cada grupo
capa_sf_25830 <- st_as_sf(Fianzas_viviendas_25830, coords = c("coorx", "coory"), crs = 25830)
capa_sf_25829 <- st_as_sf(Fianzas_viviendas_25829, coords = c("coorx", "coory"), crs = 25829)

# Transformar las coordenadas de la capa en srs 25829 a srs 25830
capa_sf_25829_transformada <- st_transform(capa_sf_25829, crs = 25830)

# Unir las capas en una sola capa sf
Fianzas_viviendas <- rbind(capa_sf_25830, capa_sf_25829_transformada)

coords_25830 <- st_coordinates(Fianzas_viviendas)

# Agregar las coordenadas al dataframe de la capa
Fianzas_viviendas$coorx_25830 <- coords_25830[, 1]
Fianzas_viviendas$coory_25830 <- coords_25830[, 2]

rm(Fianzas_viviendas_25829, Fianzas_viviendas_25830, capa_sf_25829_transformada,
   coords_25830, capa_sf_25829, capa_sf_25830)


# Verificar el SRS de la capa sf final
#print(st_crs(capa_sf_final))







# ELIMINACION DE REGISTROS CONSIDERADOS NO VALIDOS PARA EL ANALISIS DE
# EVOLUCION DE LAS FIANZAS RECOGIDAS EN EL REGISTRO DE FIANZAS

# Eliminar los registros cuyos de tipologia de construcción no esté entre los
# correctos
borrados <- Fianzas_viviendas %>%
  filter(!tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131")) %>%
  nrow()
cat(paste("El número de registros eliminados por disponer tipo de construcción no válido es",borrados))

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

borrados <- Fianzas_viviendas %>%
  filter(codigo_ine != cod_ine) %>%
  nrow()

cat(paste("El número de registros eliminados por incongruencia entre el código de municipio asignado por AVRA y el código de municipio correspondiente a la referencia catastral es",borrados))

Fianzas_viviendas <- Fianzas_viviendas %>%
  filter(codigo_ine == cod_ine)

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
# cat(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados))
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


borrados <- inner_join(Fianzas_viviendas, 
                       ref_cat_a_borrar,
                       by = c("referencia_catastral" = "referencia_catastral")) %>% 
               nrow()


Fianzas_viviendas <- anti_join(Fianzas_viviendas, 
                                     ref_cat_a_borrar,
                                     by = c("referencia_catastral" = "referencia_catastral"))

cat(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados))
rm(ref_cat_a_borrar, borrados, Q025, Q975)


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
Barrios_sf <- st_read(dsn = "datos_aux/13_24_BarrioUrbano.shp")
Secciones_sf <- st_read(dsn = "datos_aux/13_27_SeccionCensal.shp")

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

save.image("datos3.RData")

# Exportar el dataframe a Excel
write_xlsx(Fianzas_viviendas, "Fianzas_viviendas_filtrado.xlsx")



# La superficie asignada a cada vivienda por el resumen de datos IECA Catastro
# se calcula, dentro del fichero tipo 14 de construcciones, sumando la superficie
# total del local de cada una de las construcciones que conforman cada vivienda.
# Este dato se toma del campo “84_stl” de la hoja 14.








rm(list =ls())
load("datos3.RData")

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Indice
# Primero voy a hacer una descripción breve de las variables
# A continuación más detallada de cada variable
# Por último haremos un análisis cruzado variables

# Descripción breve de variables

# La descripción de variables será distinta en función de si la variable es
# tipo numerico, texto o factor.




cat ("\n El número total de registros para el análisis es ",
     nrow(Fianzas_viviendas), "\n")


Fianzas_viviendas <- Fianzas_viviendas %>% select(-num_repeticiones)

# Función para obtener la descripción de una columna de tipo texto
get_text_description <- function(column) {
  distinct_values <- length(unique(column))
  values <- unique(column)
  num_na <- sum(is.na(column))
  result <- list(
    distinct_values = distinct_values,
    values = values,
    num_na = num_na
  )
  return(result)
}

# Función para obtener la descripción de una columna numérica
get_numeric_description <- function(column) {
  result <- list(
    summary = summary(column),
    mode = Mode(column),
    IQR = IQR(column, na.rm = TRUE),
    sd = sd(column),
    deciles <- quantile(na.omit(column), probs = seq(0.1, 0.9, by = 0.1)),
    num_na = sum(is.na(column))
  )
  return(result)
}
# Función para calcular la moda de una columna numérica
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Obtener descripción de cada columna del dataframe
for (col in names(Fianzas_viviendas)) {
  column <- Fianzas_viviendas[[col]]
  
  cat("Columna '", col, "':\n")
  
  if (is.character(column) | is.factor(column)) {
    # Columna de texto
    description <- get_text_description(column)
    cat("Número de valores distintos:", description$distinct_values, "\n")
    if (description$distinct_values <= 10) {
      cat("Valores distintos:\n")
      value_counts <- table(column)
      for (i in seq_along(value_counts)) {
        cat(value_counts[i], ":", names(value_counts)[i], "\n")
      }
      if (description$num_na > 0) {
        cat(description$num_na, ": NA's \n")
      }
    }
  } else if (is.numeric(column)) {
    # Columna numérica
    description <- get_numeric_description(column)
    cat("Resumen estadístico:\n")
    print(description$summary)
    cat("Deciles:\n")
    print(description$deciles)
    cat("Moda:", description$mode, "\n")
    cat("Recorrido intercuantílico (IQR):", description$IQR, "\n")
    cat("Desviación estándar:", description$sd, "\n")
  }
  
  cat("\n")
}


#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Analisis variable a variable

# Defino funciones para el analisis individualizado de variables
# Función para obtener la descripción de una columna de tipo texto
get_text_description <- function(column) {
  distinct_values <- length(unique(column))
  values <- unique(column)
  num_na <- sum(is.na(column))
  result <- list(
    distinct_values = distinct_values,
    values = values,
    num_na = num_na
  )
  return(result)
}

# Función para obtener la descripción de una columna numérica
get_numeric_description <- function(column) {
  result <- list(
    summary = summary(column),
    mode = Mode(column),
    IQR = IQR(column, na.rm = TRUE),
    sd = sd(column),
    deciles = quantile(na.omit(column), probs = seq(0.1, 0.9, by = 0.1)),
    num_na = sum(is.na(column))
  )
  return(result)
}
# Función para calcular la moda de una columna numérica
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Definicion de funcion 
Resumen_basico <- function(column) {
  if (is.character(column) | is.factor(column)) {
    # Columna de texto
    description <- get_text_description(column)
    cat("Número de valores distintos:", description$distinct_values, "\n")
    if (description$distinct_values <= 10) {
      cat("Valores distintos:\n")
      value_counts <- table(column)
      for (i in seq_along(value_counts)) {
        cat(value_counts[i], ":", names(value_counts)[i], "\n")
      }
      if (description$num_na > 0) {
        cat(description$num_na, ": NA's \n")
      }
    }
  } else if (is.numeric(column)) {
    # Columna numérica
    description <- get_numeric_description(column)
    cat("Resumen estadístico:\n")
    print(description$summary)
    cat("Deciles:\n")
    print(description$deciles)
    cat("Moda:", description$mode, "\n")
    cat("Recorrido intercuantílico (IQR):", description$IQR, "\n")
    cat("Desviación estándar:", description$sd, "\n")
  }
  
  cat("\n")
  #  }
  #  return(doble)
}

# Comienzo el analisis de cada variable
# 
# 
# 
#.-.-.-.-.-.-
#codigo_expediente__rue
Resumen_basico(Fianzas_viviendas$codigo_expediente__rue)

#.-.-.-.-.-.-
#numero_documento
Resumen_basico(Fianzas_viviendas$numero_documento)

#.-.-.-.-.-.-
#nif_cif_arrendador_anonimizado
Resumen_basico(Fianzas_viviendas$nif_cif_arrendador_anonimizado)

#.-.-.-.-.-.-
#sexo_arrendador
Resumen_basico(Fianzas_viviendas$sexo_arrendador)
ggplot(Fianzas_viviendas, aes(x = sexo_arrendador),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Sexo del arrendador",
       y = "Frecuencia", title = "Sexo del arrendador")+
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", c("V" = "Varón", "M" = "Mujer"))) +
  theme_minimal()

#.-.-.-.-.-.-
#tipo_persona_arrendador
Resumen_basico(Fianzas_viviendas$tipo_persona_arrendador)
# Crear el gráfico de barras con etiquetas de altura
ggplot(Fianzas_viviendas, aes(x = tipo_persona_arrendador)) +
  geom_bar(fill = "cyan", color = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Tipo persona arrendador", 
       y = "Frecuencia",
       title = "Tipo persona arrendador") +
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", c("F" = "Física", "J" = "Jurídica"))) +
  theme_minimal()

#.-.-.-.-.-.-
#tipo_entidad_arrendador
Resumen_basico(Fianzas_viviendas$tipo_entidad_arrendador)
ggplot(Fianzas_viviendas, aes(x = substr(tipo_entidad_arrendador,1,1))) +
  geom_bar(fill = "cyan", color = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(x = "Tipo entidad arrendador", 
       y = "Frecuencia",
       title = "Tipo entidad arrendador") +
  theme_minimal()+
  theme(axis.text.x = element_text( vjust = 0.1, hjust = 0.3))+
  scale_x_discrete(labels = function(x) ifelse(is.na(x), "No esp.", x))

#.-.-.-.-.-.-
#sexo_arrendatario
Resumen_basico(Fianzas_viviendas$sexo_arrendatario)
ggplot(Fianzas_viviendas, aes(x = sexo_arrendatario),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Sexo del arrendatario",
       y = "Frecuencia", title = "Sexo del arrendatario")+
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", c("V" = "Varón", "M" = "Mujer"))) +
  theme_minimal()

#.-.-.-.-.-.-
#nacionalidad_arrendatario
Resumen_basico(Fianzas_viviendas$nacionalidad_arrendatario)
ggplot(Fianzas_viviendas, aes(x = nacionalidad_arrendatario),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Nacionalidad del arrendatario",
       y = "Frecuencia", title = "Nacionalidad del arrendatario")+
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", x)) +
  theme_minimal()

#.-.-.-.-.-.-
#referencia_catastral
Resumen_basico(Fianzas_viviendas$referencia_catastral)

#.-.-.-.-.-.-
#provincia_806
Resumen_basico(Fianzas_viviendas$provincia_806)
ggplot(Fianzas_viviendas, aes(x = provincia_806)) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Provincia Vivienda",
       y = "Frecuencia", title = "Provincia")+
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", x)) +
  theme_minimal()

#.-.-.-.-.-.-
#provincia_806
frecuencias <- table(Fianzas_viviendas$provincia_806)
df_frecuencias <- data.frame(valor = names(frecuencias), frecuencia = frecuencias) %>%
  arrange(desc(frecuencia.Freq))
ggplot(df_frecuencias, 
       aes(x = reorder(frecuencia.Var1, -frecuencia.Freq), 
           y = frecuencia.Freq)) +
  geom_bar(fill = "cyan", color = "steelblue", stat = "identity") +
  geom_text(aes(label = frecuencia.Freq), vjust = 1.3, size = 3) +
  labs(x = "Provincia", y = "Frecuencia", title = "Gráfico de barras") +
  theme_minimal()
rm(frecuencias,df_frecuencias)

#.-.-.-.-.-.-
#duracion_contrato_años
Resumen_basico(Fianzas_viviendas$duracion_contrato_años)
ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "Duración contrato", 
                   y = duracion_contrato_años),
               fill = "steelblue",
               color = "black") +
  labs(x = "", y = "Años") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = duracion_contrato_años)) +
  geom_histogram(bins = 39, fill = "steelblue", color = "cyan") +
  labs(x = "", y = "Años") +
  theme_minimal() 


ggplot(Fianzas_viviendas, aes(x = duracion_contrato_años)) +
  geom_histogram(breaks = seq(0,40), 
                 fill = "steelblue",
                 color = "cyan") +
  labs(x = "", y = "Años") +
  theme_minimal() 


#Alternativo con indicación de la amplitud de los intervalos
ggplot(Fianzas_viviendas,
       aes(x = cut(duracion_contrato_años, breaks = seq(0, 40, 1), include.lowest = TRUE))) +
  geom_bar(fill = "steelblue", color = "cyan", stat = "count", na.rm = FALSE ) +
  labs(x = "Duración contrato", y = "Años") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(xlim = c(0, 10))

#.-.-.-.-.-.-
#Fecha de devengo
#esta variable es de tipo fecha, se describe de otra forma al resto
Hmisc::describe(Fianzas_viviendas$fecha_devengo)
#hay 365 fechas distintas
Hmisc::describe(lubridate::year(Fianzas_viviendas$fecha_devengo))
#Efectivamente todas las fecha de devengo son de 2022

#.-.-.-.-.-.-
#Número de habitaciones
Resumen_basico(Fianzas_viviendas$num_habitaciones)
ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "", 
                   y = num_habitaciones),
               fill = "steelblue",
               color = "black") +
  labs(x = "", y = "Nº habitaciones " ,title = "Número de habitaciones") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = num_habitaciones)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "cyan") +
  labs(x = "Nº Habitaciones", y = "", title = "Número de habitaciones") +
  theme_minimal() 

ggplot(Fianzas_viviendas, aes(x = num_habitaciones)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "steelblue", color = "cyan") +
  geom_density(adjust = 6, color = "red") +
  labs(x = "Nº Habitaciones", y = "", title = "Número de habitaciones") +
  theme_minimal()

#.-.-.-.-.-.-
#Tipo de arrendamiento
Resumen_basico(Fianzas_viviendas$tipo_de_arrendamiento)
ggplot(Fianzas_viviendas, aes(x = tipo_de_arrendamiento),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Tipo de arrendamiento",
       y = "Frecuencia", title = "Tipo de arrendamiento")+
  scale_x_discrete(labels = function(x) 
    ifelse(is.na(x), "No especificado", x)) +
  theme_minimal()

#.-.-.-.-.-.-
#Importe _de_la_renta
Resumen_basico(Fianzas_viviendas$importe_de_la_renta)
ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "", 
                   y = importe_de_la_renta),
               fill = "steelblue",
               color = "black") +
  labs(x = "", y = "Importe de la renta" ,title = "Importe de la renta") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = importe_de_la_renta)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "cyan") +
  labs(x = "Importe de la renta", y = "€/mes", title = "Importe de la renta") +
  theme_minimal()
    
ggplot(Fianzas_viviendas, aes(x = importe_de_la_renta)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "steelblue", color = "cyan") +
  geom_density(adjust = 2, color = "red") +
  labs(x = "Importe de la renta", y = "", title = "Importe de la renta") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 2000))













