######## DEFINICION DE FACTORES ############
# FACTORES CORRESPONDIENTES A CAMPOS DE LA TABLA ORIGINAL DE DATOS DE AVRA

# Si convierto los NA en un nivel del factor comenzará a ordenarse como el resto
# de los niveles, si lo dejo como NA se van siempre al último de la lista.

# datos <- datos %>%
#   mutate (tipo_persona_arrendador = as.character(tipo_persona_arrendador),
#           sexo_arrendador = as.character(sexo_arrendador))

# Para obtener los resultados de datos para BADEA es preciso contar con todos
# los municipios, todas las secciones censales y todos los distritos censales
# Para ello voy a definir los factores con todos ellos y así podré obtenerlos
# en los resultados aunque no tengan datos asociados
# Los factores de secciones censales, distritos censales, unidades POTA y barrios
# lo hago en la función preparacion_datos al tiempo que añado la información.
# El factor de los municipios lo hago aqui

# Leo los municipios de la capa de municipios de datos_aux. En los script de 
# creación de mapas leo los datos de WMF. Aquí lo hago distinto.


Municipios_sf <- st_read(dsn = "datos_aux/13_01_TerminoMunicipal.shp", quiet = TRUE)
Municipios <- st_drop_geometry(Municipios_sf) %>% 
  select(cod_mun, nombre) %>% 
  distinct(cod_mun, nombre)

datos <- datos %>% 
  mutate(cod_ine = factor(cod_ine,
                          levels = Municipios$cod_mun, 
                          labels = Municipios$nombre)
  )
rm(Municipios_sf, Municipios)

datos <- datos %>%
  #mutate (tipo_persona_arrendador = replace_na(tipo_persona_arrendador,"NEspec")) %>% # no hay casos
  mutate (tipo_persona_arrendador = factor(tipo_persona_arrendador, 
                                           levels = c("F", "J"), 
                                           labels = c("Física", "Jurídica")),
          tipo_entidad_arrendador = as.factor(tipo_entidad_arrendador),
          sexo_arrendador = factor(sexo_arrendador,
                                   levels = c("M", "V"),
                                   labels = c("Mujeres", "Hombres")),
          sexo_arrendatario = factor(sexo_arrendatario,
                                     levels = c("M", "V"),
                                     labels = c("Mujeres", "Hombres")),
          nacionalidad_arrendatario = factor(nacionalidad_arrendatario),
          tipo_de_arrendamiento = case_when(     #otra forma podría ser usando na_if()
            tipo_de_arrendamiento == "AMUEBLADO"       ~ "Amueblado", 
            tipo_de_arrendamiento == "SIN AMUEBLAR"    ~ "Sin Amueblar",
            TRUE                                 ~ NA_character_) , 
          tipo_de_arrendamiento = factor(tipo_de_arrendamiento),
          provincia_806 = factor (provincia_806, 
                                  labels = c("Almería", "Cádiz","Córdoba","Granada","Huelva","Jaén","Málaga","Sevilla"))
  )

datos <- datos %>%
  mutate (  tipo_de_arrendamiento = case_when(     #otra forma podría ser usando na_if()
    tipo_de_arrendamiento == "AMUEBLADO"       ~ "Amueblado", 
    tipo_de_arrendamiento == "SIN AMUEBLAR"    ~ "Sin Amueblar",
    TRUE                                 ~ NA_character_) , 
    tipo_de_arrendamiento = factor(tipo_de_arrendamiento))


anyos <-cut(datos$duracion_contrato_años,
            breaks = c(0,1,3,5,max(datos$duracion_contrato_años, na.rm = TRUE)),
            right = TRUE,    #Intervalos cerrados por la derecha
            include.lowest = TRUE,  # Para que incluya el valor máximo
            dig.lab = 10)  #dígitos usados sin que se muestren en formato científico

etiquetas <- c("1 año",">1 - 3 años",">3 - 5 años",">5 años")

datos <- datos %>% mutate(f.durac_contrato = factor(anyos, labels =etiquetas))


#max <- max(datos$num_habitaciones,  na.rm = TRUE)
habit_agrupa <- 4  # A partir de 4 habitaciones los agrupa en una sola barra
cortes <- c(seq(0, habit_agrupa, 1),max(datos$num_habitaciones, na.rm = T))
etiquetas <- c(seq(1,habit_agrupa,1),glue("{habit_agrupa+1} o más"))
n_hab <-  cut(datos$num_habitaciones, 
              breaks = cortes,
              rigth = TRUE,
              include.lowest = TRUE)

datos <- datos %>% mutate(f.hab = factor(n_hab, labels =etiquetas))


rentas <-cut(datos$importe_de_la_renta,
             breaks = c(0,300,500,700,900,1200,max(datos$importe_de_la_renta, na.rm = TRUE)),
             right = FALSE,    #Intervalos cerrados por la izquierda
             include.lowest = TRUE,  # Para que incluya el valor máximo
             dig.lab = 10)  #dígitos usados sin que se muestren en formato científico

etiquetas <- c("<300€","300€ - 499€","500€ - 699€",
               "700€ - 899€","900€ - 1.199€",">=1.200€")

datos <- datos %>% mutate(f.renta_alq = factor(rentas, labels =etiquetas))