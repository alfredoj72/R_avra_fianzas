# Genera la salida enviada a BADEA el 14/11/2023
# Genera microcubos enviado como prueba


if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,writexl,readxl)


# rm(list =ls()) 
# load("./datos_output/datos_para_analisis_2022.RData")
# datos <- datos_para_analisis_2022[["datos"]]
# rm(datos_para_analisis_2022)
# datos <- as.data.frame(datos)
# datos <- st_drop_geometry(datos)
# str(datos)

rm(list =ls()) 
load("./datos_output/datos_para_analisis_todos.RData")
datos_con_geometria <- datos_analisis
datos <- st_drop_geometry(datos_analisis)
rm(datos_analisis)

###################################################################
# 1 ###############################################################
###################################################################
#> Selección de variables a usar y
#> sustitucion de valores NA por No especificado 
#> (previamente en la preparacion de datos se han sustituido los NA
#> que correspodían a Valor no procedente, por ej en sexo arrendador de persona
#> jurídica)

#> o lo hago año a año y meto el campo anyo despues
#> o lo hago uniendo todos los años antes de pasar este scritp
#>  y el campo anyo lo meto siempre con los pivotes.

#datos$anyo <- lubridate::year(datos$fecha_devengo)

# old lista_campos
dimensiones <- c("anyo", "f.durac_contrato","f.renta_alq", "f.renta_m2",
                 "sexo_arrendador", "f.persona_fj", "sexo_arrendatario",
                 "nacionalidad_arrendatario", "tipo_de_arrendamiento",
                 "f.super","f.hab","f.tipolog", "f.antig_bi",
                 "f.tam_pob","pota.jerarquia", 
                 "pota.tipo_unidad","pota.unidad_territorial")


# el anyo es un dimensión especial porque siempre hay que pivotar sobre ella,
# ya que en a BADEA todos los datos contiene rellena la columna de datos
dimensiones_pivotantes <- 
  dimensiones[dimensiones != "anyo"]

territorio <- c("provincia_806","cod_ine","seccion.codigo","seccion.distrito")

medidas <- c("importe_de_la_renta" , "stotalocal_14" , "renta_m2")

# Me quedo con las variables que necesito
datos_para_BADEA <- datos %>% select(all_of(c(dimensiones, territorio, medidas)))

#summary(datos_para_BADEA)

# Paso las variables a formato texto
datos_para_BADEA[c(dimensiones, territorio)] <- 
  lapply(datos_para_BADEA[c(dimensiones, territorio)], as.character)


# Si un campo por el que se agrupa tiene NA este también aparecerá en el resultado
# No deben existir valores NA, de lo contrario se confundirán con los NA
# que salen cuando un campo no se usa como criterio de agrupación. Se deben rellenar
# con "No Especificado"
# Reemplazo los valores NA por "No especificado" en cualquier campo

datos_para_BADEA[is.na(datos_para_BADEA)] <- "No especificado"

###################################################################
# 1B ##############################################################
###################################################################
#> Crear campos de jerarquía
#> Los campos de jerarquía con campos que se usan para algunas variables
#> que en BADEA tienen definida una jerarquia. Ej
#> 00 Ambos sexos
#>   01 Hombres
#>   06 Mujeres
#>   NC No consta
#> 0NP No procede
#>   No procede
#>   Para resolver esta situación, para cada jerarquía creo un campo distinto
#>   Luego haré los cruces de variables cada jerarquia y por último uniré los resultados

# Leemos el fichero que contiene la información de traduccion de categorias
# usadas en el estudio a categorias de BADEA
cambio_modelo_datos <- read_excel("./datos_aux/Modelo datos BADEA.xlsx",
                                  sheet = "Dimensiones",
                                  skip = 11) %>% 
  filter(!is.na(var))

# diccionario para cambiar el contenido de las variables de jerarquia
diccionario_badea <- cambio_modelo_datos %>% 
  mutate(cod = categorias_jerarquia,
         var = paste0(var,"_jer")) %>% 
  filter(!is.na(cod) & !is.na(categ)) %>% 
  select(var,categ,cod)


# Creo las variables jerárquicas
datos_para_BADEA$sexo_arrendador_jer <- datos_para_BADEA$sexo_arrendador
datos_para_BADEA$f.persona_fj_jer <- datos_para_BADEA$f.persona_fj

# Cambio del contenido de la variables de acuerdo al diccionario cargado
datos_para_BADEA <- datos_para_BADEA %>%
  mutate(across(where(is.character), ~ 
                  coalesce(deframe(subset(diccionario_badea, var == cur_column(), 
                                          select= -var))[as.character(.)], as.character(.)
                  )
  )
  )


###################################################################
# 2 ###############################################################
###################################################################
#> Definición de funciones para crear las combinaciones de campos y
#> la generación de las tablas de salida para BADEA


# Calcula las combinaciones de campos desde tamaño 0 hasta n_campos
obten_combinaciones_campos <- function(n_campos, lista_campos) {
  unlist(lapply(0:n_campos, function(n){combn(lista_campos,m=n, simplify = FALSE)} ),
         recursive = FALSE)
}

genera_lineas_BADEA <- function(df, campo_fijo = NULL, combinaciones_campos){
  #browser()
  resultado <- data.frame()
  parciales <- data.frame()
  for (campos  in rev(combinaciones_campos)) {
    if (!is.null(campo_fijo)){
      campos_a_usar <- c(campos, campo_fijo)
    } else {
      campos_a_usar <- campos
    }
    parciales <- df %>%
      select(all_of(campos_a_usar),
             importe_de_la_renta , stotalocal_14 , renta_m2) %>%
      
      #  group_by(across(c(all_of(campos),any_of(campo_fijo)))) %>% 
      
      group_by(across(all_of(campos_a_usar))) %>% 
      
      # en el summarise siguiente es en el que hay que colocar los parámetros
      # que se quieran calcular para cada grupo: n(), mediana, Q25, Q75 de los campos
      summarise(n = n(),
                mediana_precio_mes = median(importe_de_la_renta),
                p25_precio_mes = quantile(importe_de_la_renta, probs = 0.25, type = 4),
                p75_precio_mes = quantile(importe_de_la_renta, probs = 0.75, type = 4),
                mediana_superficie = median(stotalocal_14),
                p25_superficie = quantile(stotalocal_14, probs = 0.25, type = 4),
                p75_superficie = quantile(stotalocal_14, probs = 0.75, type = 4),
                mediana_precio_m2 = median(renta_m2),
                p25_precio_m2 = quantile(renta_m2, probs = 0.25, type = 4),
                p75_precio_m2 = quantile(renta_m2, probs = 0.75, type = 4),
                campos = paste(campos_a_usar, collapse = " "),
                pivote = paste(campo_fijo, collapse = " "),
                .groups = "drop") 
    
    resultado <- bind_rows(resultado, parciales)
  }
  resultado <- resultado %>%
    select(any_of(c(campo_fijo, dimensiones)), everything())
  return(resultado)
}


###################################################################
# 3 ###############################################################
###################################################################
# Traducir los valores que toma cada variable por el codigo que se usa en BADEA

Recodifica_de_R_a_BADEA <- function(datos_badea){
  #Los valores NA de la tabla se deben a que no se hace uso de ninguna categoria
  # de la variable, por tanto, responden al valor No interviene
  datos_badea[is.na(datos_badea)] <- "No interviene"  
  #datos_badea[is.na(datos_badea)] <- "Total"
  
  
  # Leemos el fichero que contiene la información de traduccion de categorias
  # usadas en el estudio a categorias de BADEA
  cambio_modelo_datos <- read_excel("./datos_aux/Modelo datos BADEA.xlsx",
                                    sheet = "Dimensiones",
                                    skip = 11) %>% 
    filter(!is.na(var))
  
  #names(cambio_modelo_datos)
  
  # diccionario para cambiar el contenido de las variables
  diccionario_badea <- cambio_modelo_datos %>% 
    filter(!is.na(categ)) %>% 
    select(var,categ,cod)
  
  
  # Cambio del contenido de la variables de acuerdo al diccionario cargado
  datos_badea <- datos_badea %>%
    mutate(across(where(is.character), ~ 
                    coalesce(deframe(subset(diccionario_badea, var == cur_column(), 
                                            select= -var))[as.character(.)], as.character(.)
                    )
    )
    )
  
  # # Diccionario para cambiar el contenido de las variables
  # rename_vec <- cambio_modelo_datos %>% 
  #   filter (is.na(categ) & is.na(cod)) %>% pull(var,descrip)
  # 
  # # Renombra las variables (creo que no es necesario, que BADEA reconoce las variables
  # # por el orden en que se introducen)
  # datos_badea <- datos_badea %>% 
  #   rename(all_of(rename_vec))
  
}



genera_microcubo_mun <- function(combinaciones_campos){
  BADEA_mun <- genera_lineas_BADEA(datos_para_BADEA,
                                   campo_fijo = c("anyo","cod_ine"),
                                   combinaciones_campos)
  
  BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) 
  
  BADEA_prov <- genera_lineas_BADEA(datos_para_BADEA,
                                    campo_fijo = c("anyo","provincia_806"),
                                    combinaciones_campos)
  
  BADEA_prov <- BADEA_prov %>% rename(territorio = provincia_806)
  
  
  BADEA_andalucia <- genera_lineas_BADEA(datos_para_BADEA,
                                         campo_fijo = c("anyo"),
                                         combinaciones_campos)
  
  tabla_formato_BADEA <- bind_rows(BADEA_mun, 
                                   BADEA_prov,
                                   BADEA_andalucia) %>% 
#    filter(n>=10) %>%   # No filtrar la información, se filtra con BADEA
    
    Recodifica_de_R_a_BADEA() %>% 
    
    relocate(territorio, .after = "anyo") %>%
    
    select(-c(campos,pivote))
  
  #renombrado de los campos
  
  rename_vec <- c(
    "durac" = "f.durac_contrato",
    "renta" = "f.renta_alq",
    "renta_m2" = "f.renta_m2",
    "sexo_ardor" = "sexo_arrendador", 
    "tip_person" = "f.persona_fj",
    "sexo_arrio" = "sexo_arrendatario", 
    "nacionalid" = "nacionalidad_arrendatario",
    "muebles" = "tipo_de_arrendamiento", 
    "superf" = "tipo_de_arrendamiento",
    "habitac" = "hab", 
    "tipolog_v" = "f.tipolog",
    "antig_bi" = "f.antig_bi",
    "tam_pob" = "f.tam_pob",
    
    "sexo_ardor" = "sexo_arrendador_jer", 
    "tip_person" = "f.persona_fj_jer"
  )

  tabla_formato_BADEA <- tabla_formato_BADEA %>% 
  rename(any_of(rename_vec))

return(tabla_formato_BADEA)
}

genera_microcubo_prov <- function(combinaciones_campos){
  BADEA_prov <- genera_lineas_BADEA(datos_para_BADEA,
                                    campo_fijo = c("anyo","provincia_806"),
                                    combinaciones_campos)
  
  BADEA_prov <- BADEA_prov %>% rename(territorio = provincia_806)
  
  
  BADEA_andalucia <- genera_lineas_BADEA(datos_para_BADEA,
                                         campo_fijo = c("anyo"),
                                         combinaciones_campos)
  
  tabla_formato_BADEA <- bind_rows(BADEA_prov,
                                   BADEA_andalucia) %>% 
    #    filter(n>=10) %>%   # No filtrar la información, se filtra con BADEA
    
    Recodifica_de_R_a_BADEA() %>% 
    
    relocate(territorio, .after = "anyo") %>%
    
    select(-c(campos,pivote))
  
  #renombrado de los campos
  
  rename_vec <- c(
    "durac" = "f.durac_contrato",
    "renta" = "f.renta_alq",
    "renta_m2" = "f.renta_m2",
    "sexo_ardor" = "sexo_arrendador", 
    "tip_person" = "f.persona_fj",
    "sexo_arrio" = "sexo_arrendatario", 
    "nacionalid" = "nacionalidad_arrendatario",
    "muebles" = "tipo_de_arrendamiento", 
    "superf" = "tipo_de_arrendamiento",
    "habitac" = "hab", 
    "tipolog_v" = "f.tipolog",
    "antig_bi" = "f.antig_bi",
    "tam_pob" = "f.tam_pob",
    
    "sexo_ardor" = "sexo_arrendador_jer", 
    "tip_person" = "f.persona_fj_jer"
  )
  
  tabla_formato_BADEA <- tabla_formato_BADEA %>% 
    rename(any_of(rename_vec))
  
  return(tabla_formato_BADEA)
}

genera_microcubo_pota <- function(combinaciones_campos){
  BADEA_unidad <- genera_lineas_BADEA(datos_para_BADEA,
                                      campo_fijo = c("anyo","pota.unidad_territorial"),
                                      combinaciones_campos)
  
  #BADEA_unidad <- BADEA_unidad %>% rename(territorio = pota.unidad_territorial) 
  
  BADEA_tipo_unidad <- genera_lineas_BADEA(datos_para_BADEA,
                                           campo_fijo = c("anyo","pota.tipo_unidad"),
                                           combinaciones_campos)
  
  BADEA_tipo_unidad <- BADEA_tipo_unidad %>% rename(pota.unidad_territorial = pota.tipo_unidad)
  
  
  BADEA_andalucia <- genera_lineas_BADEA(datos_para_BADEA,
                                         campo_fijo = c("anyo"),
                                         combinaciones_campos)
  
  tabla_formato_BADEA <- bind_rows(BADEA_unidad, 
                                   BADEA_tipo_unidad,
                                   BADEA_andalucia) %>% 
    
    #    filter(n>=10) %>%   # No filtrar la información, se filtra con BADEA
    
    Recodifica_de_R_a_BADEA() %>% 
    
    rename(territorio = pota.unidad_territorial) %>% 
    relocate(territorio, .after = "anyo") %>%
    
    select(-c(campos,pivote))
  
  #como hay registros en los datos origienales que responden  municipios cuyo
  #ambito pota no se ha especificado
  
  # #n vez de filter (n>=10)
  #  %>% 
  #  mutate(across(
  #    mediana_precio_mes:p75_precio_m2,
  #    ~ ifelse(n <= 10, NA, .)
  #  ))
  
  
  #renombrado de los campos
  rename_vec <- c(
    "durac" = "f.durac_contrato",
    "renta" = "f.renta_alq",
    "renta_m2" = "f.renta_m2",
    "sexo_ardor" = "sexo_arrendador", 
    "tip_person" = "f.persona_fj",
    "sexo_arrio" = "sexo_arrendatario", 
    "nacionalid" = "nacionalidad_arrendatario",
    "muebles" = "tipo_de_arrendamiento", 
    "superf" = "tipo_de_arrendamiento",
    "habitac" = "hab", 
    "tipolog_v" = "f.tipolog",
    "antig_bi" = "f.antig_bi",
    "tam_pob" = "f.tam_pob",
    
    "sexo_ardor" = "sexo_arrendador_jer", 
    "tip_person" = "f.persona_fj_jer"
  )
  
  tabla_formato_BADEA <- tabla_formato_BADEA %>% 
    rename(any_of(rename_vec))
  
  return(tabla_formato_BADEA)
}


genera_microcubo_secciones <- function(combinaciones_campos){
  BADEA_seccion <- genera_lineas_BADEA(datos_para_BADEA,
                                       campo_fijo = c("anyo","seccion.codigo"),
                                       combinaciones_campos)
  
  BADEA_seccion <- BADEA_seccion %>% rename(territorio = seccion.codigo) 
  
  BADEA_distrito <- genera_lineas_BADEA(datos_para_BADEA,
                                        campo_fijo = c("anyo","seccion.distrito"),
                                        combinaciones_campos)
  
  BADEA_distrito <- BADEA_distrito %>% rename(territorio = seccion.distrito)
  
  BADEA_mun <- genera_lineas_BADEA(datos_para_BADEA,
                                   campo_fijo = c("anyo","cod_ine"),
                                   combinaciones_campos)
  
  BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) 
  
  # BADEA_andalucia <- genera_lineas_BADEA(datos_para_BADEA,
  #                                        campo_fijo = c("anyo"),
  #                                        combinaciones_campos)
  
  tabla_formato_BADEA <- bind_rows(BADEA_seccion, 
                                   BADEA_distrito,
                                   BADEA_mun) %>% 
#    filter(n>=10) %>% 
    
    Recodifica_de_R_a_BADEA() %>% 
    
    relocate(territorio, .after = "anyo") %>%
    
    select(-c(campos,pivote))
  
  #renombrado de los campos
  
  rename_vec <- c(
    "durac" = "f.durac_contrato",
    "renta" = "f.renta_alq",
    "renta_m2" = "f.renta_m2",
    "sexo_ardor" = "sexo_arrendador", 
    "tip_person" = "f.persona_fj",
    "sexo_arrio" = "sexo_arrendatario", 
    "nacionalid" = "nacionalidad_arrendatario",
    "muebles" = "tipo_de_arrendamiento", 
    "superf" = "tipo_de_arrendamiento",
    "habitac" = "hab", 
    "tipolog_v" = "f.tipolog",
    "antig_bi" = "f.antig_bi",
    "tam_pob" = "f.tam_pob",
    
     "sexo_ardor" = "sexo_arrendador_jer", 
     "tip_person" = "f.persona_fj_jer"
  )
  
  tabla_formato_BADEA <- tabla_formato_BADEA %>% 
    rename(any_of(rename_vec))
  
  return(tabla_formato_BADEA)
}

######################################################################
# y ejecuto salida por MUNICIPIOS, PROVINCIA Y ANDALUCIA

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14706_f.durac_contrato   
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.durac_contrato")
cubo_BADEA_14706_f.durac_contrato <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14706_f.durac_contrato,
            file = "./datos_output/cubo_BADEA_14706_f.durac_contrato.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

# write_xlsx(cubo_BADEA_14706_f.durac_contrato, "./datos_output/cubo_BADEA_14706_f.durac_contrato.xlsx")

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14707_renta_mes   
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.renta_alq")
cubo_BADEA_14707_renta_mes <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14707_renta_mes,
            file = "./datos_output/cubo_BADEA_14707_renta_mes.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14708_renta_m2  
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.renta_m2")
cubo_BADEA_14708_renta_m2 <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14708_renta_m2,
            file = "./datos_output/cubo_BADEA_14708_renta_m2.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14702_sexo_arrendador
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("sexo_arrendador"))
cubo_BADEA_14702_sexo_arrendador_1 <- genera_microcubo_mun(combinaciones_campos)

# combinaciones_campos <- obten_combinaciones_campos(
#   n_campos = 1,
#   lista_campos = c("sexo_arrendador_jer"))
# Ahora no tomo todas las combinaciones ya que se producirían registros duplicados
combinaciones_campos <- c("sexo_arrendador_jer")
cubo_BADEA_14702_sexo_arrendador_2 <- genera_microcubo_mun(combinaciones_campos)

# cubo_BADEA_14702_sexo_arrendador_2 <- cubo_BADEA_14702_sexo_arrendador_2 %>% 
#   rename(sexo_ardor = sexo_arrendador_jer)

cubo_BADEA_14702_sexo_arrendador <- bind_rows(
  cubo_BADEA_14702_sexo_arrendador_1,
  cubo_BADEA_14702_sexo_arrendador_2
)
write.table(cubo_BADEA_14702_sexo_arrendador,
            file = "./datos_output/cubo_BADEA_14702_sexo_arrendador.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14704_tipo_entidad
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.persona_fj"))
cubo_BADEA_14704_tipo_entidad_1 <- genera_microcubo_mun(combinaciones_campos)

# combinaciones_campos <- obten_combinaciones_campos(
#   n_campos = 1,
#   lista_campos = c("f.persona_fj_jer"))
# Ahora no tomo todas las combinaciones ya que se producirían registros duplicados
combinaciones_campos <- c("f.persona_fj_jer")
cubo_BADEA_14704_tipo_entidad_2 <- genera_microcubo_mun(combinaciones_campos)

cubo_BADEA_14704_tipo_entidad <- bind_rows(
  cubo_BADEA_14704_tipo_entidad_1,
  cubo_BADEA_14704_tipo_entidad_2
)

write.table(cubo_BADEA_14704_tipo_entidad,
            file = "./datos_output/cubo_BADEA_14704_tipo_entidad.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14705_sex_nac_arr
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 2,
  lista_campos = c("sexo_arrendatario","nacionalidad_arrendatario"))
cubo_BADEA_14705_sex_nac_arr <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14705_sex_nac_arr,
            file = "./datos_output/cubo_BADEA_14705_sex_nac_arr.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14715_tipo_construccion
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.tipolog"))
cubo_BADEA_14715_tipo_construccion <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14715_tipo_construccion,
            file = "./datos_output/cubo_BADEA_14715_tipo_construccion.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14709_tipo_inmueble
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("tipo_de_arrendamiento"))
cubo_BADEA_14709_tipo_inmueble <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14709_tipo_inmueble,
            file = "./datos_output/cubo_BADEA_14709_tipo_inmueble.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14711_superficie
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.super"))
cubo_BADEA_14711_superficie <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14711_superficie,
            file = "./datos_output/cubo_BADEA_14711_superficie.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14710_habitaciones
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.hab"))
cubo_BADEA_14710_habitaciones <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14710_habitaciones,
            file = "./datos_output/cubo_BADEA_14710_habitaciones.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14712_antiguedad
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.antig_bi"))
cubo_BADEA_14712_antiguedad <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14712_antiguedad,
            file = "./datos_output/cubo_BADEA_14712_antiguedad.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14713_tam_municipio
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.tam_pob"))
cubo_BADEA_14713_tam_municipio <- genera_microcubo_mun(combinaciones_campos)

write.table(cubo_BADEA_14713_tam_municipio,
            file = "./datos_output/cubo_BADEA_14713_tam_municipio.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_BADEA_14714_jerarquia_ciudades
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("pota.jerarquia"))
cubo_BADEA_14714_jerarquia_ciudades <- genera_microcubo_prov(combinaciones_campos)

write.table(cubo_BADEA_14714_jerarquia_ciudades,
            file = "./datos_output/cubo_BADEA_14714_jerarquia_ciudades.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)




######################################################################
# y ejecuto salida por AMBITOS POTA

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14827_f.durac_contrato   
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.durac_contrato")
cubo_pota_14827_f.durac_contrato   <- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14827_f.durac_contrato,
            file = "./datos_output/cubo_pota_14827_f.durac_contrato.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

# write_xlsx(cubo_BADEA_14706_f.durac_contrato, "./datos_output/cubo_BADEA_14706_f.durac_contrato.xlsx")

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14830_renta_mes   
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.renta_alq")
cubo_pota_14830_renta_mes   <- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14830_renta_mes,
            file = "./datos_output/cubo_pota_14830_renta_mes.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14832_renta_m2  
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = "f.renta_m2")
cubo_pota_14832_renta_m2<- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14832_renta_m2,
            file = "./datos_output/cubo_pota_14832_renta_m2.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14824_sexo_arrendador
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("sexo_arrendador"))
cubo_pota_14824_sexo_arrendador_1 <- genera_microcubo_pota(combinaciones_campos)

# combinaciones_campos <- obten_combinaciones_campos(
#   n_campos = 1,
#   lista_campos = c("sexo_arrendador_jer"))
# Ahora no tomo todas las combinaciones ya que se producirían registros duplicados
combinaciones_campos <- c("sexo_arrendador_jer")
cubo_pota_14824_sexo_arrendador_2 <- genera_microcubo_pota(combinaciones_campos)

# cubo_BADEA_14702_sexo_arrendador_2 <- cubo_BADEA_14702_sexo_arrendador_2 %>% 
#   rename(sexo_ardor = sexo_arrendador_jer)

cubo_pota_14824_sexo_arrendador<- bind_rows(
  cubo_pota_14824_sexo_arrendador_1,
  cubo_pota_14824_sexo_arrendador_2
)
write.table(cubo_pota_14824_sexo_arrendador,
            file = "./datos_output/cubo_pota_14824_sexo_arrendador.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14825_tipo_entidad
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.persona_fj"))
cubo_pota_14825_tipo_entidad_1 <- genera_microcubo_pota(combinaciones_campos)

# combinaciones_campos <- obten_combinaciones_campos(
#   n_campos = 1,
#   lista_campos = c("f.persona_fj_jer"))
# Ahora no tomo todas las combinaciones ya que se producirían registros duplicados
combinaciones_campos <- c("f.persona_fj_jer")
cubo_pota_14825_tipo_entidad_2 <- genera_microcubo_pota(combinaciones_campos)

cubo_pota_14825_tipo_entidad <- bind_rows(
  cubo_pota_14825_tipo_entidad_1,
  cubo_pota_14825_tipo_entidad_2
)

write.table(cubo_pota_14825_tipo_entidad,
            file = "./datos_output/cubo_pota_14825_tipo_entidad.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14826_sex_nac_arr
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 2,
  lista_campos = c("sexo_arrendatario","nacionalidad_arrendatario"))
cubo_pota_14826_sex_nac_arr<- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14826_sex_nac_arr,
            file = "./datos_output/cubo_pota_14826_sex_nac_arr.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14839_tipo_construccion
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.tipolog"))
cubo_pota_14839_tipo_construccion<- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14839_tipo_construccion,
            file = "./datos_output/cubo_pota_14839_tipo_construccion.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14833_tipo_inmueble
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("tipo_de_arrendamiento"))
cubo_pota_14833_tipo_inmueble <- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14833_tipo_inmueble,
            file = "./datos_output/cubo_pota_14833_tipo_inmueble.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14835_superficie
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.super"))
cubo_pota_14835_superficie<- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14835_superficie,
            file = "./datos_output/cubo_pota_14835_superficie.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14834_habitaciones
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.hab"))
cubo_pota_14834_habitaciones<- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14834_habitaciones,
            file = "./datos_output/cubo_pota_14834_habitaciones.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# cubo_pota_14836_antiguedad
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.antig_bi"))
cubo_pota_14836_antiguedad <- genera_microcubo_pota(combinaciones_campos)

write.table(cubo_pota_14836_antiguedad,
            file = "./datos_output/cubo_pota_14836_antiguedad.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)




######################################################################
# y ejecuto salida por SECCIONES CENSALES

# cubo_BADEA_14820_tipo_construccion
combinaciones_campos <- obten_combinaciones_campos(
  n_campos = 1,
  lista_campos = c("f.tipolog"))
cubo_secciones_14820_tipo_construccion <- genera_microcubo_secciones(combinaciones_campos)

# # arreglo temporal , arreglar en la parte de superposición con la capa de secciones
# cubo_secciones_14820_tipo_construccion <- cubo_secciones_14820_tipo_construccion %>% 
#   filter(territorio != "No especificado")

write.table(cubo_secciones_14820_tipo_construccion,
            file = "./datos_output/cubo_secciones_14820_tipo_construccion.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)

skimr::skim(cubo_secciones_14820_tipo_construccion)
# naniar::vis_miss(cubo_secciones_14820_tipo_construccion)
# naniar::gg_miss_upset(cubo_secciones_14820_tipo_construccion, nsets = 10, nintersects = 50)
