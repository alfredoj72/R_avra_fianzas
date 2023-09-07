#> ojo a las siguientes dudas
#> No se han incluido como dimensiones superficie, precio, precio m2
#> Se ha incluido ambitos POTA como dimension y no como territorio.
#> Asociado a cada municipio he incluido el ambito POTA pero no se si sirve de algo
#> y como afecta esto en BADEA
#> No se han considerado las secciones censales
#> Se prodría reagrupar el constructo persona jurídica?
#> 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf,tidyverse,writexl,readxl)


rm(list =ls())
load("./datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["datos"]]
rm(datos_para_analisis_2022)
datos <- as.data.frame(datos)
datos <- st_drop_geometry(datos)
str(datos)


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

datos$anyo <- lubridate::year(datos$fecha_devengo)

# old lista_campos
dimensiones <- c("anyo", "f.durac_contrato","f.renta_alq", "f.renta_m2",
                  "sexo_arrendador", "f.persona_fj", "sexo_arrendatario",
                  "nacionalidad_arrendatario", "tipo_de_arrendamiento",
                  "f.super","f.hab","f.tipolog", "f.antig_bi",
                  "f.tam_pob", "pota.unidad_territorial")

dimensiones_sin_POTA <- dimensiones[dimensiones != "pota.unidad_territorial"]

territorio <- c("provincia_806","cod_ine","seccion.codigo")

medidas <- c("importe_de_la_renta" , "stotalocal_14" , "renta_m2")

# Me quedo con las variables que necesito
datos_BADEA <- datos %>% select(all_of(c(dimensiones, territorio, medidas)))

#summary(datos_BADEA)

# Paso las variables a formato texto
datos_BADEA[c(dimensiones, territorio)] <- 
    lapply(datos_BADEA[c(dimensiones, territorio)], as.character)


# Si un campo por el que se agrupa tiene NA este también aparecerá en el resultado
# No deben existir valores NA, de lo contrario se confundirán con los NA
# que salen cuando un campo no se usa como criterio de agrupación. Se deben rellenar
# con "No Especificado"
# Reemplazo los valores NA por "No especificado" en cualquier campo
datos_BADEA[is.na(datos_BADEA)] <- "No especificado"



# Para pruebas
#lista_campos <- c("anyo", "f.durac_contrato","f.renta_alq")

# El campo pivote es el del nivel de agregación para el que se van a tener 
# los resultados. Tendremos: 
# Para toda Andalucía c("")
# Para provincias c("provincia_806), municipio "municipio_806" ,etc 

# campo_pivote <- c("") #"provincia_806","cod_ine") 
# 
# combinaciones_campos <- unlist(lapply(0:length(lista_campos),
#                     function(n){combn(lista_campos,m=n, simplify = FALSE)} ),
#                         recursive = FALSE)

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

# obtiene la tabla para BADEA
# if (exists("resultado")) rm(resultado) ; resultado <- data.frame()
# 
# for (campos  in rev(combinaciones_campos)) {
#   parciales <- datos %>%
#     select(all_of(campos),
#            any_of(campo_pivote),
#            importe_de_la_renta , stotalocal_14 , renta_m2) %>%
#     
#     group_by(across(c(all_of(campos),any_of(campo_pivote)))) %>% 
#    
#     # en el summarise siguiente es en el que hay que colocar los parámetros
#     # que se quieran calcular para cada grupo: n(), mediana, Q25, Q75 de los campos
#     summarise(n = n(),
#               mediana_precio_mes = median(importe_de_la_renta),
#               p25_precio_mes = quantile(importe_de_la_renta, 0.25),
#               p75_precio_mes = quantile(importe_de_la_renta, 0.75),
#               mediana_superficie = median(stotalocal_14),
#               p25_superficie = quantile(stotalocal_14, 0.25),
#               p75_superficie = quantile(stotalocal_14, 0.75),
#               mediana_precio_m2 = median(renta_m2),
#               p25_precio_m2 = quantile(renta_m2, 0.25),
#               p75_precio_m2 = quantile(renta_m2, 0.75),
#               .groups = "drop") 
#   
#   resultado <- bind_rows(resultado, parciales)
# }


# en el df resultado habría que sustituir los NA (por no usar el criterio)
# por el valor que en 
# BADEA se utilize para indicar que dicha variable no se usa en esa línea



genera_BADEA <- function(df, campo_pivote = NULL, combinacion_campos){
  #browser()
  resultado <- data.frame()
  parciales <- data.frame()
  for (campos  in rev(combinacion_campos)) {
    if (!is.null(campo_pivote)){
      campos_a_usar <- c(campos, campo_pivote)
    } else {
      campos_a_usar <- campos
    }
    parciales <- df %>%
      select(all_of(campos_a_usar),
             importe_de_la_renta , stotalocal_14 , renta_m2) %>%
      
      #  group_by(across(c(all_of(campos),any_of(campo_pivote)))) %>% 
      
      group_by(across(all_of(campos_a_usar))) %>% 
      
      # en el summarise siguiente es en el que hay que colocar los parámetros
      # que se quieran calcular para cada grupo: n(), mediana, Q25, Q75 de los campos
      summarise(n = n(),
                mediana_precio_mes = median(importe_de_la_renta),
                p25_precio_mes = quantile(importe_de_la_renta, 0.25),
                p75_precio_mes = quantile(importe_de_la_renta, 0.75),
                mediana_superficie = median(stotalocal_14),
                p25_superficie = quantile(stotalocal_14, 0.25),
                p75_superficie = quantile(stotalocal_14, 0.75),
                mediana_precio_m2 = median(renta_m2),
                p25_precio_m2 = quantile(renta_m2, 0.25),
                p75_precio_m2 = quantile(renta_m2, 0.75),
                campos = paste(campos_a_usar, collapse = " "),
                pivote = paste(campo_pivote, collapse = " "),
                .groups = "drop") 
    
    resultado <- bind_rows(resultado, parciales)
  }
  resultado <- resultado %>% 
    select(any_of(c(dimensiones_sin_POTA, campo_pivote)), everything())
  return(resultado)
}


###################################################################
# 3 ###############################################################
###################################################################
# Traducir los valores que toma cada variable por el codigo que se usa en BADEA




# Leemos el fichero que contiene la información de traduccion de categorias
# usadas en el estudio a categorias de BADEA

cambio_modelo_datos <- read_excel("./datos_aux/BADEA/Modelo datos BADEA.xlsx",
                                sheet = "Dimensiones",
                                skip = 11) %>% 
     filter(!is.na(var))

names(cambio_modelo_datos)

#--------------------------------------------------------
# recodifica el contenido de las variables

diccionario_badea <- cambio_modelo_datos %>% 
  filter(!is.na(categ)) %>% 
  select(var,categ,cod)

#diccionario_badea <- read_excel("./datos_aux/BADEA/pruebadiccionarioBADEA.xlsx") 
#diccionario_badea$N1_DESCR <- NULL


Recodifica_de_R_a_BADEA <- function(datos_badea){
#Los valores NA de la tabla se deben a que no se hace uso de ninguna categoria
# de la variable, por tanto, responden al valor total
datos_badea[is.na(datos_badea)] <- "Total"

datos_badea <- datos_badea %>%
  mutate(across(everything(), ~ 
            coalesce(deframe(subset(diccionario_badea, var == cur_column(), 
                           select= -var))[as.character(.)], as.character(.)
                     )
            )
         )



# Renombra las variables (creo que no es necesario, que BADEA reconoce las variables
# por el orden en que se introducen)
rename_vec <- cambio_modelo_datos %>% 
  filter (is.na(categ)) %>% pull(var,descrip)

datos_badea <- datos_badea %>% 
  rename(all_of(rename_vec))

}



######################################################################
# y ejecuto

# puedo incluir la unidad territorial POTA junto al municipio, esto no aporta
# filas, pero no se si es interesante para algo (preguntar a Esther)

BADEA_mun <- genera_BADEA(datos_BADEA,c("cod_ine", "pota.unidad_territorial"),
                          obten_combinaciones_campos(1,dimensiones_sin_POTA))
BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) 

#old
# BADEA_mun <- genera_BADEA(datos_BADEA,"cod_ine",
#                           obten_combinaciones_campos(1,dimensiones_sin_POTA))
# BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) %>% 
#   mutate(pota.unidad_territorial = "", .before = "n")


BADEA_prov <- genera_BADEA(datos_BADEA,"provincia_806",
                           obten_combinaciones_campos(2,dimensiones_sin_POTA))
BADEA_prov <- BADEA_prov %>% rename(territorio = provincia_806)

BADEA_andalucia <- genera_BADEA(datos_BADEA, NULL,
                           obten_combinaciones_campos(3, dimensiones_sin_POTA))

BADEA_andalucia_pot <- genera_BADEA(datos_BADEA, "pota.unidad_territorial",
                           obten_combinaciones_campos(1, dimensiones_sin_POTA))

tabla_para_badea <- bind_rows(BADEA_mun, 
                        BADEA_prov,
                        BADEA_andalucia,
                        BADEA_andalucia_pot) %>% 
              Recodifica_de_R_a_BADEA()


IMPORTANTE
# tengo que resolver si las unidades territoriales del pota se incluyen como 
# territorio o se incluyen como medidas ( así esta ahora). Y si al incluirla 
# como medida es interesante que este en la información de municipios o no sirve de nada?
  



# pruebas 
kk <- BADEA_andalucia %>% filter(n >=10)
BADEA_prov <- genera_BADEA(datos_BADEA,"provincia_806", obten_combinaciones_campos(2,dimensiones_sin_POTA))
BADEA_mun <- genera_BADEA(datos_BADEA,"cod_ine", obten_combinaciones_campos(1,dimensiones_sin_POTA))

BADEA_andalucia <- genera_BADEA(datos_BADEA, NULL,
                                obten_combinaciones_campos(1, dimensiones_sin_POTA))

#plantear solo algunas variables? es decir, no todas las variables?
BADEA_sec <- genera_BADEA(datos_BADEA,"seccion.codigo", obten_combinaciones_campos(1,dimensiones_sin_POTA))
kk <- BADEA_sec %>% filter(n >=10)


BADEA_andalucia_F <- De_R_a_BADEA(BADEA_andalucia)





