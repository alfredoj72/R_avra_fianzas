# Version de pruebas NO SE HAN ENVIADO DATOS CON ESTE MODELO A NINGUN SITIO.
# genera un UNICO GRAN CUBO y mezcla en una columna los atributos de tipo de
# unidad y de unidad POTA
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




# Calcula las combinaciones de campos desde tamaño 0 hasta n_campos
obten_combinaciones_campos <- function(n_campos, lista_campos) {
  unlist(lapply(0:n_campos, function(n){combn(lista_campos,m=n, simplify = FALSE)} ),
         recursive = FALSE)
}

genera_BADEA <- function(df, campo_fijo = NULL, lista_combinaciones_campos){
  #browser()
  resultado <- data.frame()
  parciales <- data.frame()
  for (campos  in rev(lista_combinaciones_campos)) {
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
    select(any_of(c(dimensiones, campo_fijo)), everything())
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



######################################################################
# y ejecuto

#> el año es pivote siempre.Para cada ambito territorial pivota su campo, y el
#> resto son dimensiones 

# dimensiones_sin_POTA <- dimensiones[!dimensiones %in% 
#                                       c("pota.unidad_territorial", "anyo")]

# old lista_campos
dimensiones <- c("anyo", "f.durac_contrato","f.renta_alq", "f.renta_m2",
                 "sexo_arrendador", "f.persona_fj", "sexo_arrendatario",
                 "nacionalidad_arrendatario", "tipo_de_arrendamiento",
                 "f.super","f.hab","f.tipolog", "f.antig_bi",
                 "f.tam_pob","pota.tipo_unidad", "pota.unidad_territorial",
                 "pota.jerarquia")

territorio <- c("provincia_806","cod_ine","seccion.codigo")

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


#> El campo que contiene la información del POTA referida a unidades territoriales
#> tiene una doble clasificación. Por una parte se encuentran los tipos de
#> unidades territoriales y cada uno de los tipos se divide en las diferentes
#> unidades territoriales. Es decir funciona como si hubiese dos variables en 
#> un mismo campo, en nuestro caso las dos variables son pota.tipo_unidad y
#> pota.unidad_territorial. Que se van a unir en BADEA en el campo pota.
#> Por este motivo la lista de combinaciones de campos por los que hay que pivotar 
#> hay que generarla a base de combinaciones. Por una parte se combinan todas
#> las combinaciones posibles de todas las variables sin tener en cuenta 
#> pota.unidad_territorial y por otra parte se usan las combinaciones de 
#> pota.unidad_territorial solo en los casos en los que ella interviene


# el anyo es un dimensión especial porque siempre hay que pivotar sobre ella,
# ya que en a BADEA todos los datos contiene rellena la columna de datos
dimensiones_pivotantes <- 
  dimensiones[!dimensiones %in% c("anyo","pota.unidad_territorial")]

combinaciones_de_campos <- obten_combinaciones_campos(1,dimensiones_pivotantes)

dimensiones_pivotantes <- 
  dimensiones[!dimensiones %in% c("anyo","pota.tipo_unidad")]

# Me quedo solo con las combinaciones de campos en las que esta pota.unidad_territorial
contiene <- function(sublista){
  any(sublista == "pota.unidad_territorial")
}
combinaciones_de_campos2 <- obten_combinaciones_campos(1,dimensiones_pivotantes)
combinaciones_de_campos2 <- combinaciones_de_campos2[sapply(combinaciones_de_campos2, contiene)]

combinaciones_de_campos <- c(combinaciones_de_campos, combinaciones_de_campos2)


BADEA_mun <- genera_BADEA(datos_para_BADEA, c("anyo", "cod_ine"),
                          combinaciones_de_campos)
BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) 




dimensiones_pivotantes <- dimensiones[!dimensiones %in% c("anyo","pota.unidad_territorial")]
combinaciones_de_campos <-  obten_combinaciones_campos(2,dimensiones_pivotantes)

dimensiones_pivotantes <- 
  dimensiones[!dimensiones %in% c("anyo","pota.tipo_unidad")]

combinaciones_de_campos2 <- obten_combinaciones_campos(2,dimensiones_pivotantes)
combinaciones_de_campos2 <- combinaciones_de_campos2[sapply(combinaciones_de_campos2, contiene)]

combinaciones_de_campos <- c(combinaciones_de_campos, combinaciones_de_campos2)

BADEA_prov <- genera_BADEA(datos_para_BADEA, c("anyo","provincia_806"),
                           combinaciones_de_campos)

BADEA_prov <- BADEA_prov %>% rename(territorio = provincia_806)




dimensiones_pivotantes <- dimensiones[!dimensiones %in% c("anyo","pota.unidad_territorial")]
combinaciones_de_campos <-  obten_combinaciones_campos(3,dimensiones_pivotantes)

dimensiones_pivotantes <- 
  dimensiones[!dimensiones %in% c("anyo","pota.tipo_unidad")]

combinaciones_de_campos2 <- obten_combinaciones_campos(3,dimensiones_pivotantes)
combinaciones_de_campos2 <- combinaciones_de_campos2[sapply(combinaciones_de_campos2, contiene)]

combinaciones_de_campos <- c(combinaciones_de_campos, combinaciones_de_campos2)

BADEA_andalucia <- genera_BADEA(datos_para_BADEA, "anyo", #antes NULL
                                combinaciones_de_campos)





tabla_para_badea <- bind_rows(BADEA_mun, 
                              BADEA_prov,
                              BADEA_andalucia) %>% 
  filter(n>=10) %>% 
  
  Recodifica_de_R_a_BADEA() %>% 
  
  #Construye el nuevo campo que contiene información del tipo de unidad y de la unidad territorial
  mutate(pota = coalesce(pota.tipo_unidad, pota.unidad_territorial),
         pota.tipo_unidad = NULL,
         pota.unidad_territorial = NULL) %>%
  relocate(pota, .before = "pota.jerarquia") %>%
  relocate(territorio, .before = "pota") %>%
  
  select(-c(campos,pivote))

nombres_campos <- c("anyo", "durac",  "renta", "renta_m2", "sexo_ardor", 
                    "tip_person", "sexo_arrio",  "nacionalid", "muebles", 
                    "superf", "habitac", "tipolog_v", "antig_bi", "tam_pob",
                    "territorio", "pota", "pota_jerar", "n", "m_precio",
                    "p25_precio", "p75_precio", "m_superf", "p25_superf",
                    "p75_superf",  "m_pre_m2", "p25_pre_m2", "p75_pre_m2")

colnames(tabla_para_badea) <- nombres_campos

# # #comprobacion no hay combinaciones de campos duplicadas
# solo_dimensiones <- tabla_para_badea %>% select(-c(n:p75_precio_m2))
# duplicados <- solo_dimensiones[duplicated(solo_dimensiones), ]

write.table(tabla_para_badea,
            file = "./datos_output/tabla_para_badea.txt",
            sep = "\t", 
            row.names = FALSE,
            col.names = TRUE)


# write_xlsx(tabla_para_badea, "./datos_output/tabla_para_badea.xlsx")
# write.csv(tabla_para_badea, file = "./datos_output/tabla_para_badea.csv", row.names = FALSE)


