# Genera la salida enviada a BADEA el 05/10/2023 a última hora
# Genera una salida de un ÚNICO GRAN CUBO
# Contiene la columna de unidades territoriales rellena para los municipios
# lo cuál genera problemas ya que con el filtro en el campo POTA = todos los municipios
# no sale ningún municipio 

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
                 "f.tam_pob","pota.jerarquia")
     #, "pota.tipo_unidad","pota.unidad_territorial")


# el anyo es un dimensión especial porque siempre hay que pivotar sobre ella,
# ya que en a BADEA todos los datos contiene rellena la columna de datos
dimensiones_pivotantes <- 
  dimensiones[dimensiones != "anyo"]

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

genera_BADEA <- function(df, campo_fijo = NULL, combinacion_campos){
  #browser()
  resultado <- data.frame()
  parciales <- data.frame()
  for (campos  in rev(combinacion_campos)) {
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

BADEA_mun <- genera_BADEA(datos_para_BADEA,
                          c("anyo","cod_ine"),
                          obten_combinaciones_campos(
                            n_campos = 1,
                            lista_campos = dimensiones_pivotantes))

BADEA_mun <- BADEA_mun %>% rename(territorio = cod_ine) 

#AÑADIR CAMPOS DEL POTA A PARTIR DEL MUNICIPIO
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
         pota.tipo_unidad = tipo_de_unidad_territorial,
         pota.unidad_territorial = unidad_territorial) %>% 
  filter(!is.na(provincia)) %>% 
  select(codigo_municipal, pota.tipo_unidad, pota.unidad_territorial)

BADEA_mun <-  left_join(BADEA_mun, 
                    adsc_mun, 
                    by = c("territorio" = "codigo_municipal"))

BADEA_mun <- BADEA_mun %>%
  relocate(pota.tipo_unidad, pota.unidad_territorial ,pota.jerarquia, .after = territorio)



BADEA_prov <- genera_BADEA(datos_para_BADEA, c("anyo","provincia_806"),
                           obten_combinaciones_campos(2,dimensiones_pivotantes))
BADEA_prov <- BADEA_prov %>% rename(territorio = provincia_806)

BADEA_andalucia <- genera_BADEA(datos_para_BADEA, "anyo", #antes NULL
                                obten_combinaciones_campos(3, dimensiones_pivotantes))


tabla_para_badea <- bind_rows(BADEA_mun, 
                              BADEA_prov,
                              BADEA_andalucia) %>% 
  filter(n>=10) %>% 
  
  #elimino registros en los que se replica la información de tipo de unidad y unidad territorial
  #en la tabla de salida ambos campos son una única dimensión
  # filter(!(!is.na(pota.tipo_unidad) & !is.na(pota.unidad_territorial))) %>%  
  
  Recodifica_de_R_a_BADEA() %>% 
  
  #Construye el nuevo campo que contiene información del tipo de unidad y de la unidad territorial
  mutate(pota = pota.unidad_territorial,
         pota.tipo_unidad = NULL,
         pota.unidad_territorial = NULL) %>%
  relocate(pota, .before = "pota.jerarquia") %>%
  relocate(territorio, .before = "pota") %>%
  
  select(-c(campos,pivote))

 # tabla_para_badea <- janitor::clean_names(tabla_para_badea)
 # colnames(tabla_para_badea) <- substr(names(tabla_para_badea), 1, 10)
 # tabla_para_badea <- janitor::clean_names(tabla_para_badea)

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


