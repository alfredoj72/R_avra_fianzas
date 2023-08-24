
# Realiza un analisis descriptivo de las variables.

#################  INICIO    ################
paquetes_necesarios = c("sf","tidyverse","flextable","tmap") # c( "ggplot2","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)

# # parto
# rm(list =ls())
# load("./datos_output/datos_para_analisis_2022.RData")
# contenedor <- datos_para_analisis_2022 ; rm(datos_para_analisis_2022)
# datos <- contenedor[["Fianzas_viviendas"]]
# rm(contenedor)

rm(list =ls())
load("./datos_output/avra_catastro_2022.RData")
contenedor <- avra_catastro_2022
rm(avra_catastro_2022)
avra_datos_originales <- contenedor[["originales"]]
# avra_catastro <- contenedor[["avra_catastro"]]
 tabla_frecuencias  <- contenedor[["tabla_frecuencias"]]
 tabla_frecuencias_final  <- contenedor[["tabla_frecuencias_final"]]
# Fianzas_casan_1_vivienda <- contenedor[["Fianzas_casan_1_vivienda"]]
# Fianzas_no_casan_catastro <- contenedor[["Fianzas_no_casan_catastro"]]
# Fianzas_casan_distintas_viviendas <- contenedor[["Fianzas_casan_distintas_viviendas"]]
# Fianzas_casan_distintas_viviendas_case <- contenedor[["Fianzas_casan_distintas_viviendas_case"]]
datos <- avra_datos_originales
rm(contenedor)

#Leo las tablas de diccionario de datos y defino la función que recupera la 
# etiqueta de un campo a partir de su nombre



# Defino un tema propio para aplicar a todos los graficos que haga con ggplot2
# hacer a mi gusto con los parametros que tengo más abajo
# por aqui voy
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

tema_ams2 <- theme_bw() +
  theme(plot.background = element_rect (size = 1, color ="blue", fill ="black"),
        text = element_text(size = 12, family = "Serif", color= "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour ="red"),
        panel.background = element_rect(fill ="green"))


############# ANALISIS VARIABLE TODAS JUNTAS Y POR CONSOLA ###########
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 
# Funciones que voy a emplear en la descripcion de las variables 

# La descripción de variables será distinta en función de si la variable es
# tipo numerico, texto o factor.


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
for (col in names(datos)) {
  column <- datos[[col]]
  
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

############# ANALISIS VARIABLE A VARIABLE #################
### Definición de funciones y parámetros flextable ####

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

# Parámetros para pintar las tablas en formato bonito
library(flextable)
# Establezco parámetros por defecto para tablas
init_flextable_defaults() #reinicia los valores por defecto del paq flextable
set_flextable_defaults(
  big.mark = ".",
  decimal.mark = ",",
  digits = 1,
  # theme_fun = theme_box,
  font.family = "calibri",  
  line_spacing = 1.3,
  font.color = "grey8", # "#333333",
  font.size = 11,
  border.color = "grey60", # "#333333",
  padding = 4
)

# define el estilo del borde
border_style1 = officer::fp_border(color="grey60", width=2)
border_style2 = officer::fp_border(color="grey60", width=0.5)

# Función que calcula una tabla con recuentos de un solo campo y la pinta con flextable
pinta_tabla <- function(datos,campo,campo_descriptivo,orden = "NO"){
  #browser()
  campo <- datos[[campo]]
  df <- data.frame(campo = campo)
  
  tabla <-df %>% 
    group_by(campo) %>% 
    summarise(Casos=n())  %>%
    ungroup() %>% 
    mutate(Porcentaje = 100 * Casos / sum(Casos), # se puede usar := en vez de =
           campo = as.character(campo),
           campo = ifelse(is.na(campo),"No Especificado", campo))   # se puede usar := en vez de =
  
  n_resto <- tabla %>% 
               filter(campo == "Resto") %>% 
               nrow()
  
  if (orden == "SI") {
    tabla <- tabla %>%  arrange(desc(Casos))
    if (n_resto > 0){
      tabla <- tabla %>% arrange(ifelse(campo == "Resto", 1, 0))
    }
  }
  
  suma_fila <- tabla %>%
    summarise( campo = "Suma",
               Casos = sum(Casos),
               Porcentaje = sum(Porcentaje))
  
  tabla <- bind_rows(tabla, suma_fila) #%>% 
  #mutate({{campo}} := ifelse(is.na({{campo}}), "Suma", {{campo}}))
  
  tabla_impresa <- tabla %>%  flextable() %>%  
    set_header_labels(campo = campo_descriptivo) %>% 
    colformat_double() %>% 
    autofit() %>% 
    border_remove() %>% 
    hline(part = "header", i = 1, border = border_style1)   %>%
    hline(part = "body", i = nrow(tabla)-1, border = border_style2) 
  
  return(tabla_impresa)
}

# Función que calcula una tabla con recuentos de un solo campo, considerando
# todos los valores y solo lo válidos, y la pinta con flextable
pinta_tabla2 <- function(datos,campo,campo_descriptivo,orden = "NO"){
  #browser()
  campo <- datos[[campo]]
  df <- data.frame(campo = campo)
  
  tabla <- df %>% 
    group_by(campo) %>% 
    summarise(Casos=n())  %>%
    ungroup() %>% 
    mutate(Porcentaje = 100 * Casos / sum(Casos), # se puede usar := en vez de =
           campo = as.character(campo),
           Casos_v = as.integer(ifelse(is.na(campo), 0, Casos)),
           Porcentaje_v = 100 * Casos_v / sum(Casos_v),
           campo = ifelse(is.na(campo),"No Especificado", campo))  # se puede usar := en vez de =
  
  n_resto <- tabla %>% 
    filter(campo == "Resto") %>% 
    nrow()
  
  if (orden == "SI") {
    tabla <- tabla %>%  arrange(desc(Casos))
    if (n_resto > 0){
      tabla <- tabla %>% arrange(ifelse(campo == "Resto", 1, 0))
    }
  }
  
  suma_fila <- tabla %>%
    summarise( campo = "Suma",
               Casos = sum(Casos),
               Porcentaje = sum(Porcentaje),
               Casos_v = sum(Casos_v),
               Porcentaje_v = sum(Porcentaje_v))
  
  tabla <- bind_rows(tabla, suma_fila) #%>% 
  #mutate({{campo}} := ifelse(is.na({{campo}}), "Suma", {{campo}}))
  
  tabla_impresa <- tabla %>%  flextable() %>%  
    set_header_labels(campo = campo_descriptivo) %>% 
    colformat_double() %>% 
    autofit() %>% 
    border_remove() %>% 
    
    add_header_row(
      top = TRUE,                # La nueva cabecera va encima de la fila de cabecera existente
      values = c("",             # Valores de cabecera para cada columna a continuación
                 "Total", 
                 "",             # Este será el encabezado de nivel superior para esta columna y las dos siguientes
                 "Válidos",
                 "")) %>% 
    
    
    set_header_labels(         # Renombra las columnas de la fila de cabecera original
      Casos_v = "Casos", 
      Porcentaje_v = "Porcentaje")  %>% 
    
    # Combina horizontalmente las columnas 2 a 3  y 4 a 5 en la nueva fila de encabezado
    merge_at(i = 1, j = 2:3, part = "header") %>% 
    merge_at(i = 1, j = 4:5, part = "header") %>% 
    
    align(align = "center", j = c(2:5), part = "all") %>% 
    
    hline(part = "header", i = 2, border = border_style1)   %>%
    hline(part = "body", i = nrow(tabla)-1, border = border_style2) 
  
  return(tabla_impresa)
}


pinta_tabla3 <- function(datos,campo,campo_descriptivo,orden = "NO"){
  #browser()
  campo <- datos[[campo]]
  df <- data.frame(campo = campo)
  
  tabla <- df %>% 
    group_by(campo) %>% 
    summarise(Casos=n())  %>%
    ungroup() %>% 
    mutate(Porcentaje = 100 * Casos / sum(Casos), # se puede usar := en vez de =
           campo = as.character(campo),
           Casos_v = as.integer(ifelse(is.na(campo), 0, Casos)),
           Porcentaje_v = ifelse(Casos_v >0 , 100 * Casos_v / sum(Casos_v), NA) ,
           campo = ifelse(is.na(campo),"No Especificado", campo)) %>%  # se puede usar := en vez de =
    select(-Casos_v)
  
  #Comprueba si hay una fila que contiene el valor "Resto"
  n_resto <- tabla %>% 
    filter(campo == "Resto") %>% 
    nrow()
  
  #Si se ha indicado que se ordene por número de casos, y si hay una línea con el
  # valor del resto la lleva al final
  if (orden == "SI") {
    tabla <- tabla %>%  arrange(desc(Casos))
    if (n_resto > 0){
      tabla <- tabla %>% arrange(ifelse(campo == "Resto", 1, 0))
    }
  }
  
  suma_fila <- tabla %>%
    summarise( campo = "Suma",
               Casos = sum(Casos),
               Porcentaje = sum(Porcentaje),
               #Casos_v = sum(Casos_v),
               Porcentaje_v = sum(Porcentaje_v, na.rm = TRUE))
  
  tabla <- bind_rows(tabla, suma_fila) #%>% 
  #mutate({{campo}} := ifelse(is.na({{campo}}), "Suma", {{campo}}))
  
  tabla_impresa <- tabla %>%  flextable() %>%  
    set_header_labels(campo = campo_descriptivo) %>% 
    colformat_double() %>% 
    autofit() %>% 
    border_remove() %>% 
    
    # add_header_row(
    #   top = TRUE,                # La nueva cabecera va encima de la fila de cabecera existente
    #   values = c("",             # Valores de cabecera para cada columna a continuación
    #              "Total", 
    #              "",             # Este será el encabezado de nivel superior para esta columna y las dos siguientes
    #              "Válidos",
    #              "")) %>% 
    
    
    set_header_labels(         # Renombra las columnas de la fila de cabecera original
      #Casos_v = "Casos", 
      Porcentaje_v = "Porcentaje Válidos")  %>% 
    
    # Combina horizontalmente las columnas 2 a 3  y 4 a 5 en la nueva fila de encabezado
    # merge_at(i = 1, j = 2:3, part = "header") %>% 
    # merge_at(i = 1, j = 4:5, part = "header") %>% 
    
    align(align = "center", j = c(2:4), part = "all") %>% 
    
    hline(part = "header", i = 1, border = border_style1)   %>%
    hline(part = "body", i = nrow(tabla)-1, border = border_style2) 
  
  return(tabla_impresa)
}


# ANALISIS DE DATOS AUSENTES (NA) #####

#pacman::p_load(naniar)
library(naniar)

# Porcentaje de TODOS los valores del dataframe que faltan
pct_miss(datos)

# Porcentaje de filas en las que falta algún valor
pct_miss_case(datos)   # usa n_complete() para los recuentos


# Porcentaje de filas que están completas (no faltan valores) 
pct_complete_case(datos) # usa n_complete() para los recuentos

datos %>% 
  gg_miss_var( show_pct = TRUE)

datos %>% 
  gg_miss_var(show_pct = TRUE, facet = tipo_persona_arrendador)

# Gráfico de valores faltantes en todo el dataframe
gg_miss_fct(datos, tipo_persona_arrendador) + labs(title = "Valores no disponibles")
######## ANALISIS VARIABLE A VARIABLE ###############################
# Primero voy a hacer una descripción breve de las variables
# Por último haremos un análisis cruzado variables

# Descripción breve de variables
# Comienzo el analisis de cada variable


cat ("\n El número total de registros para el análisis es",
     nrow(datos), "\n")
stringr::str_glue("{nrow(datos)} Total de registros") 

cat ("La tabla que resume el resultado de la conexión de los datos de viviendas con los datos de catastro")


tabla_frecuencias <- tabla_frecuencias %>%
  mutate(`Frec. Absoluta` = as.integer(`Frec. Absoluta`)) 

suma_fila <- tabla_frecuencias %>%
  summarise( `Frec. Absoluta` = sum(`Frec. Absoluta`),
             `Frec. Relativa` = sum(`Frec. Relativa`))


tabla_frecuencias <- bind_rows(tabla_frecuencias, suma_fila) %>% 
 mutate(enlace = ifelse(is.na(enlace), "Suma", enlace))

tabla_frecuencias %>% 
      flextable() %>%  
      set_header_labels(enlace = "Tipo enlace") %>% 
  colformat_double() %>% 
  autofit() %>% 
  border_remove() %>% 
  align(align = "center", j = 1, part = "all") %>% 
  hline(part = "header", i = 1, border = border_style1)   %>%
  hline(part = "body", i = nrow(tabla_frecuencias)-1, border = border_style2) 




cat ("Hay casos en las que la Referencia Catastral indicada en el Registro de Fianzas")
cat ("enlaza en catastro con más de una vivienda (idenfificada por el IECA)")
cat ("Cuando todas las viviendas de catastro asociadas a una vivienda del Registro")
cat ("son muy similares (coeficiente de variación <2) enlazamos con una de ellas")
cat ("Es por esto que al final la tabla de conexiones con catastro queda:")

suma_fila <- tabla_frecuencias_final %>%
  summarise(casos= sum(casos),
            frec_relativa = sum(frec_relativa))

tabla_frecuencias_final <- bind_rows(tabla_frecuencias_final, suma_fila) %>% 
  mutate(tipo = ifelse(is.na(tipo), "Suma", tipo))

tabla_frecuencias_final %>%
  flextable() %>%  
  set_header_labels(tipo = "Tipo enlace",
                    casos = "Frec.Absoluta",
                    frec_relativa = "Frec.Relativa") %>% 
  colformat_double() %>% 
  autofit() %>% 
  border_remove() %>% 
  #align(align = "center", j = 1, part = "all") %>% 
  hline(part = "header", i = 1, border = border_style1)   %>%
  hline(part = "body", i = nrow(tabla_frecuencias)-1, border = border_style2) 




# 
# 
# 
#.-.-.-.-.-.-
#codigo_expediente__rue
Resumen_basico(datos$codigo_expediente__rue)

#.-.-.-.-.-.-
#numero_documento
Resumen_basico(datos$numero_documento)

#.-.-.-.-.-.-
#nif_cif_arrendador_anonimizado
Resumen_basico(datos$nif_cif_arrendador_anonimizado)



######### DEFINICION DE FACTORES ############
#

# Si convierto los NA en un nivel del factor comenzará a ordenarse como el resto
# de los niveles, si lo dejo como NA se van siempre al último de la lista.

# datos <- datos %>%
#   mutate (tipo_persona_arrendador = as.character(tipo_persona_arrendador),
#           sexo_arrendador = as.character(sexo_arrendador))

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





####################   tipo_persona_arrendador       ##################

#Resumen_basico(Fianzas_viviendas$tipo_persona_arrendador)
tabla <- pinta_tabla3(datos, "tipo_persona_arrendador", "Tipo Persona Arrendador")
print(tabla)

# tabla <- pinta_tabla3(datos, "tipo_persona_arrendador", "Tipo Persona Arrendador")
# print(tabla)

# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = tipo_persona_arrendador)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
                            vjust = +0.5, size = 3,
                            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo de persona del arrendador") +
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = tipo_persona_arrendador)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", y = "Frecuencia", title = "Tipo de persona del arrendador") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_minimal()


####################   sexo_arrendador       ##################

datos_pf <- datos %>%
              filter(tipo_persona_arrendador == "Física")
tabla <- pinta_tabla(datos_pf,
                     campo = "sexo_arrendador",
                     campo_descriptivo = "Sexo Arrendador",
                     orden = "SI")
print(tabla)

tabla <- pinta_tabla3(datos_pf,
                     campo = "sexo_arrendador",
                     campo_descriptivo = "Sexo Arrendador",
                     orden = "SI")
print(tabla)

# Gráfico incluyendo valores faltantes
ggplot(datos_pf, aes(x = sexo_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Sexo Arrendador") +
  scale_x_discrete(labels = c(levels(datos_pf$sexo_arrendador), "No especificado"))+
  # scale_x_discrete(labels = function(x)              #forma alternativa de asignar etiquetas
  #   ifelse(is.na(x), "No especificado", c("F" = "Mujer", "V" = "Hombre"))) +
  theme_minimal()

# Gráfico SIN valores faltantes

ggplot(drop_na(datos_pf, sexo_arrendador),
       aes(x = sexo_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Sexo Arrendador") +
  scale_x_discrete(labels = c(levels(datos_pf$sexo_arrendador), "No especificado"))+
  theme_minimal()



# Otra forma de realizar el grafíco de proporciones
#
# # Calcular las proporciones del campo sexo_arrendador
# proporcion_sexo <- datos %>%
#   count(sexo_arrendador) %>%
#   mutate(proporcion = n / sum(n))
# 
# # Crear el gráfico de barras con las proporciones
# ggplot(proporcion_sexo, aes(x = sexo_arrendador, y = proporcion)) +
#   geom_bar(stat = "identity", fill = "cornsilk1", color = "cornsilk2") +
#   geom_text(aes(label = scales::percent(proporcion)), vjust = 0.5, color = "cornsilk4") +
#   labs(x = "Sexo del Arrendador",
#        y = "Proporción",
#        title = "Proporción de Sexo del Arrendador")+
#   theme_minimal()
# 
# rm(Fianzas_viviendas_pf)





####################   tipo_entidad_arrendador       ##################

datos_pj <- datos %>%
  filter(tipo_persona_arrendador == "Jurídica")
tabla <- pinta_tabla(datos_pj,
                     "tipo_entidad_arrendador", "Tipo Entidad Arrendador")
print(tabla)
tabla <- pinta_tabla(datos_pj,
                     "tipo_entidad_arrendador", "Tipo Entidad Arrendador", orden ="SI")
print(tabla)


# pinta la grafica

plot <- ggplot(datos_pj, aes(x = tipo_entidad_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador") +
  scale_x_discrete(labels = levels(datos_pj$tipo_entidad_arrendador))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot)
plotly::ggplotly(plot)

ggplot(datos_pj, aes(x = tipo_entidad_arrendador ))  +
                       geom_bar(fill = "cornsilk1", color = "cornsilk2") +
                       geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
                       labs(x = "", 
                            y = "Frecuencia",
                            title = "Tipo Entidad Arrendador") +
                       scale_x_discrete(labels = str_wrap(levels(datos_pj$tipo_entidad_arrendador), width = 20))+
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1))
                     

######## 2da version con texto corto, columnas ordenadas por frecuencia

primera_letra <- substr(datos_pj$tipo_entidad_arrendador,1,1) %>% as.factor()
ggplot(datos_pj, aes(x = (fct_infreq(tipo_entidad_arrendador)))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador") +
  scale_x_discrete(labels = levels(primera_letra))+
  theme_minimal()

# Ahora agrupo factores para quedarme con los más frecuentes (4 en este caso)
N_grupos <- 4 # número de factores que dejo 
# Dejo solo los 4 tipos más frecuentes 
datos_pj <- datos_pj %>% 
    mutate(tipo_pj_agr = fct_lump(
      tipo_entidad_arrendador,
      n = N_grupos,
      other_level = "Resto"
    ))


tabla <- pinta_tabla(datos_pj,
                     "tipo_pj_agr", 
                     "Tipo Entidad Arrendador más frecuentes", 
                     orden ="SI")
print(tabla)

primera_letra <- ifelse(datos_pj$tipo_pj_agr == "Resto",
                        "Resto",
                        substr(datos_pj$tipo_pj_agr,1,1) )  %>%
    as.factor()

# Ordeno los factores por frecuencia de los factores y me llevo el factor "Resto" al final, detrás del penúltimo
posicion_penultima <- length(levels(primera_letra)) - 1
orden_factores <- fct_relevel(fct_infreq(primera_letra), "Resto", after = posicion_penultima)

ggplot(datos_pj, aes(x = orden_factores)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador")+
  theme_minimal()

#Ahora igual pero con el nombre largo
# Ordeno los factores por frecuencia de los factores y me llevo el factor "Resto" al final, detrás del penúltimo
posicion_penultima <- length(levels(datos_pj$tipo_pj_agr)) - 1
orden_factores <- fct_relevel(fct_infreq(datos_pj$tipo_pj_agr), "Resto", after = posicion_penultima)

ggplot(datos_pj, aes(x = orden_factores)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador")+
  scale_x_discrete(labels = str_wrap(levels(datos_pj$tipo_pj_agr), width = 20))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




####################   sexo_arrendatario       ##################


Resumen_basico(datos$sexo_arrendatario)
tabla <- pinta_tabla(datos, "sexo_arrendatario", "Sexo Arrendatario")
print(tabla)

tabla <- pinta_tabla3(datos, "sexo_arrendatario", "Sexo Arrendatario")
print(tabla)

# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = sexo_arrendatario)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +0.5, size = 3,
            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Sexo del Arrendatario") +
  scale_x_discrete(labels = c(levels(datos$sexo_arrendatario), "No especificado"))+
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = sexo_arrendatario)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Sexo del Arrendatario") +
  scale_x_discrete(labels = c(levels(datos$sexo_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()

# Gráfico de barras de las proporciones sin contabilizar "No Especificado"


ggplot(drop_na(datos, sexo_arrendatario), aes(x = sexo_arrendatario)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Sexo del Arrendatario") +
  #scale_x_discrete(labels = c(levels(datos$sexo_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()



####################   nacionalidad_arrendatario       ##################

Resumen_basico(datos$nacionalidad_arrendatario)
tabla <- pinta_tabla(datos, "nacionalidad_arrendatario", "Nacionalidad Arrendatario")
print(tabla)


# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = nacionalidad_arrendatario)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +0.5, size = 3,
            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Nacionalidad Arrendatario") +
  #scale_x_discrete(labels = c(levels(datos$nacionalidad_arrendatario), "No especificado"))+
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = nacionalidad_arrendatario)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Nacionalidad Arrendatario") +
  #scale_x_discrete(labels = c(levels(datos$nacionalidad_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()







####################   tipo_de_arrendamiento       ##################

Resumen_basico(datos$tipo_de_arrendamiento)
tabla <- pinta_tabla(datos, "tipo_de_arrendamiento", "Tipo de Arrendamiento")
print(tabla)

datos <- datos %>%
  mutate (  tipo_de_arrendamiento = case_when(     #otra forma podría ser usando na_if()
    tipo_de_arrendamiento == "AMUEBLADO"       ~ "Amueblado", 
    tipo_de_arrendamiento == "SIN AMUEBLAR"    ~ "Sin Amueblar",
    TRUE                                 ~ NA_character_) , 
    tipo_de_arrendamiento = factor(tipo_de_arrendamiento))

tabla <- pinta_tabla(datos,
                     campo = "tipo_de_arrendamiento",
                     campo_descriptivo = "Tipo de arrendamiento",
                     orden = "SI")
print(tabla)

tabla <- pinta_tabla3(datos,
                      campo = "tipo_de_arrendamiento",
                      campo_descriptivo = "Tipo de arrendamiento",
                      orden = "SI")
print(tabla)



# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = tipo_de_arrendamiento)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +0.5, size = 3,
            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo de Arrendamiento") +
  scale_x_discrete(labels = c(levels(datos$tipo_de_arrendamiento), "No especificado"))+
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = tipo_de_arrendamiento)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Tipo de Arrendamiento") +
  scale_x_discrete(labels = c(levels(datos$tipo_de_arrendamiento), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()


# Gráfico de barras de las proporciones con etiquetas de altura sin no especificados
ggplot(drop_na(datos, tipo_de_arrendamiento), aes(x = tipo_de_arrendamiento)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Tipo de Arrendamiento") +
  scale_x_discrete(labels = c(levels(datos$tipo_de_arrendamiento), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()





####################   provincia_806 y municipio_806       ##################

tabla <- pinta_tabla(datos,
                     "provincia_806", "Provincia Modelo 806")
print(tabla)

tabla <- pinta_tabla(datos,
                     "provincia_806", "Provincia Modelo 806", orden ="SI")

print(tabla)



# pinta la grafica

ggplot(datos, aes(x = provincia_806)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Provincia Modelo 806") +
  #scale_x_discrete(labels = levels(datos$provincia_806))+
  theme_minimal()#+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))


######## 2da version con columnas ordenadas por frecuencia

ggplot(datos, aes(x = fct_infreq(provincia_806))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Provincia Modelo 806") +
  theme_minimal()#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = provincia_806)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Provincia Modelo 806") +
  #scale_x_discrete(labels = levels(datos$provincia_806))+
  theme_minimal()
    

# Gráfico de barras de las proporciones con etiquetas y ordenados por tamaño

ggplot(datos, aes(x = fct_infreq(provincia_806))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Provincia Modelo 806") +
  #scale_x_discrete(labels = levels(datos$provincia_806))+
  theme_minimal()



# Gráficos de distribución de casos por municipios

ggplot(datos, aes(x = fct_infreq(municipio_806))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  #geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = Etiqueta("municipio_806")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Construir el ranking de los municipios con más casos
n_ranking <- 25
munic_25_mayores <- datos %>% 
  group_by(municipio_806) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(n_ranking) %>% 
  mutate(municipio_806 = factor(municipio_806,
                    levels = munic_25_mayores$municipio_806[order(munic_25_mayores$n)])
  )

ggplot(munic_25_mayores, aes(x = municipio_806, y = n)) +
  geom_bar(stat="identity", fill = "cornsilk1", color = "cornsilk2") +
  #geom_text(stat = "count", aes(label = ..count..), vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = Etiqueta("municipio_806")) +
  coord_flip() +
  theme_minimal() 
  


####################   MAPAS       ##################
# Los mapas los he preparado con ggplot, con tmap, con tmap y pasado a leaflet
# y en leaflet desde cero.
# Se encuentran en los ficheros "mapas ggplot.R", "mapas tmap.R"  y "mapas leaflet.R"





####################   duracion_contrato_años       ##################

# Ver en R epidemiologia 8 Limpieza de datos y funciones básicas.
# un enfoque con age_categories del paquete epikit
Resumen_basico(datos$duracion_contrato_años)

ggplot(datos) +
  geom_boxplot(aes(y = duracion_contrato_años),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Duración del contrato", y = "Años") +
  theme_minimal()

ggplot(datos) +
  geom_density(aes(x = duracion_contrato_años),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Duración del contrato", y = "Años") +
  theme_minimal()


ggplot(datos, aes(x = duracion_contrato_años)) +
  geom_histogram(bins = 40, fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Duración contrato (años)", y = "Casos") +
  theme_minimal() 


ggplot(datos, aes(x = duracion_contrato_años)) +
  geom_histogram(breaks = seq(0,40), 
                 fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Duración contrato (años)", y = "Casos") +
  theme_minimal() 


#Alternativo con indicación de la amplitud de los intervalos
max <- max(datos$duracion_contrato_años)
cortes <- c(seq(0, 9, 1),max(datos$duracion_contrato_años))
etiquetas <- c(seq(1,9,1),">10")
ggplot(datos,
       aes(x = cut(duracion_contrato_años, breaks = cortes, include.lowest = TRUE))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2", stat = "count", na.rm = FALSE ) +
  labs(x = "Duración contrato (años)", y = "Casos") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5,
            size = 3, 
            color = "cornsilk4") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas)+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #coord_cartesian(xlim = c(0, 10))+
  coord_cartesian(ylim = c(0, 32000))


#Alternativo con indicación de la amplitud de los intervalos y porcentajes
max <- max(datos$duracion_contrato_años)
anyo_agrupa <- 5  # A partir de 5 años los agrupa en una sola barra
cortes <- c(seq(0, anyo_agrupa, 1),max(datos$duracion_contrato_años))
etiquetas <- c(seq(1,anyo_agrupa,1),"Más")
anyos <-  cut(datos$duracion_contrato_años, breaks = cortes, include.lowest = TRUE)
ggplot(datos,
       aes(x = anyos)) +
  geom_bar(aes (y = ..count../sum(..count..)),
           fill = "cornsilk1",
           color = "cornsilk2",
           stat = "count",
           na.rm = FALSE ) +   # no produce ningún efecto aunque el manual diga lo contario
  labs(x = "Duración contrato (años)", y = "Casos") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4")  +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas)

####################   Fecha de devengo  ####
#esta variable es de tipo fecha, se describe de otra forma al resto
#Hmisc::describe(datos$fecha_devengo)

# Análisis por días a lo largo del año
tabla <- pinta_tabla(datos, "fecha_devengo", "Fecha de inicio de contrato")
print(tabla)

# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = fecha_devengo)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  # geom_text(stat = "count", aes(label = ..count..),
  #           vjust = +0.5, size = 3,
  #           color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Fecha de inicio de contrato") +
  theme_minimal()



# Análisis por meses 
datos$mes_devengo <- lubridate::month(datos$fecha_devengo,
                                      label = TRUE,
                                      abbr = FALSE)

#Resumen_basico(datos$mes_devengo) # No tiene sentido con campo de tipo fecha
tabla <- pinta_tabla(datos, "mes_devengo", "Mes de inicio de contrato")
print(tabla)

datos$mes_devengo <- lubridate::month(datos$fecha_devengo,
                                      label = TRUE,
                                      abbr = TRUE)

# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = mes_devengo)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +0.5, size = 3,
            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Mes de inicio contrato") +
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = mes_devengo)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Mes de inicio contrato") +
  #scale_x_discrete(labels = c(levels(datos$nacionalidad_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()





# Análisis por días del mes
datos$dia_devengo <- lubridate::day(datos$fecha_devengo)

#Resumen_basico(datos$mes_devengo) # No tiene sentido con campo de tipo fecha
tabla <- pinta_tabla(datos, "dia_devengo", "Día del mes que inicia de contrato")
print(tabla)


# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = dia_devengo)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +0.5, size = 3,
            color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Día del mes que inicia de contrato") +
  theme_minimal()

# Gráfico de barras de las proporciones con etiquetas de altura
ggplot(datos, aes(x = dia_devengo)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Día del mes que inicia de contrato") +
  #scale_x_discrete(labels = c(levels(datos$nacionalidad_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()


####################   num_habitaciones  ####

Resumen_basico(datos$num_habitaciones)

ggplot(datos, aes(x = num_habitaciones)) +
  geom_histogram(aes(y = ..density..), bins = 25, fill = "steelblue", color = "cyan") +
  geom_density(adjust = 6, color = "red") +
  labs(x = "Nº Habitaciones", y = "%", title = "Número de habitaciones") +
  scale_y_continuous(labels = scales::percent_format(scale = 100))+  # Escala en porcentaje
  theme_minimal() 



ggplot(datos) +
  geom_boxplot(aes(x="", y = num_habitaciones),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Número de habitaciones", y = NULL, title = "Diagrama") +
  theme_minimal()


ggplot(datos) +
  geom_density(aes(x = num_habitaciones),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Número de habitaciones", y = "Frecuencia") +
  theme_minimal()


ggplot(datos, aes(x = num_habitaciones)) +
  geom_histogram(aes(y = ..density..),bins = 25, fill = "cornsilk1", color = "cornsilk2") +
  geom_density(adjust = 6, color = "red") +
  labs(x = "Duración contrato (años)", y = "Frecuencia") +
  theme_minimal() 


ggplot(datos, aes(x = num_habitaciones)) +
  geom_histogram(breaks = seq(0,25), 
                 fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Número de habitaciones", y = "Casos") +
  theme_minimal() 


#Alternativo con indicación de la amplitud de los intervalos
max <- max(datos$num_habitaciones,  na.rm = TRUE)
cortes <- c(seq(0, 9, 1),max(datos$num_habitaciones))
etiquetas <- c(seq(1,9,1),">10")
ggplot(datos,
       aes(x = cut(num_habitaciones, breaks = cortes, include.lowest = TRUE))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2", stat = "count", na.rm = FALSE ) +
  labs(x = "Nº de habitaciones", y = "Casos") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5,
            size = 3, 
            color = "cornsilk4") +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas)+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #coord_cartesian(xlim = c(0, 10))+
  coord_cartesian(ylim = c(0, 32000))


#Alternativo con indicación de la amplitud de los intervalos y porcentajes
max <- max(datos$num_habitaciones,  na.rm = TRUE)
habit_agrupa <- 5  # A partir de 5 años los agrupa en una sola barra
cortes <- c(seq(0, habit_agrupa, 1),max(datos$num_habitaciones))
etiquetas <- c(seq(1,habit_agrupa,1),"Más")
anyos <-  cut(datos$num_habitaciones, breaks = cortes, include.lowest = TRUE)
ggplot(datos,
       aes(x = anyos)) +
  geom_bar(aes (y = ..count../sum(..count..)),
           fill = "cornsilk1",
           color = "cornsilk2",
           stat = "count",
           na.rm = FALSE ) +   # no produce ningún efecto aunque el manual diga lo contario
  labs(x = "Nº de habitaciones", y = "%") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4")  +
  theme_minimal() +
  scale_x_discrete(labels = etiquetas)
























por aqui voy 19/08/2023
####################   importe_de_la_renta     #####

Resumen_basico(datos$importe_de_la_renta)

datos %>% summarise(Media = mean(importe_de_la_renta, na.rm = TRUE),
                    Desv.Típica = sqrt(var(importe_de_la_renta, na.rm = TRUE)),
                    Moda = modeest::mfv(importe_de_la_renta)
                    # Mínimo=min(importe_de_la_renta, na.rm = TRUE),
                    # Q10 = quantile(importe_de_la_renta, 0.1, na.rm = TRUE),
                    # Q20 = quantile(importe_de_la_renta, 0.2, na.rm = TRUE),
                    # Q30 = quantile(importe_de_la_renta, 0.3, na.rm = TRUE),
                    # Q40 = quantile(importe_de_la_renta, 0.4, na.rm = TRUE),
                    # Q50 = quantile(importe_de_la_renta, 0.5, na.rm = TRUE),
                    # Q60 = quantile(importe_de_la_renta, 0.6, na.rm = TRUE),
                    # Q70 = quantile(importe_de_la_renta, 0.7, na.rm = TRUE),
                    # Q80 = quantile(importe_de_la_renta, 0.8, na.rm = TRUE),
                    # Q90 = quantile(importe_de_la_renta, 0.9, na.rm = TRUE),
                    # Máximo = max(importe_de_la_renta, na.rm = TRUE)
                    ) %>% 
  pivot_longer(everything(),names_to = "Medida", values_to = "Valor") %>% 
  flextable() %>% 
  colformat_double(digits=1) %>% 
  #fontsize(size=12,part="all") %>% 
  autofit() %>% 
  set_caption("Descripción de la variable")

datos %>% summarise(#Media = mean(importe_de_la_renta, na.rm = TRUE),
                    #Desv.Típica = sqrt(var(importe_de_la_renta, na.rm = TRUE)),
                    #Mínimo=min(importe_de_la_renta, na.rm = TRUE),
                    Q10 = quantile(importe_de_la_renta, 0.1, na.rm = TRUE),
                    Q20 = quantile(importe_de_la_renta, 0.2, na.rm = TRUE),
                    Q30 = quantile(importe_de_la_renta, 0.3, na.rm = TRUE),
                    Q40 = quantile(importe_de_la_renta, 0.4, na.rm = TRUE),
                    Q50 = quantile(importe_de_la_renta, 0.5, na.rm = TRUE),
                    Q60 = quantile(importe_de_la_renta, 0.6, na.rm = TRUE),
                    Q70 = quantile(importe_de_la_renta, 0.7, na.rm = TRUE),
                    Q80 = quantile(importe_de_la_renta, 0.8, na.rm = TRUE),
                    Q90 = quantile(importe_de_la_renta, 0.9, na.rm = TRUE),
                    Máximo = max(importe_de_la_renta, na.rm = TRUE)
) %>% 
  #pivot_longer(everything(),names_to = "Medida", values_to = "Valor") %>% 
  flextable() %>% 
  colformat_double(digits=0) %>% 
  #fontsize(size=12,part="all") %>% 
  autofit() %>% 
  set_caption("Descripción de la variable")


Renta <-table(cut(datos$importe_de_la_renta,
               breaks = c(seq(0, quantile["90%"]*1.5,50),max(datos$importe_de_la_renta, na.rm = TRUE)),
               right = FALSE,    #Intervalos cerrados por la izquierda
               include.lowest = TRUE,  # Para que incluya el valor máximo
               dig.lab = 10))  #cerrados por la izquierda

Renta %>% as.data.frame() %>% 
  rename(Intervalo = Var1, Casos = Freq) %>%
  flextable()


ggplot(datos) +
  geom_boxplot(aes(x="", y = importe_de_la_renta),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "", y = "Importe de la renta") +
  theme_minimal()

ggplot(datos) +
  geom_boxplot(aes(x="", y = importe_de_la_renta),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "", y = "Importe de la renta") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 2000)) 

ggplot(datos) +
  geom_boxplot(aes(x="", y = importe_de_la_renta),
               fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "", y = "Importe de la renta") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1000)) 


# ggplot(datos, aes(x = importe_de_la_renta)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 500, 
#                  fill = "cornsilk1", color = "cornsilk2") +
#   #geom_density(adjust = 1, color = "red") +
#   labs(x = "Importe de la renta", y = "%", title = "Importe de la renta") +
#   theme_minimal() +
#   coord_cartesian(xlim = c(0, 2000)) +
#   scale_y_continuous(labels = percent_format(scale = 100))
# No me gusta porque no se contrala cuales son los extremos de los intervalos

table(cut(datos$importe_de_la_renta,
          breaks = c(seq(0, quantile["90%"]*1.5,50),Inf),
          right = FALSE))  #cerrados por la izquierda



valor_maximo <- max(hist(datos$importe_de_la_renta,
                         breaks = seq(0, max(datos$importe_de_la_renta), 50), 
                         plot = FALSE)$counts)

grafico <-
ggplot(datos, aes(x = importe_de_la_renta)) +
  geom_histogram(breaks = seq(0,max(datos$importe_de_la_renta, rm.na = TRUE),50),
                 closed = c("left"),   #importante, los intervalos cerrados por la izquierda
                 fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Importe de la renta (€/mes)", y = "Casos") +
  #tema_ams +
  scale_x_continuous(breaks = seq(0, max(datos$importe_de_la_renta, na.rm = TRUE),5000))

grafico
quantile <- quantile(datos$importe_de_la_renta, prob = seq(0,1,0.1))
names <- paste0("Q", seq(0,100,10))
grafico + geom_vline(xintercept=quantile, color="lightsalmon3" )

quantile <- quantile(datos$importe_de_la_renta, prob = seq(0,1,0.1))

grafico <-
ggplot(datos, aes(x = importe_de_la_renta)) +
  geom_histogram(breaks = seq(0,max(datos$importe_de_la_renta, rm.na = TRUE),50),
                 closed = c("left"),
                 fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Importe de la renta (€/mes)", y = "Casos") +
  #tema_ams +
  coord_cartesian(xlim = c(0, quantile["90%"]*1.5)) +
  scale_x_continuous(breaks = seq(0, max(datos$importe_de_la_renta, na.rm = TRUE),100))

grafico


# Añadimos los deciles
grafico + geom_vline(xintercept=quantile, color="lightsalmon3" ) +
  annotate("text",                        # Add text for mean
           color="lightsalmon3",
           x = quantile,
           y = valor_maximo, #-500 -seq(0,1,0.1)*1000,
           hjust =-0.1,
           label = paste0(names,"\n",quantile) ,
           family = "sans", size = 8/.pt
  )




# PAra indicar en el gráfico cuántos valores se quedan por debajo de la media
# Obtengo la distribución empírica
ecdf_func <- ecdf(datos$importe_de_la_renta)

#y calculo el porcentaje de valores inferiores a la media
# Calcular en qué rango de probabilidad cae el valor
prob <- ecdf_func( mean(datos$importe_de_la_renta)) * 100
prob <- round(prob, 1) # y lo redondeo

# El tamaño de la fuente se expresa en mm
# si queremos tamaño 12, indicamos 12/.pt
# .pt = 2.845276, .pt es 0.35 mm

grafico + geom_vline(xintercept=quantile["50%"], color="lightsalmon3" ) +
  annotate("text",                        # Add text for mean
           color="lightsalmon3",
           x = quantile["50%"],
           y = valor_maximo, #-500 -seq(0,1,0.1)*1000,
           hjust = 1,
           label = paste0("Mediana","\n",quantile["50%"]) ,
           family = "sans", size = 8/.pt
           ) +
  geom_vline(xintercept=mean(datos$importe_de_la_renta), color="red" ) +
  annotate("text",                        # Add text for mean
           color="red",
           x = mean(datos$importe_de_la_renta), 
           y = valor_maximo, #-500 -seq(0,1,0.1)*1000,
           hjust = -0.1,
           vjust = 1,
           label = paste0("Media","\n",
                          sprintf("%.1f", mean(datos$importe_de_la_renta)),"\n",
                          "Porc < Media: ", prob,"%") ,
           family = "sans", size = 8/.pt
  ) 




# Expresado en porcentajes
ggplot(datos, aes(x = importe_de_la_renta)) +
  geom_histogram(breaks = seq(0,2000,50),closed = c("left"), aes(y = (..count..)/sum(..count..)),
                 fill = "cornsilk1", color = "cornsilk2") +
  labs(x = "Importe de la renta (€/mes)", y = "Casos") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100))





library(ggplot2)

# Obtener todos los valores por defecto de los parámetros de temas
valores_por_defecto <- theme_get()

# Imprimir todos los valores por defecto
print(valores_por_defecto)






# Tipo de actualización #####
Ver si se pueden agruapar valores o ver algo de valor en la variable

# Importe de la fianza
Hacer analisis de los casos en que la fianza es distinta a la renta para ver cuantos casos responden cambios en el contrato















