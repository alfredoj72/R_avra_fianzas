
# Realiza un analisis descriptivo de las variables.

paquetes_necesarios = c("sf","tidyverse","flextable") # c( "ggplot2","classInt") 
for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)

# parto
rm(list =ls())
load("./datos_output/avra_catastro_2022.RData")
datos <- avra_catastro_2022 ; rm(avra_catastro_2022)
avra_datos_originales <- datos[["originales"]]
# avra_catastro <- datos[["avra_catastro"]]
# tabla_frecuencias  <- datos[["tabla_frecuencias"]]
# tabla_frecuencias_final  <- datos[["tabla_frecuencias_final"]]
# Fianzas_casan_1_vivienda <- datos[["Fianzas_casan_1_vivienda"]]
# Fianzas_no_casan_catastro <- datos[["Fianzas_no_casan_catastro"]]
# Fianzas_casan_distintas_viviendas <- datos[["Fianzas_casan_distintas_viviendas"]]
# Fianzas_casan_distintas_viviendas_case <- datos[["Fianzas_casan_distintas_viviendas_case"]]


load("./datos_output/datos_para_analisis_2022.RData")
Fianzas_viviendas <- datos_para_analisis_2022[["Fianzas_viviendas"]]
Fianzas_viviendas <- st_drop_geometry(Fianzas_viviendas)
rm(datos_para_analisis_2022)



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


#Fianzas_viviendas <- Fianzas_viviendas %>% select(-num_repeticiones)

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
           Porcentaje_v = 100 * Casos_v / sum(Casos_v),
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
               Porcentaje_v = sum(Porcentaje_v))
  
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
#

# Si convierto los NA en un nivel del factor comenzará a ordenarse como el resto
# de los niveles, si lo dejo como NA se van siempre al último de la lista.

Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate (tipo_persona_arrendador = as.character(tipo_persona_arrendador),
          tipo_entidad_arrendador = as.factor(tipo_entidad_arrendador),
          sexo_arrendador = as.character(sexo_arrendador))

Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate (tipo_persona_arrendador = replace_na(tipo_persona_arrendador,"NEspec")) %>% # no hay casos
  mutate (tipo_persona_arrendador = factor(tipo_persona_arrendador, 
                                   levels = c("F", "J"), 
                                   labels = c("Física", "Jurídica")),
          sexo_arrendador = factor(sexo_arrendador,
                                   levels = c("M", "V"),
                                   labels = c("Mujeres", "Hombres")))


####################   tipo_persona_arrendador       ##################

#Resumen_basico(Fianzas_viviendas$tipo_persona_arrendador)
tabla <- pinta_tabla(Fianzas_viviendas, "tipo_persona_arrendador", "Tipo Persona Arrendador")
print(tabla)

tabla <- pinta_tabla3(Fianzas_viviendas, "tipo_persona_arrendador", "Tipo Persona Arrendador")
print(tabla)

# Crear el gráfico de barras con etiquetas de altura
ggplot(Fianzas_viviendas, aes(x = tipo_persona_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo de persona del arrendador") +
  theme_minimal()


# Calcular las proporciones del campo sexo_arrendador
proporcion <- Fianzas_viviendas %>%
  count(tipo_persona_arrendador) %>%
  mutate(proporcion = n / sum(n))

# Crear el gráfico de barras con las proporciones
ggplot(proporcion, aes(x = tipo_persona_arrendador, y = proporcion)) +
  geom_bar(stat = "identity", fill = "cornsilk1", color = "cornsilk2") +
  geom_text(aes(label = scales::percent(proporcion)), vjust = 0.5, color = "cornsilk4") +
  labs(x = "Tipo de Persona del Arrendador",
       y = "Proporción",
       title = "Tipo de Persona del Arrendador")




####################   sexo_arrendador       ##################

Fianzas_viviendas_pf <- Fianzas_viviendas %>%
                        filter(tipo_persona_arrendador == "Física")
tabla <- pinta_tabla(Fianzas_viviendas_pf,
                     campo = "sexo_arrendador",
                     campo_descriptivo = "Sexo Arrendador",
                     orden = "SI")
print(tabla)

tabla <- pinta_tabla3(Fianzas_viviendas_pf,
                     campo = "sexo_arrendador",
                     campo_descriptivo = "Sexo Arrendador",
                     orden = "SI")
print(tabla)

# Gráfico incluyendo valores faltantes
ggplot(Fianzas_viviendas_pf, aes(x = sexo_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Sexo Arrendador") +
  scale_x_discrete(labels = c(levels(Fianzas_viviendas_pf$sexo_arrendador), "No especificado"))+
  # scale_x_discrete(labels = function(x) 
  #   ifelse(is.na(x), "No especificado", c("F" = "Física", "J" = "Jurídica"))) +
  theme_minimal()

# Gráfico SIN valores faltantes

ggplot(drop_na(Fianzas_viviendas_pf, sexo_arrendador),
       aes(x = sexo_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Sexo Arrendador") +
  scale_x_discrete(labels = c(levels(Fianzas_viviendas_pf$sexo_arrendador), "No especificado"))+
  theme_minimal()


# Calcular las proporciones del campo sexo_arrendador
proporcion_sexo <- Fianzas_viviendas %>%
  count(sexo_arrendador) %>%
  mutate(proporcion = n / sum(n))

# Crear el gráfico de barras con las proporciones
ggplot(proporcion_sexo, aes(x = sexo_arrendador, y = proporcion)) +
  geom_bar(stat = "identity", fill = "cornsilk1", color = "cornsilk2") +
  geom_text(aes(label = scales::percent(proporcion)), vjust = -0.5, color = "cornsilk4") +
  labs(x = "Sexo del Arrendador",
       y = "Proporción",
       title = "Proporción de Sexo del Arrendador")

rm(Fianzas_viviendas_pf)





####################   tipo_entidad_arrendador       ##################

Fianzas_viviendas_pj <- Fianzas_viviendas %>%
  filter(tipo_persona_arrendador == "Jurídica")
tabla <- pinta_tabla(Fianzas_viviendas_pj,
                     "tipo_entidad_arrendador", "Tipo Entidad Arrendador")
print(tabla)
tabla <- pinta_tabla(Fianzas_viviendas_pj,
                     "tipo_entidad_arrendador", "Tipo Entidad Arrendador", orden ="SI")
print(tabla)


# pinta la grafica

ggplot(Fianzas_viviendas_pj, aes(x = tipo_entidad_arrendador)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador") +
  scale_x_discrete(labels = levels(Fianzas_viviendas_pj$tipo_entidad_arrendador))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


######## 2da version con texto corto, columnas ordenadas por frecuencia

primera_letra <- substr(Fianzas_viviendas_pj$tipo_entidad_arrendador,1,1) %>% as.factor()
ggplot(Fianzas_viviendas_pj, aes(x = (fct_infreq(tipo_entidad_arrendador)))) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador") +
  scale_x_discrete(labels = levels(primera_letra))+
  theme_minimal()

N_grupos <- 4 # número de factores que dejo 
# Dejo solo los 4 tipos más frecuentes 
Fianzas_viviendas_pj <- Fianzas_viviendas_pj %>% 
    mutate(tipo_pj_agr = fct_lump(
      tipo_entidad_arrendador,
      n = N_grupos,
      other_level = "Resto"
    ))


tabla <- pinta_tabla(Fianzas_viviendas_pj,
                     "tipo_pj_agr", 
                     "Tipo Entidad Arrendador más frecuentes", 
                     orden ="SI")
print(tabla)

primera_letra <- ifelse(Fianzas_viviendas_pj$tipo_pj_agr == "Resto",
                        "Resto",
                        substr(Fianzas_viviendas_pj$tipo_pj_agr,1,1) )  %>%
    as.factor()

# Ordeno los factores por frecuencia de los factores y me llevo el factor "Resto" al final, detrás del penúltimo
posicion_resto <- length(levels(primera_letra)) - 1
orden_factores <- fct_relevel(fct_infreq(primera_letra), "Resto", after = posicion_resto)

ggplot(Fianzas_viviendas_pj, aes(x = orden_factores)) +
  geom_bar(fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = "Tipo Entidad Arrendador")+
  theme_minimal()



por aqui voy.
Usar  fct_explicit_na() para definir el valor de los NA y que en los graficos y tablas se siga quedando al final
https://epirhandbook.com/es/factors.html

Fianzas_viviendas %>% 
  tabyl(sexo_arrendador)

Fianzas_viviendas %>% 
  mutate(sexo_arrendador = fct_na_value_to_level(sexo_arrendador, level  = "Missing")) %>% 
  tabyl(sexo_arrendador) 

kk <-Fianzas_viviendas %>% 
  mutate(sexo_arrendador = fct_na_value_to_level(sexo_arrendador, level  = "Missing")) 

levels(kk$sexo_arrendador)
sum(kk$sexo_arrendador == "Mujeres")
sum(is.na(kk$sexo_arrendador))

orden_factores <- fct_relevel(fct_infreq(kk$sexo_arrendador), "Mujeres", after = 2)

kk<-pinta_tabla(Fianzas_viviendas, "tipo_entidad_arrendador", "Tipo Entidad Arrendador")
kk
#.-.-.-.-.-.-
#sexo_arrendador

Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate (sexo_arrendador = as.character(sexo_arrendador))

#opcion 1
Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate (sexo_arrendador = replace_na(sexo_arrendador,"NEspec")) %>% 
  mutate (sexo_arrendador = factor(sexo_arrendador, 
                                   levels = c("M", "V","NEspec"), 
                                   labels = c("Mujer", "Varón","No Especificado")))
Resumen_basico(Fianzas_viviendas$sexo_arrendador)
ggplot(Fianzas_viviendas, aes(x = fct_infreq(sexo_arrendador)),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Sexo del arrendador",
       y = "Frecuencia", title = "Sexo del arrendador")+
  theme_minimal()

#opcion 2
Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate (sexo_arrendador = factor(sexo_arrendador, 
                                   levels = c("M", "V"), 
                                   labels = c("Mujer", "Varón")))


Resumen_basico(Fianzas_viviendas$sexo_arrendador)
ggplot(Fianzas_viviendas, aes(x = fct_infreq(sexo_arrendador)),) +
  geom_bar(fill = "cyan" , color = "steelblue")+  #(fill = "steelblue", bins = 2) +
  geom_text(stat = "count", aes(label = ..count..), vjust = +1.3, size = 3) +
  labs(x = "Sexo del arrendador",
       y = "Frecuencia", title = "Sexo del arrendador")+
   scale_x_discrete(labels = function(x) 
     ifelse(is.na(x), "No especificado", c("V" = "Varóncito", "M" = "Mujercita"))) +
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













