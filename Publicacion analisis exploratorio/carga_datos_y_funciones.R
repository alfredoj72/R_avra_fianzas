


#################  Carga información en memoria    ################

#rm(list =ls())
load("../datos_output/avra_catastro_2022.RData")
contenedor <- avra_catastro_2022
rm(avra_catastro_2022)
#avra_datos_originales <- contenedor[["originales"]]
#avra_catastro <- contenedor[["avra_catastro"]]
tabla_frecuencias  <- contenedor[["tabla_frecuencias"]]
tabla_frecuencias_final  <- contenedor[["tabla_frecuencias_final"]]
# Fianzas_casan_1_vivienda <- contenedor[["Fianzas_casan_1_vivienda"]]
# Fianzas_no_casan_catastro <- contenedor[["Fianzas_no_casan_catastro"]]
# Fianzas_casan_distintas_viviendas <- contenedor[["Fianzas_casan_distintas_viviendas"]]
# Fianzas_casan_distintas_viviendas_case <- contenedor[["Fianzas_casan_distintas_viviendas_case"]]

#datos <- Fianzas_casan_1_vivienda #datos <- avra_datos_originales
rm(contenedor)



# inicio con datos para el analisis
#rm(list =ls())
load("../datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["datos"]]
datos_originales <- datos_para_analisis_2022[["originales"]]
tabla_frecuencia_filtros <- datos_para_analisis_2022[["tabla_borrados"]]

datos_con_geometria <- datos
datos <- st_drop_geometry(datos)
rm(datos_para_analisis_2022)




# Leo las tablas de diccionario de datos y defino la función que recupera la 
# etiqueta de un campo a partir de su nombre
diccionario_campos <- read_excel("../datos_output/campos_avra_catastro_completados.xlsx") 

# Función que devuelve la etiqueta del campo a partir de su nombre
Etiqueta <- function(campo_input) {
  fila <- subset(diccionario_campos, campo == campo_input)
  
  if (nrow(fila) == 0) {
    mensaje <- paste("El campo", campo_input, "no se encontró")
    return(mensaje)
  }
  
  valor_salida <- fila$etiqueta
  return(valor_salida)
}
# Ej Etiqueta("stotalocal_14")

to_int <- function(num){
  num %>%
    formatC(format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
}
to_float <- function(num){
  num %>%
    formatC(format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}
to_porcentaje <- function(num){
  num %>%
    formatC(format = "f", digits = 2, big.mark = ".", decimal.mark = ",")
}

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
  theme(plot.background = element_rect (linewidth = 1, color ="blue", fill ="black"),
        text = element_text(size = 12, family = "Serif", color= "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour ="red"),
        panel.background = element_rect(fill ="green"))


### Definición de funciones y parámetros flextable ####
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
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4,
  text.align = "c",
  border.color = "grey60", # "#333333",
  padding = 4
)

# define el estilo del borde
border_style1 = officer::fp_border(color="grey60", width=2)
border_style2 = officer::fp_border(color="grey60", width=0.5)

# Función que calcula una tabla con recuentos de un solo campo y 
# la pinta con flextable
pinta_tabla <- function(datos,campo,campo_descriptivo,orden = "NO"){
  #browser()
  c <- campo
  campo <- datos[[campo]]
  
  # Si el campo no existe devulve mensaje de error en tabla
  if (is.null(campo)) {
    mensaje_error <- data.frame(campo = numeric(0)) %>%  flextable() %>%
      set_header_labels(campo = glue("El campo {c} no existe"))
    return(mensaje_error)
  }
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
  c <- campo
  campo <- datos[[campo]]
  
  # Si el campo no existe devulve mensaje de error en tabla
  if (is.null(campo)) {
    mensaje_error <- data.frame(campo = numeric(0)) %>%  flextable() %>%
      set_header_labels(campo = glue("El campo {c} no existe"))
    return(mensaje_error)
  }
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
  c <- campo
  campo <- datos[[campo]]
  
  # Si el campo no existe devulve mensaje de error en tabla
  if (is.null(campo)) {
    mensaje_error <- data.frame(campo = numeric(0)) %>%  flextable() %>%
      set_header_labels(campo = glue("El campo {c} no existe"))
    return(mensaje_error)
  }
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

grafico_barras <- function(df, variable, label = NULL){
  # Gráfico de barras con etiquetas de altura
  ggplot(df, aes(x = .data[[variable]])) +
    geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
    geom_text(stat = "count", aes(label = ..count..),
              vjust = +1, size = 3,
              color = "cornsilk4") +
    # geom_text(stat = "count", aes(y = (..count..), 
    #           label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
    #           vjust = +2.5, size = 3, color = "cornsilk4") +
    labs(x = "", 
         y = "Frecuencia",
         title = ifelse(is.null(label), Etiqueta(variable), label)) +
    theme_minimal()
}
