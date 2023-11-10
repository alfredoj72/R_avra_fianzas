# Este codigo se ha quedado obsoleto al elaborar GeneraSalidaBADEA

# Calculo de resumenes para comprobar los elementos que cumplen la condición del
# número mínimo de testigos para ser publicados.

# La función principal es obtiene_resumen_y_contabiliza 

# rm(list =ls())
# load("datos3.RData")

rm(list =ls())
load("./datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["Fianzas_viviendas"]]


calcula_resumen <- function(datos, campo_pivote, campo_resumen ) {
  campo_piv <- datos[[campo_pivote]]
  campo_res <- datos[[campo_resumen]]
  res <- data.frame(pivote = campo_piv, resumen = campo_res)
    resumen_datos <- res %>%
              group_by(pivote) %>%
                  summarize(
                    media = mean(resumen),
                    maxima = max(resumen),
                    minima = min(resumen),
                    mediana = median(resumen),
                    repeticiones = n(),
                    lista = list(resumen)
    ) %>%
    ungroup()
    
   return(resumen_datos)
}

campo_pivote <- "cod_ine"
campo_resumen <- "renta_m2"
calcula_resumen(datos,campo_pivote , campo_resumen)



# La misma función pasando los parámetros sin "
calcula_resumen2 <- function(datos, campo_pivote, campo_resumen ) {
  resumen_datos <- datos %>% 
           group_by({{campo_pivote}}) %>% 
           summarise(
             "media_{{campo_resumen}}" := mean({{campo_resumen}}),
             "minimo_{{campo_resumen}}" := min({{campo_resumen}}),
             "maximo_{{campo_resumen}}" := max({{campo_resumen}}),
             "mediana_{{campo_resumen}}" := median({{campo_resumen}}),
             "q25_{{campo_resumen}}" := quantile({{campo_resumen}},0.25),
             "q75_{{campo_resumen}}" := quantile({{campo_resumen}},0.75),
             "n_casos" = n()
           ) %>% 
  ungroup()
  
  return(resumen_datos)
}

datos <- st_drop_geometry(datos)

resumen_municipios <- calcula_resumen2(datos, cod_ine, renta_m2)
resumen_municipios


resumen_barrio <- calcula_resumen2(datos, barrio.distrito, renta_m2)
resumen_barrio


# Función para obtener el número de registros y el número de registros que cumplen la condición
obtener_num_registros <- function(df_list, condicion) {
  num_registros <- lapply(df_list, nrow)
  num_registros_condicion <- lapply(df_list, function(df) {
    nrow(filter(df, repeticiones >= condicion))
  })
  num_registros <- stack(num_registros)
  colnames(num_registros) <- c("total", "grupo")
  
  num_registros_condicion <- stack(num_registros_condicion)
  colnames(num_registros_condicion) <- c("condicion", "grupo")
  
  
  result <- left_join(num_registros, num_registros_condicion )
  result <- result %>% select(grupo, total, condicion)
  # result <- cbind(num_registros, num_registros_condicion)
  # result <- list(num_registros_total = num_registros, num_registros_condicion = num_registros_condicion)
  return(result)
}

datos <- st_drop_geometry(Fianzas_viviendas)
pivote <- "cod_ine"
resumen <- "renta_m2"

 resumen_municipios <- calcula_resumen(datos, pivote, resumen)
# nrow(resumen_municipios)
# nrow(resumen_municipios[resumen_municipios$repeticiones >= 10,])


# Parámetros para la función
condicion <- 10

datos$aux <- 1
lista_dataframes <- split(datos, datos$aux)

# lista_dataframes <- split(datos, datos$sexo_arrendatario)

lista_dataframes_r <- lapply(lista_dataframes, 
                             function(df) calcula_resumen(df, pivote, resumen))
view(lista_dataframes_r[[1]])
resultados <- obtener_num_registros(lista_dataframes_r, condicion)
view(resultados)





campo_grupo   <- "sexo_arrendatario"
campo_pivote  <- "cod_ine"
campo_resumen <- "renta_m2"
obtiene_resumen_y_contabiliza <- function( datos, grupo, pivote, res){
  campo_gru <- datos[[grupo]]
  # campo_piv <- datos[[pivote]]
  # campo_res <- datos[[res]]
  lista_dataframes <- split(datos, campo_gru)
  lista_dataframes_r <- lapply(lista_dataframes, 
                               function(df) calcula_resumen(df, pivote, res))
  resultados <- obtener_num_registros(lista_dataframes_r, condicion)
  return(list(lista_dataframes_r,resultados))
}

sol <- obtiene_resumen_y_contabiliza(datos, campo_grupo, campo_pivote, campo_resumen)
lista_de_dataframes <- sol[[1]]
resumen_datos_condicion <- sol[[2]]
resumen_datos_condicion








lista_dataframes <- split(datos, datos$tip_const4d_14)

# Usando lapply() para aplicar 'calcula_resumen' a cada DataFrame 
lista_dataframes_r <- lapply(lista_dataframes, 
                             function(df) calcula_resumen(df, pivote, resumen))

# Aplicar la función a la lista de DataFrames
resultados <- obtener_num_registros(lista_dataframes_r, condicion)
view(resultados)




pivote <- "barrio.nombre"
lista_dataframes <- split(datos, datos$aux)
lista_dataframes_r <- lapply(lista_dataframes, 
                             function(df) calcula_resumen(df, pivote, resumen))
resultados <- obtener_num_registros(lista_dataframes_r, condicion)
view(resultados)



lista_dataframes <- split(datos, datos$tipviv)
# Usando lapply() para aplicar 'calcula_resumen' a cada DataFrame 
lista_dataframes_r <- lapply(lista_dataframes, 
                             function(df) calcula_resumen(df, pivote, resumen))

# Aplicar la función a la lista de DataFrames
resultados <- obtener_num_registros(lista_dataframes_r, condicion)
view(resultados)




print(levels(datos$tipviv))
factor_recodificado <- recode(datos$tipviv, "21" = "2", "22" = "2")
print(levels(factor_recodificado))

lista_dataframes <- split(datos, factor_recodificado)
# Usando lapply() para aplicar 'calcula_resumen' a cada DataFrame 
lista_dataframes_r <- lapply(lista_dataframes, 
                             function(df) calcula_resumen(df, pivote, resumen))

# Aplicar la función a la lista de DataFrames
resultados <- obtener_num_registros(lista_dataframes_r, condicion)
view(resultados)





