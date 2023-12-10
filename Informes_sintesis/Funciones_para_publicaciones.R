

to_int <- function(num){
  round(num)
}

to_decimal <- function(num, decimales = 2){
  round(num,decimales) 
}



genera_cruce_variables <- function(df, campo_fijo = NULL, combinacion_campos){
  dimensiones <- c("anyo", "f.durac_contrato","f.renta_alq", "f.renta_m2",
                   "sexo_arrendador", "f.persona_fj", "sexo_arrendatario",
                   "nacionalidad_arrendatario", "tipo_de_arrendamiento",
                   "f.super","f.hab","f.tipolog", "f.antig_bi",
                   "f.tam_pob", "pota.unidad_territorial")
  # browser()
  # df <- df %>% 
  #   mutate(provincia_806 = as.character(provincia_806))
  resultado <- data.frame()
  parciales <- data.frame()
  for (campos  in (combinacion_campos)) {
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
                
                preciom2_M = to_decimal(median(renta_m2),1) ,
                preciom2_p25 = to_decimal(quantile(renta_m2, 0.25, type = 4),1),
                preciom2_p75 = to_decimal(quantile(renta_m2, 0.75, type = 4),1),
                
                superficie_M = to_int(median(stotalocal_14)),
                superficie_p25 = to_int(quantile(stotalocal_14, 0.25, type = 4)),
                superficie_p75  = to_int(quantile(stotalocal_14, 0.75, type = 4)),
                
                preciomes_M = to_int(median(importe_de_la_renta)),
                preciomes_p25 = to_int(quantile(importe_de_la_renta, 0.25, type = 4)),
                preciomes_p75 = to_int(quantile(importe_de_la_renta, 0.75, type = 4)),
                
                # campos = paste(campos_a_usar, collapse = " "),
                # pivote = paste(campo_fijo, collapse = " "),
                .groups = "drop") 
    
    resultado <- bind_rows(resultado, parciales)
  }
  resultado <- resultado %>%
    select(any_of(c(dimensiones, campo_fijo)), everything())
  
  # resultado <- resultado %>%
  #   mutate(across(n:last_col(), ~ ifelse(valor < 10, NA, .)))
  
  
  return(resultado)
}





genera_resumen_barrio <- function(datos){
  
  #> la capa de barrios no casa con la capa de muncipios de catastro de forma que 
  #> hay testigos que estan en un barrio de granada capital y segun catastro estan
  #> en el municipios de cenes de la vega. Esto genera problemas en la tabla que
  #> generamos con los datos de los barrios ya que aparecera en el listado el
  #> municipio de cenes de la vega sin infomración de barrios y en los barrios de
  #> granada aparecerá dos veces el barrio en cuestión, una por cada municipio
  #> Para evitar dicha situación elimino del listado los registros en los que
  #> el municipio según la capa de barrios sea distinto al municipio asignado por
  #> catastro
  datos_aux <- datos %>% filter (barrio.cod_ine == cod_ine)
  
  d_barrio <-  genera_cruce_variables(datos_aux, c("anyo", 
                                                   "provincia_806",
                                                   "cod_ine",
                                                   "nombre",
                                                   "barrio.codigo"),
                                      list(character(0))) %>% 
    filter(!is.na(barrio.codigo))  # Esta línea ya no es precisa ya que al filtra
  # que cod_ine case con barrio.cod_ine solo se
  # obtienen datos de barrios
  
  d_mun <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "codigo_ine", "nombre"),
                                   list(character(0)))
  
  # Voy a asegurarme de que TODOS los barrios salgan en el listado aunque no
  # tengan ninguna observación (testigo), es decir aunque n = 0
  if (!file.exists(here("datos_output","capas_para_mapas.Rdata"))) {
    source("Funciones.R")
    # Ejecuta el script para crear las capas 
    Pasar_capas_shp_a_R()
  } 
  
  # Carga las capas
  load(file = here("datos_output","capas_para_mapas.Rdata"))
  
  barrios <- barrios_sf %>% 
    st_drop_geometry() %>% 
    mutate (barrio.nombre_mun = municipio) %>% 
    select(cod_mun, barrio.codigo, barrio.nombre, barrio.nombre_mun)
  
  
  d_barrio <- left_join(barrios, 
                        d_barrio,
                        by = c("barrio.codigo" = "barrio.codigo")) 
  
  # Tomo solo los municipios presentes en la capa de barrios
  d_mun <- d_mun %>% 
    filter(nombre %in% unique(d_barrio$nombre)) %>% 
    mutate(bold = TRUE,
           desplaza = 0,
           cod_mun = codigo_ine)
  
  d_barrio <- d_barrio %>% 
    mutate(desplaza = 1)    
  
  t <- bind_rows(d_barrio, d_mun) %>% 
    mutate(nombre = coalesce(nombre, barrio.nombre_mun),
           territorio = coalesce(barrio.nombre, nombre),
           barrio.nombre = ifelse(is.na(barrio.nombre),"",barrio.nombre),
           n = ifelse(is.na(n), 0, n),
           across(preciom2_M:preciomes_p75, ~ ifelse(n<10, NA, .))) %>% 
    arrange(cod_mun, barrio.nombre) %>%   #ordena las filas de la tabla
    relocate(territorio ,bold, desplaza, .after = "anyo")
  return(t)
}


genera_resumen_municipios <- function(datos){
  d_mun <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre"),
                                   list(character(0)))
  
  d_prov <- genera_cruce_variables(datos, c("anyo","provincia_806"),
                                   list(character(0)))
  
  # d_and <- genera_cruce_variables(datos, c("anyo"),
  #                                 list(character(0)))
  
  
  d_mun <- d_mun %>% 
    mutate(desplaza = 3)%>% 
    filter(n >= 10) %>% 
    mutate(provincia_806 = as.character(provincia_806),
           nombre = as.character(nombre),
           nombre = ifelse(provincia_806 == nombre,
                           paste(nombre, " (capital)"),
                           nombre))
  
  d_prov <- d_prov %>% 
    mutate(bold = TRUE,
           desplaza = 1)
  
  # d_and <- d_and %>% 
  #   mutate(bold = TRUE,
  #          desplaza = 0)
  
  
  # t <- bind_rows(d_mun, d_prov, d_and) %>%
  #   mutate(territorio = coalesce(nombre, provincia_806, "Andalucía"),
  #          nombre = ifelse(is.na(nombre),"",nombre),
  #          provincia_806 = ifelse(is.na(provincia_806),"",provincia_806)) %>% 
  #   arrange(provincia_806, nombre) 
  
  t <- bind_rows(d_mun, d_prov) %>%
    mutate(territorio = coalesce(nombre, provincia_806),
           nombre = ifelse(is.na(nombre),"",nombre),
           provincia_806 = ifelse(is.na(provincia_806),"",provincia_806)) %>% 
    arrange(provincia_806, nombre)
  
  return(t)
}


genera_resumen_provincias <- function(datos){
 # browser()
  d_prov <- genera_cruce_variables(datos, c("anyo","provincia_806"),
                                   list(character(0)))
  
  d_and <- genera_cruce_variables(datos, c("anyo"),
                                  list(character(0)))
  
  d_prov <- d_prov %>% 
    mutate(desplaza = 1)
  
  d_and <- d_and %>% 
    mutate(bold = TRUE,
           desplaza = 0)
  
  
  t <- bind_rows( d_prov, d_and) %>%
    mutate(territorio = coalesce( provincia_806, "Andalucía"),
           provincia_806 = ifelse(is.na(provincia_806),"",provincia_806)) %>% 
    arrange(provincia_806) 
  
  return(t)
}





resumen_a_flex <- function(t){
  
  # Defino el flextable indicando las columnas a mostrar y quito bordes
  tft <- t %>% flextable(col_keys = c("territorio",
                                      "n",
                                      "preciom2_M",
                                      "preciom2_p25",
                                      "preciom2_p75",
                                      "superficie_M",
                                      "superficie_p25",
                                      "superficie_p75",
                                      "preciomes_M",
                                      "preciomes_p25",
                                      "preciomes_p75")) %>% 
    border_remove() 
  
  #Defino los textos de la cabecera de la tabla
  tft <- set_header_labels(tft,
                           territorio = "",
                           n = "Nº testigos",
                           preciom2_M = "M",
                           preciom2_p25 = "P25",
                           preciom2_p75 = "P75",
                           superficie_M = "M",
                           superficie_p25 = "P25",
                           superficie_p75 = "P75",
                           preciomes_M = "M",
                           preciomes_p25 = "P25",
                           preciomes_p75 = "P75")
  
  # Defino el tamaño del texto 
  tft <-fontsize(tft, size = 9, part = "all") 
  
  # Añado fila de encabezamiento
  tft <- tft %>% add_header_row(values = c("","", 
                                           "Renta (€/m2.mes)", "Superficie", "Cuantía (€/mes)" ),
                                colwidths = c(1,1,3,3,3))
  
  
  # Indico los tamaños de las columnas
  tft <- tft %>% 
    width(j = 1, width = 2.4) %>% 
    width(j = 2, width = 0.6) %>% 
    width(j = 3:11, width = .45)
  
  # Alineo el texto de las columnas con datos numéricos
  tft <- align(tft, part = "all", j = 2:11, align = "center") 
  
  # Añado líneas verticales separando los grupos de información
  tft <- tft %>%  hline(part = "header", i = 2, border = border_style1) 
  tft <- tft %>%  hline(part = "header", i = 1, j=3:11, border = border_style2) 
  tft <- tft %>%  hline_top(part = "header",  j=3:11, border = border_style2)
  
  # #Pongo filas en negrita y añado linea debajo
  #   for (fila in filas_negrita){
  #     tft <- tft %>% 
  #       bold( bold = TRUE, i = fila) %>%      #pongo la fila en negrita
  #       hline(part = "body", i = fila, border = border_style2 )  # añado un línea debajo
  #   }
  
  #Pongo en negrita y añado líneas debajo de las filas con campo bold = TRUE 
  tft <- tft %>% 
    bold( bold = TRUE, i = ~ bold == TRUE )  %>% 
    hline(part = "body", i = ~ bold == TRUE)
  
  
  #browser()
  #> inserto espacio en blanco (en realidad son guiones - ya que los espacios se 
  #> los como al representar)
  #> #aqui da el error y no se porque
  
  for (k in 1:max(t$desplaza) ){
    tft <- tft %>% 
      mk_par( 
        i = as.formula(paste("~ desplaza ==", k)),
        j = "territorio",
        value = as_paragraph(as_chunk( paste0(rep("-", k), collapse = ""),
                                       props = officer::fp_text(color = "white")),
                             as_chunk(territorio)
        )
      )
  }
  
  #> Inserto líneas verticales
  tft <- tft %>% 
    vline(part = "body", j= c(2,5,8) )
  
  tft <- tft %>% 
    vline(part = "header", j= c(2,5,8) )
  
  return(tft)  
}






#> Genera un gráfico rankin a partir de una salida de genera_datos_resumen (o de _prov)
#> y una variables para el rankin. Lo normar es que el campo tipo tome uno de
#> los 3 valores preciom2_M, superficie_M, preciomes_M
#> El gráfico siempre se ordena por orden creciente del campo preciom2_M para
#> que sea más facil detectar las diferencias 
grafico_ranking <- function(datos, tipo){
  #tipo <- datos[[tipo]]
  plot <- datos %>% 
    mutate(name = fct_reorder(territorio, preciom2_M)) %>% # {{tipo}}
    ggplot(aes(x = name, y = {{tipo}})) +
    geom_bar(stat = "identity", fill = "cornsilk1", color = "cornsilk2")+
    geom_text(stat = "identity", 
              aes(label = {{tipo}}), 
              hjust = -0.5,
              size = 2.2, 
              color = "cornsilk4")+
    coord_flip() +
    theme_minimal() +
    theme(text = element_text(size=8)) +
    labs(x = "", y = "") 
  return(plot)
}

#Genera un gráfico apilado. Toma como entrada la salida de genera_datos_resumen
# y la variable a usar (preciom2, preciomes o superficie)y devuelve un grafico de
#barrras apiladas
genera_grafico_apilado <- function(datos, tipo){
  #var toma los valores preciom2, preciomes o superficie
  t_largo <- datos %>% 
    pivot_longer(
      cols= preciom2_M:preciomes_p75,
      names_to = c("variable", "medida"),
      names_pattern = "^(.*?)_(.*?)$",
      values_to = "valor"
    ) %>% 
    select(territorio, variable, medida, valor)  #cambiar la leyenda
  
  # orden_territorio <- datos %>% 
  #   arrange(preciom2_M) %>% 
  #   pull(territorio)
  
  # El orden de las provincias será en orden creciente de la mediana de la
  # variable que se esté representando  
  orden_territorio <-  t_largo %>% 
    filter(variable == tipo & medida == "M") %>% 
    arrange(valor) %>% 
    pull(territorio)
  
  # el orden las barras apiladas
  orden_medidas <- c("p25", "M", "p75")
  
  # etiquetas para la leyenda
  etiquetas <- c("Percentil 25", "Mediana", "Percentil 75")
  
  # filtro de los datos con una sola de las 3 variables
  t_largo_filtro <- t_largo %>% filter(variable == tipo)%>% 
    mutate(territorio = factor (territorio, levels = orden_territorio),
           medida = factor (medida, levels = orden_medidas))
  
  # brewer.pal(3, "YlOrBr")
  # display.brewer.pal(3, "YlOrBr")
  colores <- c(brewer.pal(7, "YlOrBr")[[3]],
               brewer.pal(7, "YlOrBr")[[5]],
               brewer.pal(7, "YlOrBr")[[1]])
  # colores  # muesta los nombres 
  # show_col(colores) # muestra los colores 
  
  plot <- ggplot(t_largo_filtro, aes(fill = medida, x = territorio, y = valor ))+
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = colores, labels = etiquetas ) +
    theme_minimal() +
    theme(text = element_text(size=8)) +
    labs(x = "", y = "", fill = "") #, fill = "Renta mensual €/m2") 
  
  return(plot)
}

#Crea un mapa de barrios con ggplot2
crear_mapa_barrio <- function(barrios_de_un_municipio){
  gg <-
    ggplot() +
    geom_sf(data = barrios_de_un_municipio,
            aes(fill= f.renta_m2),
            color="grey") +
    geom_sf(data = provincia_sf, fill="transparent", color="grey5") +
    theme_void()+
    coord_sf(xlim = c(st_bbox(barrios_de_un_municipio)[[1]], 
                      st_bbox(barrios_de_un_municipio)[[3]]),
             ylim = c(st_bbox(barrios_de_un_municipio)[[2]], 
                      st_bbox(barrios_de_un_municipio)[[4]]))+
    
    # Aquí aplicamos la escala de colores
    scale_fill_brewer(palette = "YlOrBr",
                      name=expression("Renta mensual (€/m"^"2"~")"),
                      na.value = "gray95",
                      drop = FALSE,   #muestra todos los niveles aunque no esten presentes en el municipio 
                      labels = c(levels(barrios_de_un_municipio$f.renta_m2),
                                 "Sin datos"))  #+
  #  theme(legend.position = "none")  # Oculta la leyenda
  # labs(
  #   title = "Precios del alquiler (€/m2 mes)",
  #   subtitle = "Número de testigos",
  #   caption = "Fuente: AVRA | Elabora: Secretaría General de Vivienda"
  #   ) +
  # theme(
  #   text = element_text(color = "#22211d"),
  #   legend.position = c(0.8, 0.22)
  # ) 
  return(gg)
}



# 
# my_color_fun <- function(x) {
#   out <- rep("red", length(x))
#   out[x <= m+3] <- adjustcolor('red', alpha=0.2)
#   out[x <= m+1] <- adjustcolor('white')
#   out[x <= m-1] <- adjustcolor('blue', alpha=0.2)
#   out[x <= m-3] <- adjustcolor('blue', alpha=0.6)
#   return(out)
# }


# pal_colores <- colorNumeric(palette = "YlOrBr", domain = c(0,100))
colores <- rev(hcl.colors(6,"YlOrBr"))
my_color_fun <- function(x) {
  out <- rep("white", length(x)) # tb vale "red"
  out[x < Inf] <- colores[[6]]
  out[x < 10]  <- colores[[5]] #tb vale 'white' 
  out[x < 8] <- colores[[4]]
  out[x < 6] <- colores[[3]]
  out[x < 4] <- colores[[2]]
  out[x < 2] <- colores[[1]]
  return(out)
}


# # Otra forma de construir los colores que si va bien al generar word
# red_blue_palette <- colorRampPalette(c("red", "white", "blue"))
# colors_map <- red_blue_palette(5)
# my_color_fun <- function(x) {
#   out <- rep(colors_map[[1]], length(x)) # tb vale "red"
#   out[x <= m+3] <- colors_map[[2]]
#   out[x <= m+1] <- colors_map[[3]] #tb vale 'white' 
#   out[x <= m-1] <- colors_map[[4]]
#   out[x <= m-3] <- colors_map[[5]]
#   return(out)
# }



#> Toma un número y 4 puntos de ruptura
#> Devuelve el número de intervalo al que pertenece el valor
#> NO SE USA ESTA FUNCION EN NINGÚN SITIO

asignar_intervalo <- function(x, puntos_de_ruptura){
  if (length(puntos_de_ruptura) != 4) {
    stop("La lista de puntos de ruptura debe contener exactamente 4 valores.")
  }
  
  # Agrega -Infinito como primer límite
  limites <- c(-Inf, puntos_de_ruptura, Inf)
  
  for (i in 1:5) {
    if (x <= limites[i + 1]) {
      return(i)
    }
  }
  
  return(NULL)  # Fuera de todos los intervalos
}



