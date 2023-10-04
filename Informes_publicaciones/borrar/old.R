#######################################################################
# Genera la tabla en formato flex 
# el parametro cabeceras expresa que filas deben ser cabeceras (van en negrita)
# toma los valores "solo_andalucia" y "andaluciayprovincias"

resumen_a_flex <- function(t, cabeceras = "solo_andalucia"){
  t <- t %>% 
    mutate(n_fila = row_number())
  
  #Crea una lista con los números de fila que irán en negrita
  if(cabeceras != "andaluciayprovincias"){
    filas_cabecera <- t %>% 
      filter (territorio == "Andalucía")%>% 
      select(n_fila) %>% 
      as.list() %>%  
      .[[1]]
  }
  else { 
    filas_cabecera <- t %>% 
      filter(nombre == "") %>% 
      select(n_fila) %>% 
      as.list() %>%  
      .[[1]]
  }
  
  filas_no_cabecera <- setdiff(1:nrow(t), filas_cabecera)
  
  # Dejo solo las columnas que se van a utilizar
  t <- t %>% select(-c(anyo,provincia_806, nombre,n_fila))
  
  
  tft <- t %>% flextable() %>% 
    border_remove() 
  
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
  
  tft <-fontsize(tft, size = 9, part = "all") 
  
  tft <- tft %>% add_header_row(values = c("","", 
                                           "Renta (€/m2.mes)", "Superficie", "Cuantía (€/mes)" ),
                                colwidths = c(1,1,3,3,3))
  
  tft <- tft %>% 
    width(j = 1, width = 2.4) %>% 
    width(j = 2, width = 0.6) %>% 
    width(j = 3:11, width = .45)
  
  
  tft <- align(tft, part = "all", j = 3:11, align = "center") 
  
  tft <- tft %>%  hline(part = "header", i = 2, border = border_style1) 
  tft <- tft %>%  hline(part = "header", i = 1, j=3:11, border = border_style2) 
  tft <- tft %>%  hline_top(part = "header",  j=3:11, border = border_style2)
  
  for (fila in filas_cabecera){
    tft <- tft %>% 
      bold( bold = TRUE, i = fila) %>%      #pongo la fila en negrita
      hline(part = "body", i = fila, border = border_style2 )  # añado un línea debajo
  }
  
  tft <- compose( tft,
                  j = "territorio",
                  #part = "body",
                  i = filas_no_cabecera,
                  value = as_paragraph(as_chunk(". . ",
                                                props = officer::fp_text(color = "white")),
                                       as_chunk(territorio))
  )
  
  
  tft <- tft %>% 
    vline(part = "body", j= c(2,5,8) )
  
  #tft <- tft %>% vline_left(part = "body", border = border_style2)
  
  tft <- tft %>% 
    vline(part = "header", j= c(2,5,8) )
  
  
  tft <- compose( tft,
                  j = "territorio",
                  #part = "body",
                  i = filas_no_cabecera,
                  value = as_paragraph(as_chunk(". . ",
                                                props = officer::fp_text(color = "white")),
                                       as_chunk(territorio))
  )
  
  return(tft)  
}







 load("./datos_output/avra_catastro_2022.RData")
 datos_para_analisis_2022 <- preparacion_datos(avra_catastro_2022)









