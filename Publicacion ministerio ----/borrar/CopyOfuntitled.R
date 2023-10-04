genera_resumen_barrio <- function(datos){
  d_barrio <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre", "barrio.nombre"),
                         list(character(0)))
  
  d_mun <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre"),
                         list(character(0)))
  
  # Tomo solo los municipios presentes en la capa de barrios
  d_mun <- d_mun %>% 
    filter(nombre %in% unique(d_barrio$nombre))
  
  d_barrio <- d_barrio %>% 
    filter(!is.na(barrio.nombre))
  
  t <- bind_rows(d_barrio, d_mun) %>% 
    mutate(barrio.nombre = ifelse(is.na(barrio.nombre),"",barrio.nombre)) %>% 
    arrange(nombre, barrio.nombre) %>%   #ordena las filas de la tabla
    
  return(t)
}


genera_datos_resumen <- function(datos){
  d_mun <-  genera_cruce_variables(datos, c("anyo", "provincia_806", "nombre"),
                         list(character(0)))
  
  d_prov <- genera_cruce_variables(datos, c("anyo","provincia_806"),
                         list(character(0)))
  
  d_and <- genera_cruce_variables(datos, c("anyo"),
                        list(character(0)))
  
  d_prov <- d_prov %>% 
    mutate(nombre = "")
  
  d_and <- d_and %>% 
    mutate(nombre = "",
           provincia_806 = "")
  
  d_mun <- d_mun %>% 
    filter(n >= 10) %>% 
    mutate(nombre = ifelse(provincia_806 == nombre,
                           paste(nombre, " (capital)"),
                           nombre))
  
  t <- bind_rows(d_mun, d_prov, d_and) %>% 
    arrange(provincia_806, nombre) %>%   #ordena las filas de la tabla
    
    # Añado a la fila de Andalucía el nombre
    # No sirve de nada añadir espacios al nombre porque flextable elimina los espacios vacíos 
    mutate(provincia_806 = ifelse( provincia_806 == "" & nombre == "",
                                   "Andalucía",
                                   provincia_806),
           territorio = ifelse( nombre == "",
                                provincia_806,
                                nombre)) %>% 
    relocate(territorio, .after = "anyo")
  
  return(t)
}

genera_datos_resumen_prov <- function(datos){
  
  d_prov <- genera_cruce_variables(datos, c("anyo","provincia_806"),
                         list(character(0)))
  
  d_and <- genera_cruce_variables(datos, c("anyo"),
                        list(character(0)))
  
  d_prov <- d_prov %>% 
    mutate(nombre = "")
  
  d_and <- d_and %>% 
    mutate(nombre = "",
           provincia_806 = "")
  
  
  t <- bind_rows( d_prov, d_and) %>% 
    arrange(provincia_806, nombre) %>%   #ordena las filas de la tabla
    
    # Añado a la fila de Andalucía el nombre
    # No sirve de nada añadir espacios al nombre porque flextable elimina los espacios vacíos 
    mutate(provincia_806 = ifelse( provincia_806 == "" & nombre == "",
                                   "Andalucía",
                                   provincia_806),
           territorio = ifelse( nombre == "",
                                provincia_806,
                                nombre)) %>% 
    relocate(territorio, .after = "anyo")
  
  return(t)
}
