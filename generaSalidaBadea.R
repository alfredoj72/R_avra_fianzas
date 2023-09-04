rm(list =ls())
load("./datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["datos"]]
rm(datos_para_analisis_2022)
datos <- as.data.frame(datos)
datos <- st_drop_geometry(datos)
str(datos)

lista_campos <- c("sexo_arrendador", "tipo_persona_arrendador",
                  "sexo_arrendatario", "nacionalidad_arrendatario") #"tipo_entidad_arrendador"

# El campo pivote es el del nivel de agregación para el que se van a tener 
# los resultados. Tendremos: 
# Para toda Andalucía c("")
# Para provincias c("provincia_806), municipio "municipio_806" ,etc 

campo_pivote <- c("") #,"municipio_806") 

combinaciones_campos <- unlist(lapply(0:length(lista_campos),
                    function(m){combn(lista_campos,m=m, simplify = FALSE)} ),
                        recursive = FALSE)

#combinaciones_campos

rm(resultado) ; resultado <- data.frame()

for (campos  in rev(combinaciones_campos)) {
  parciales <- datos %>%
    select(all_of(campos),any_of(campo_pivote), stotalocal_14,renta_m2) %>%
    group_by(across(c(all_of(campos),any_of(campo_pivote)))) %>% 
   
     # en el summarise siguiente es en el que hay que colocar los parámetros
    # que se quieran calcular para cada grupo: n(), mediana, Q25, Q75 de los campos
    summarise(stotalocal_14 = sum(stotalocal_14), 
              mediana_renta_m2 = median(renta_m2),
              n = n(),
              .groups = "drop") 
  
  resultado <- bind_rows(resultado, parciales)
}

# Si un campo por el que se agrupa tiene NA este también aparecerá en el resultado
# No deben existir valores NA, de lo contrario se confundirán con los NA
# que salen cuando un campo no se usa como criterio de agrupación. Se deben rellenar
# con "No Especificado" o algo similar

# en el df resultado habría que sustituir los NA (por no usar el criterio)
# por el valor que en 
# BADEA se utilize para indicar que dicha variable no se usa en esa línea

# Hay que asegurarse que salen todas las combinaciones de todos los valores
# en cada una de las variables por las que se agrupa, para ello lo mejor
# es que sean factores y usar la opción (creo que .drop = F) que lo hace así.
# Ojo que al definir los factores municipos hay que incluir los 780 no solo
# los que hay en el proyecto
  
# Si introduzco funciones 

genera_BADEA <- function(campo_pivote){
  resultado <- data.frame()
for (campos  in rev(combinaciones_campos)) {
  browser()
  parciales <- datos %>%
    select(all_of(campos),any_of(campo_pivote), stotalocal_14,renta_m2) %>%
    group_by(across(c(any_of(campo_pivote),all_of(campos)))) %>% 
    
    # en el summarise siguiente es en el que hay que colocar los parámetros
    # que se quieran calcular para cada grupo: n(), mediana, Q25, Q75 de los campos
    summarise(stotalocal_14 = sum(stotalocal_14), 
              mediana_renta_m2 = median(renta_m2),
              n = n(),
              .groups = "drop") 
  
  resultado <- bind_rows(resultado, parciales )
}
  return(resultado)
}

#kk <- genera_BADEA("")

campo_pivote <- c("","provincia_806","municipio_806")  # secciones, barrios, etc
for (pivote in campo_pivote){
  resultado <- genera_BADEA(pivote)
  
  nombre_variable <- paste0("BADEA_", pivote)
  assign(nombre_variable, resultado)
}

rm(BADEA_, BADEA_municipio_806, BADEA_provincia_806)



###################################################################
# 1 ###############################################################
###################################################################

Selecciona variables a cruzar
datos <- datos %>% 
  select(f.durac_contrato, f.)

datos$f

###################################################################
# 2 ###############################################################
###################################################################

Recodificar las variables a los códigos de BADEA

###################################################################
# 3 ###############################################################
###################################################################

Gnerar salida para BADEA. El código está al principio de este fichero


###################################################################
# 4 ###############################################################
###################################################################

Sustituir en la salida los campos no rellenos por el valor (todos)
para que cuando no se ha usado una variable en un cruce tome el valor todos




