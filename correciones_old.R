
paquetes_necesarios = c("dplyr") #c("readxl","RPostgres","sf","dplyr","writexl")

for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}


rm(list =ls())

load("datos.RData")


# Me quedo con los registros que casan con mas de 1 vivienda y cuyo
# coeficiente de variación es inferior a 2
Fianzas_viviendas_N <- avra_catastro %>%
  filter ((comp > 1 & cat.coef_var < 2)) 

# Cambio en los registros el valor de superficie de cada vivienda por la media
# de todas las viviendas con las que casa. De esta forma todos los registros
# de una vivienda avra serán identicos
Fianzas_viviendas_N <- Fianzas_viviendas_N %>%
        mutate (stotalocal_14 = cat.media_superf)

# Como los registros de cada vivienda son identicos me puedo quedar con solo 1
# de cada tomando los registros diferentes
# La siguiente sentencia es mejorable buscando los registros identicos por 
# el contenido de todos sus campos y no solo el campo id_ams pero no es facil
# de hacer porque el dataframe contiene campos numericos y dan problema por la
# precision con que se almacenan
Fianzas_viviendas_N <- Fianzas_viviendas_N %>%
  distinct(id_ams, .keep_all = TRUE)


# Obtener tambien cuales se pierden para saber cuales son
# Se pierden pero se podrian vincular a traves de bloque portal escalera
Fianzas_viviendas_N_dist_tamanos <- avra_catastro %>%
  filter ((comp > 1 & cat.coef_var >= 2)) 

# Me quedo con solo registro para cada una de las viviendas tengo
Fianzas_viviendas_N_dist_tamanos_ID <- Fianzas_viviendas_N_dist_tamanos %>%
  distinct(id_ams, .keep_all = TRUE)

posicion <- which(names(Fianzas_viviendas_N_dist_tamanos_ID) == "rfcd_parcela")

Fianzas_viviendas_N_dist_tamanos_ID <- 
  Fianzas_viviendas_N_dist_tamanos_ID[,1:posicion]


# Los registros cuya referencia catastral no casa con ningún registro del catastro
# Se pueden corregir intentando encontrar la referencia catastral correcta con 
# ayuda de la Sede Electrónica del Catastro.
# Si se corrigen algunas referencias catastrales habría que lanzar de nuevo 
# el casado de la información corregida con el catastro

Fianzas_viviendas_0 <- avra_catastro %>%
  filter (comp == 0 | is.na(comp))
# Se pierden pero se podrian vincular a traves de bloque portal escalera ya que 
# encuentra en AVRA vinculados a referencia previa a division horizontal

# 9257804UF6695N0000AS es 9257804UF6695N0001SD
# 0369801UF6606N0001BJ es referencia previa a división horizontal
# 2548714UF7624N0001KE es referencia previa a división horizontal


# Las viviendas de AVRA que casan con 1 vivienda en los datos IECA Catastro
Fianzas_viviendas_1 <- avra_catastro %>%
  filter (comp == 1)

#Vamos a presentar los resultados

# Los registros que encontramos y coinciden son 1 vivienda o con varias
# pero si es con varias son de tamaño muy similar (coef. variación < 2)
Fianzas_casan_1_vivienda <- bind_rows(Fianzas_viviendas_1, Fianzas_viviendas_N)
rm(Fianzas_viviendas_1, Fianzas_viviendas_N)

# Los registros que no encontramos en catastro
# o bien no existe la RC de parcela o si existe no hay bienes inmuebles vivienda
# con la referencia de AVRA
Fianzas_no_casan_catastro <- Fianzas_viviendas_0
rm(Fianzas_viviendas_0)

# Los registros que encontramos en catastro pero sale más de 1 vivienda y
# no sabemos cuál de ellas es
Fianzas_casan_distintas_viviendas <- Fianzas_viviendas_N_dist_tamanos_ID
rm(Fianzas_viviendas_N_dist_tamanos_ID)

#si queremos ver los distintos registros con los que casa
Fianzas_casan_distintas_viviendas_case <- Fianzas_viviendas_N_dist_tamanos
rm(Fianzas_viviendas_N_dist_tamanos)

###################################################3
# SALVADO DE DATOS

save.image("datos.RData")

# Exportar el dataframe a Excel
write_xlsx(Fianzas_casan_1_vivienda, "Fianzas_casan_1_vivienda")
write_xlsx(Fianzas_no_casan_catastro, "Fianzas_no_casan_catastro")
write_xlsx(Fianzas_casan_distintas_viviendas, "Fianzas_casan_distintas_viviendas")
write_xlsx(Fianzas_casan_distintas_viviendas_case,
           "Fianzas_casan_distintas_viviendas_cases")




         