# Definir los intervalos o categorías para agrupar los valores numéricos
intervalos <- cut(Fianzas_viviendas$importe_de_la_renta,
                  breaks = c(0, 300, 400, 500, 600, 700, 800, 900, 1000,
                             1500,2000,2500, 5000, 32000), include.lowest = TRUE)
tabla_frecuencias <- table(intervalos)
print(tabla_frecuencias)
para la renta ensayar con 4,5,6,7,8,9,10 intervalos

hacer intervalos por deciles

hacer intervalos?
  
  



# busco incongruencias entre numero de habitaciones y superficie de vivienda

# Segmentar los campos en intervalos
segmento_superf <- cut(Fianzas_viviendas$stotalocal_14,
                       breaks = c(-Inf,0, 60, 100, 120, 150, 200, 300, 400, Inf),
                       include.lowest = TRUE)
segmento_num_hab <- cut(Fianzas_viviendas$num_habitaciones,
                        breaks = c(0, 1,2,3,4,5,6, Inf),
                        include.lowest = TRUE)
tabla_contingencia <- table(segmento_num_hab, segmento_superf)
print(tabla_contingencia)

min(Fianzas_viviendas$num_habitaciones, na.rm = TRUE)







ggplot(Fianzas_viviendas, aes(x = stotalocal_14)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Superficie vivienda", y = "Frecuencia") +
  theme_minimal()




# La información sobre el número de habitaciones no es congruente con la de
# superficie. 




ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "Variable", y = stotalocal_14), fill = "steelblue", color = "black") +
  labs(x = "", y = "stotalocal_14") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = stotalocal_14)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Superficie vivienda", y = "Frecuencia") +
  theme_minimal()























Altos <- Fianzas_viviendas %>%
  filter(stotalocal_14 >  Q3)

calculo_por_antiguedad2 <- Altos %>%
  group_by(a_ant_bim) %>%
  summarize(media_renta_m2 = mean(renta_m2, na.rm = FALSE),
            mediana_renta_m2 = median(renta_m2, na.rm = FALSE),
            media_sup = mean(stotalocal_14, na.rm = FALSE),
            mediana_sup = median(stotalocal_14, na.rm = FALSE),
            casos = n())
















marcas_sup <- c(0, 30, 60, 250, 500, Inf)
marcas_renta <- c(0, 100,300,500,1000,2000,5000, Inf)
marcas_renta_m2 <- c(0,)

grupos_superficie <- cut(Fianzas_viviendas$stotalocal_14, 
                         breaks = marcas_sup,
                         include.lowest = TRUE)

grupos_renta <- cut(Fianzas_viviendas$importe_de_la_renta, 
                    breaks = marcas_renta,
                    include.lowest = TRUE)
tabla_contingencia <- table(grupos_superficie, grupos_renta)

# Imprimir la tabla de contingencia
print(tabla_contingencia)

###################################

#analisis viviendas superficies 
#######################################################################
# Superficie de vivienda
Hmisc::describe(Fianzas_viviendas$stotalocal_14) 
summary(Fianzas_viviendas$stotalocal_14)

ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "Variable", y = stotalocal_14), fill = "steelblue", color = "black") +
  labs(x = "", y = "stotalocal_14") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = stotalocal_14)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Superficie vivienda", y = "Frecuencia") +
  theme_minimal()

# analisis de los mas grandes
Q1 <- quantile(Fianzas_viviendas$stotalocal_14, 0.25, na.rm = TRUE)
Q3 <- quantile(Fianzas_viviendas$stotalocal_14, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 4 * IQR

#análisis de los más grandes

percentil_95 <- quantile(Fianzas_viviendas$stotalocal_14, probs = 0.95, na.rm = TRUE)
mayores_95 <- Fianzas_viviendas %>%
  filter(stotalocal_14 > limite_superior)

Q_05 <- quantile(Fianzas_viviendas$stotalocal_14, 0.05, na.rm = TRUE)
Q_95 <- quantile(Fianzas_viviendas$stotalocal_14, 0.95, na.rm = TRUE)
centro_90 <- Fianzas_viviendas %>%
  filter(stotalocal_14 > Q_05 & stotalocal_14 < Q_95)

min_renta_m2 <- min(centro_90$renta_m2)
super_mas_alta <- max(centro_90$stotalocal_14)

prueba <- Fianzas_viviendas %>%
  filter(renta_m2 < min_renta_m2 & stotalocal_14 > super_mas_alta)

prueba <- Fianzas_viviendas %>%
  filter(renta_m2 < min_renta_m2 )

Hmisc::describe(centro_90$renta_m2) 
summary(centro_90$renta_m2)
ggplot(centro_90) +
  geom_boxplot(aes(x = "Variable", y = renta_m2), fill = "steelblue", color = "black") +
  labs(x = "", y = "Renta: €/m2") +
  theme_minimal()
ggplot(centro_90, aes(x = renta_m2)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Renta: €/m2", y = "Frecuencia") +
  theme_minimal()








# Veamos el nº de habitaciones
summary(mayores_95$num_habitaciones)
ggplot(mayores_95) +
  geom_boxplot(aes(x = "Variable", y = num_habitaciones), fill = "steelblue", color = "black") +
  labs(x = "", y = "Nº habitaciones") +
  theme_minimal()
ggplot(mayores_95, aes(x = num_habitaciones)) +
  geom_histogram(bins = 13, fill = "steelblue", color = "cyan") +
  labs(x = "Nº habitaciones", y = "Frecuencia") +
  theme_minimal()


segmento_n_hab <- cut(mayores_95$num_habitaciones, 
                      breaks = c(-Inf,unique(mayores_95$num_habitaciones), Inf), 
                      include.lowest = TRUE)


segmento_superf <- cut(mayores_95$stotalocal_14, 
                       breaks = quantile(mayores_95$stotalocal_14,
                                         probs = seq(0, 1, 0.1),
                                         na.rm = TRUE), 
                       include.lowest = TRUE)

tabla_contingencia <- table(segmento_n_hab, segmento_superf)
print(tabla_contingencia)


# Veamos el precio

summary(mayores_95$importe_de_la_renta)
ggplot(mayores_95) +
  geom_boxplot(aes(x = "Variable", y = importe_de_la_renta), fill = "steelblue", color = "black") +
  labs(x = "", y = "Nº habitaciones") +
  theme_minimal()
ggplot(mayores_95, aes(x = importe_de_la_renta)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "cyan") +
  labs(x = "Importe de la renta", y = "Frecuencia") +
  theme_minimal()


segmento_importe <- cut(mayores_95$importe_de_la_renta, 
                        breaks = quantile(mayores_95$stotalocal_14,
                                          probs = seq(0, 1, 0.1),
                                          na.rm = TRUE), 
                        include.lowest = TRUE)


segmento_superf <- cut(mayores_95$stotalocal_14, 
                       breaks = quantile(mayores_95$stotalocal_14,
                                         probs = seq(0, 1, 0.1),
                                         na.rm = TRUE), 
                       include.lowest = TRUE)

tabla_contingencia <- table(segmento_importe, segmento_superf)
print(tabla_contingencia)


# Veamos la renta media por m2

summary(mayores_95$renta_m2)
ggplot(mayores_95) +
  geom_boxplot(aes(x = "Variable", y = renta_m2), fill = "steelblue", color = "black") +
  labs(x = "", y = "Renta: €/m2") +
  theme_minimal()
ggplot(mayores_95, aes(x = renta_m2)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Renta: €/m2", y = "Frecuencia") +
  theme_minimal()


viviendas_repetidas_avra <- Fianzas_viviendas %>%
  filter(avra_rc_repet > 1 & comp == 1)

viviendas_repetidas_avra <- Fianzas_viviendas %>%
  filter(avra_rc_repet == 2 & comp == 1)

viviendas_repetidas_avra <- Fianzas_viviendas %>%
  filter(avra_rc_repet > 2 & comp == 1)

kk <- viviendas_repetidas_avra %>%
  select(fecha_devengo,referencia_catastral,avra_rc_repet, tip_const4d_14, stotalocal_14, comp,importe_de_la_renta, renta_m2, tasa_superf)

# Renta €/m2
Hmisc::describe(Fianzas_viviendas$renta_m2) 
summary(Fianzas_viviendas$renta_m2)

ggplot(Fianzas_viviendas) +
  geom_boxplot(aes(x = "Variable", y = renta_m2), fill = "steelblue", color = "black") +
  labs(x = "", y = "Renta €/m2") +
  theme_minimal()

ggplot(Fianzas_viviendas, aes(x = renta_m2)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Renta €/m2", y = "Frecuencia") +
  theme_minimal()

Q_05 <- quantile(Fianzas_viviendas$renta_m2, 0.05, na.rm = TRUE)
kk <- Fianzas_viviendas %>%
  filter(renta_m2 < Q_05)

ggplot(kk, aes(x = renta_m2)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Renta €/m2", y = "Frecuencia") +
  theme_minimal()

















#############################################################
viviendas_repetidas_avra <- Fianzas_viviendas %>%
  filter(avra_rc_repet > 1 & comp == 1)



# analisis de los mas grandes
Q1 <- quantile(Fianzas_viviendas$stotalocal_14, 0.25, na.rm = TRUE)
Q3 <- quantile(Fianzas_viviendas$stotalocal_14, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

Q95 <- quantile(Fianzas_viviendas$stotalocal_14, 0.95, na.rm = TRUE)
limite_superior2 <- 2 * Q95 

# Calcular la función de distribución acumulada empírica
fda <- ecdf(Fianzas_viviendas$stotalocal_14)

# Evaluar la función de distribución acumulada en el valor deseado
percentil <- fda(limite_superior2)

# Imprimir el percentil
print(percentil)
calculo  <- Fianzas_viviendas %>%
  filter(stotalocal_14 < limite_superior & stotalocal_14 > limite_inferior)
renta_m2_minima <- min(calculo$renta_m2)

# validas <- Fianzas_viviendas %>%
#                  filter(stotalocal_14 < limite_superior2)
# renta_m2_minima <- min(validas$renta_m2)

#elimino lo registros cuya superficie es superior al limite_superior y se
# encuentran más de una vez en la tabla de AVRA salvo que la renta_m2 sea
# superior ??

eliminar <- Fianzas_viviendas %>%
  filter(stotalocal_14 > limite_superior2 & avra_rc_repet > 1 & renta_m2 < renta_m2_minima)

eliminar2 <- Fianzas_viviendas %>%
  filter(stotalocal_14 > limite_superior2 & avra_rc_repet > 1 & renta_m2 > renta_m2_minima)


kkeliminar  <- eliminar %>%
  select(fecha_devengo,referencia_catastral,a_ant_bim,avra_rc_repet, tip_const4d_14, stotalocal_14, comp,importe_de_la_renta, renta_m2, tasa_superf)

kkeliminar2  <- eliminar2 %>%
  select(fecha_devengo,referencia_catastral,a_ant_bim,avra_rc_repet, tip_const4d_14, stotalocal_14, comp,importe_de_la_renta, renta_m2, tasa_superf)


hFianzas_viviendas <- Fianzas_viviendas %>%
  filter(stotalocal_14 < limite_superior &
           stotalocal_14 > limite_inferior)


calculo_por_antiguedad <- Fianzas_viviendas %>%
  group_by(a_ant_bim) %>%
  summarize(media_renta_m2 = mean(renta_m2, na.rm = FALSE),
            mediana_renta_m2 = median(renta_m2, na.rm = FALSE,),
            casos = n())

sum(calculo_por_antiguedad$casos, na.rm = TRUE)

# Imprimir los resultados
print(media_renta_m2)
print(mediana_renta_m2)
print(tabla_a_ant_bim)


#esot me  va servir
Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate(err_sup = ifelse( stotalocal_14 > 250, "grande",""))


Q1 <- quantile(Fianzas_viviendas$importe_de_la_renta, 0.05, na.rm = TRUE)
Q3 <- quantile(Fianzas_viviendas$importe_de_la_renta, 0.95, na.rm = TRUE)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

Fianzas_viviendas <- Fianzas_viviendas %>%
  mutate(error_renta = ifelse( importe_de_la_renta < limite_superior, "no grande",""))

kk1 <- Fianzas_viviendas %>%
  filter(err_sup == "grande" & avra_rc_repet > 1)# & error_renta == "no grande")

kk2 <- Fianzas_viviendas %>%
  filter(err_sup == "grande" & avra_rc_repet == 1)# & error_renta == "no grande")


kkk1 <- kk1 %>%
  select(fecha_devengo,referencia_catastral,a_ant_bim,avra_rc_repet, tip_const4d_14, stotalocal_14, comp,importe_de_la_renta, renta_m2, tasa_superf)

kkk2 <- kk2 %>%
  select(fecha_devengo,referencia_catastral,a_ant_bim,avra_rc_repet, tip_const4d_14, stotalocal_14, comp,importe_de_la_renta, renta_m2, tasa_superf)


kk$a_ant_bim

Q1 <- quantile(Fianzas_viviendas$renta_m2, 0.05, na.rm = TRUE)
Q3 <- quantile(Fianzas_viviendas$renta_m2, 0.95, na.rm = TRUE)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
























#################################################################################
#################################################################################




barplot(table(Fianzas_viviendas$tipo_entidad_arrendador), las = 2, cex.names = 0.8)

#para cotejar que coincide con el codigo ine provicia y muncipio del 806

conteo <- sum(Fianzas_viviendas$codigo_ine != Fianzas_viviendas$cod_ine)
cat("Hay", conteo, "registros cuyo valor de código INE de municipio no
    coincide con el asignado a través de la referencia catastral")

Fianzas_viviendas <- Fianzas_viviendas %>% 
         mutate(error_cod_municipio = ifelse(codigo_ine != cod_ine, 
                                             "No coincide",""))

# Duracion de contrato
summary(Fianzas_viviendas$duracion_contrato_años)
boxplot(Fianzas_viviendas$duracion_contrato_años)
densidad <- density(na.omit(Fianzas_viviendas$duracion_contrato_años))
plot(densidad)
hist(Fianzas_viviendas$duracion_contrato_años, breaks = 40)
# Definir los intervalos o categorías para agrupar los valores numéricos
intervalos <- cut(Fianzas_viviendas$duracion_contrato_años,
                  breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 40), include.lowest = TRUE)
tabla_frecuencias <- table(intervalos)
print(tabla_frecuencias)
barplot(tabla_frecuencias)


#Fecha de devengo
Hmisc::describe(Fianzas_viviendas$fecha_devengo)
#hay 365 fechas distintas
Hmisc::describe(lubridate::year(Fianzas_viviendas$fecha_devengo))
#Efectivamente todas las fecha de devengo son de 2022



# tipo de arrendamiento 
barplot(table(Fianzas_viviendas$tipo_de_arrendamiento))


# tipo de actualizacion
Hmisc::describe(Fianzas_viviendas$tipo_actualizacion)
#467 valores distintos
aux <- table(Fianzas_viviendas$tipo_actualizacion)
aux <- sort(aux, decreasing = TRUE)
aux2 <- data.frame( names(aux),  aux)
#como se puede ver hay que homogeneizar para poder explotar dicho campo
print(aux2)

# Importe de la fianza. Se compara con el importe de la renta
aux_may <- sum(Fianzas_viviendas$importe_de_la_fianza >
                 Fianzas_viviendas$importe_de_la_renta)
aux_men <- sum(Fianzas_viviendas$importe_de_la_fianza <
                 Fianzas_viviendas$importe_de_la_renta)
aux_ig <- sum(Fianzas_viviendas$importe_de_la_fianza ==
                Fianzas_viviendas$importe_de_la_renta)
print(aux_may)
print(aux_men)
print(aux_ig)
#La mayoria de los registros coinciden,
#algunos la fianza es mayor, muchos de ellos porque afianza dos meses
#los pocos en los que la fianza es menor debe ser porque se está completando
#una fianza anterior por un contrato anterior de menor cuantía




# Identificador de bien inmueble
Hmisc::describe(Fianzas_viviendas$id_bi)
#ojo. Hay 30145 valores distintos mientras que de referencias catastrales
#hay 30146
#eso se debe a que hay referencias catastrales que tienen igual los primeros 18 dígitos
#y los dos últimos distintos. Un de las dos referencias catastrales es errónea
print(length(unique(Fianzas_viviendas$referencia_catastral)))
print(length(unique(substr(Fianzas_viviendas$referencia_catastral,1,18))))

Fianzas_viviendas <- Fianzas_viviendas %>% 
  mutate(rc18 = substr(referencia_catastral,1,18)) %>%
  group_by(rc18) %>%
  mutate(rep_rc18 = n())
Fianzas_viviendas <- Fianzas_viviendas %>% 
  mutate(rc_2 = substr(referencia_catastral,19,20)) %>%
  group_by(rc_2) %>%
  mutate(rep_rc_2 = n())

kk <- Fianzas_viviendas %>%
  filter(rep_rc18 == 2 & rep_rc_2 == 1)
# la referencia catastral 9590202VG4099B0025M es incorrecta, debe ser
# 9590202VG4099B0025MA


# Identificador de vivienda
Hmisc::describe(Fianzas_viviendas$idvcat)
#coincide en numero con los bienes inmuebles distintos

# tipo de construccion
Hmisc::describe(Fianzas_viviendas$tip_const4d_14)
table(Fianzas_viviendas$tip_const4d_14)
barplot(table(Fianzas_viviendas$tip_const4d_14))






#pinto solo los menores de 500 m2
menores <- Fianzas_viviendas %>%
           filter(stotalocal_14 < 500)

ggplot(menores) +
  geom_boxplot(aes(x = "Variable", y = stotalocal_14), fill = "steelblue", color = "black") +
  labs(x = "", y = "stotalocal_14") +
  theme_minimal()



#pinto solo los mayores 200 m2
mayores <- subset(Fianzas_viviendas$stotalocal_14, Fianzas_viviendas$stotalocal_14 > 200)
densidad <- density(na.omit(mayores))
plot(densidad)


# Importe de la renta
hist(Fianzas_viviendas$importe_de_la_renta, breaks = 1000)

ggplot(Fianzas_viviendas, aes(x = importe_de_la_renta)) +
  geom_histogram(bins = 1000, fill = "steelblue", color = "cyan") +
  labs(x = "Importe de la Renta", y = "Frecuencia") +
  theme_minimal()

# Definir los intervalos o categorías para agrupar los valores numéricos
intervalos <- cut(Fianzas_viviendas$stotalocal_14,
                  breaks = c(0, 30, 60, 70, 80, 90, 100, 110, 120,
                             500,17000), include.lowest = TRUE)
tabla_frecuencias <- table(intervalos)
# Imprimir la tabla de frecuencias
print(tabla_frecuencias)
barplot(tabla_frecuencias, las = 2, cex.names = 0.8)

# Antigüedad de la vivienda
Hmisc::describe(Fianzas_viviendas$a_ant_bim) 
boxplot(Fianzas_viviendas$a_ant_bim)

densidad <- density(na.omit(Fianzas_viviendas$a_ant_bim))
plot(densidad)
#pinto solo los de 1900 en adelante
menores <- subset(Fianzas_viviendas$a_ant_bim, Fianzas_viviendas$a_ant_bim > 1900)
densidad <- density(na.omit(menores))
plot(densidad)

intervalos <- cut(Fianzas_viviendas$a_ant_bim,
                  breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                  include.lowest = TRUE)
tabla_frecuencias <- table(intervalos)
# Imprimir la tabla de frecuencias
print(tabla_frecuencias)
barplot(tabla_frecuencias)
hist(Fianzas_viviendas$a_ant_bim, breaks = 100)

























#*******************************************************







Hmisc::describe(Fianzas_viviendas$tasa_superf) 
boxplot(Fianzas_viviendas$tasa_superf)
summary(Fianzas_viviendas$tasa_superf)
densidad <- density(na.omit(Fianzas_viviendas$tasa_superf))
plot(densidad)

#cuantos registros tienen un tasa >3 (la estadistica del minsiterio no usa
# los registros donde dicho valor es >3)
#por aqui voy
num_registros <- sum(Fianzas_viviendas$tasa_superf > 3)
#
Fianzas_viviendas_1 <- Fianzas_viviendas[Fianzas_viviendas$tasa_superf <= 3, ]




# ANALISIS DE OUTLIERS EN LOS CAMPOS SUPERFICIE, RENTA, PRECIO / M2 Y
# PROPORCION SUPERFICIE DE VIVIENDA / SUPERFICIE DE PARCELA


# Definir una función para identificar los outliers y crear un nuevo campo
crear_nuevos_campos <- function(vector) {
  q1 <- quantile(vector, 0.25)
  q3 <- quantile(vector, 0.75)
  iqr <- q3 - q1
  limit_inf <- q1 - 1.5 * iqr
  limit_sup <- q3 + 1.5 * iqr
  nuevos_valores <- ifelse(vector < limit_inf | vector > limit_sup, "*","_")
  return(nuevos_valores)
}

# Obtener los nombres de las columnas a las que se les aplicará la función
campos <- c("stotalocal_14", "importe_de_la_renta", "renta_m2")

# Aplicar la función crear_nuevos_campos a los campos seleccionados de df usando lapply
resultados <- lapply(Fianzas_viviendas[campos], crear_nuevos_campos)

# Convertir los resultados en un nuevo data frame
nuevos_campos <- as.data.frame(resultados)

# Cambiar el nombre de los campos con información de outliers
nuevos_nombres <- lapply(campos, function(nombre) paste0("outliers_", nombre))
nuevos_campos <- setNames(nuevos_campos, nuevos_nombres)
nuevos_campos$outliers_t <- paste0 (nuevos_campos$outliers_stotalocal_14,
                                   nuevos_campos$outliers_importe_de_la_renta,
                                   nuevos_campos$outliers_renta_m2)

# Agregar los nuevos campos al dataframe original usando cbind
Fianzas_viviendas <- cbind(Fianzas_viviendas, nuevos_campos)

rm(crear_nuevos_campos, campos, nuevos_campos, nuevos_nombres, resultados)



ggplot(Fianzas_viviendas, aes( x = importe_de_la_renta,
                               y = stotalocal_14,
                               color = outliers_t)) +
       geom_point() +
       labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
            color = "Outliers")


ggplot(Fianzas_viviendas, aes( x = importe_de_la_renta,
                               y = stotalocal_14,
                               color = outliers_t)) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers") +
  coord_cartesian(xlim = c(0, 2000), ylim = c(0, 2000))


#veamos solo los no outlier
Fianzas_viviendas_1 <- Fianzas_viviendas[Fianzas_viviendas$outliers_t == "___", ]

ggplot(Fianzas_viviendas_1, aes( x = importe_de_la_renta,
                               y = stotalocal_14,
                               color = outliers_t)) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers")

#veamos los outliers
Fianzas_viviendas_1 <- Fianzas_viviendas[Fianzas_viviendas$outliers_t != "___", ]

ggplot(Fianzas_viviendas_1, aes( x = importe_de_la_renta,
                                 y = stotalocal_14,
                                 color = outliers_t)) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers")


ggplot(Fianzas_viviendas_1, aes( x = importe_de_la_renta,
                                 y = stotalocal_14,
                                 color = substr(outliers_t,1,2))) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers")+
  coord_cartesian(xlim = c(0, 2000), ylim = c(0, 2000))

ggplot(Fianzas_viviendas_1, aes( x = importe_de_la_renta,
                                 y = stotalocal_14,
                                 color = outliers_renta_m2)) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers")+
  coord_cartesian(xlim = c(0, 2000), ylim = c(0, 2000))


3#voy a analizar los casos más llamativos 
# renta alquiler > 2000
# superficie > 250
# € /m2 > 20

iqr <- IQR(Fianzas_viviendas$stotalocal_14)
third_quartile <- quantile(Fianzas_viviendas$stotalocal_14, 0.75)
limite_conservador <- third_quartile + 4 * iqr
cat("3 cuartil", third_quartile, 
    " - IQR ", iqr, 
    " -- limite conservador >", limite_conservador)


Fianzas_viviendas_2 <- Fianzas_viviendas[
                  Fianzas_viviendas$stotalocal_14 < limite_conservador, ]

Fianzas_viviendas_3 <- Fianzas_viviendas[
                  Fianzas_viviendas$stotalocal_14 > limite_conservador, ]

xlim <- c(0, 500)  # Ajustar los límites del eje x
ylim <- c(0, max(densidad$y)) # Ajustar los límites del eje y
densidad <- density(na.omit(Fianzas_viviendas$stotalocal_14))
plot(densidad, main = "Densidad Superficie Vivienda", xlim = xlim, ylim = ylim)

ylim <- c(0, 0.003) 
plot(densidad, main = "Densidad Superficie Vivienda", xlim = xlim, ylim = ylim)

densidad <- density(na.omit(Fianzas_viviendas_2$stotalocal_14))
plot(densidad)

densidad <- density(na.omit(Fianzas_viviendas_3$stotalocal_14))
plot(densidad)

#voy a ver el histograma de frecuencias
table(Fianzas_viviendas$stotalocal_14)
barplot((Fianzas_viviendas$stotalocal_14))


# Agrupar los datos por bloques de ancho 10
bloques <- cut(Fianzas_viviendas$stotalocal_14, 
               breaks = seq(0, max(Fianzas_viviendas$stotalocal_14) + 10, by = 10),
               include.lowest = TRUE)
barplot(table(bloques), xlim = c(0, 60))



barplot(table(Fianzas_viviendas_2$stotalocal_14))
barplot((Fianzas_viviendas_3$stotalocal_14))


ggplot(Fianzas_viviendas_2, aes( x = importe_de_la_renta,
                               y = stotalocal_14,
                               color = outliers_t)) +
  geom_point() +
  labs(x = "Importe de la renta", y = "Superficie de vivienda" ,
       color = "Outliers")+
       coord_cartesian(xlim = c(0, 2000), ylim = c(0, 2000))






estoy tratando de determinar que registros responden a errores,
es decir o que no corresponden a viviendas
o que se ha introducido un dato incorrecto
entiendo que para estimar precio de alquiler debo tomar solo datos de
vivienda de hasta x metros y quizas hasta x precio

en el chatgpt con hilos posteriores a New chat hay 7 hilos con cuestiones interesantes
a desarrollar.













qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq


hist(Fianzas_viviendas$importe_de_la_renta, breaks = 125)
# Definir los intervalos o categorías para agrupar los valores numéricos
intervalos <- cut(Fianzas_viviendas$importe_de_la_renta,
                  breaks = c(0, 300, 400, 500, 600, 700, 800, 900, 1000,
                             1500,2000,2500, 5000, 32000), include.lowest = TRUE)
tabla_frecuencias <- table(intervalos)

# Imprimir la tabla de frecuencias
print(tabla_frecuencias)
barplot(tabla_frecuencias)





################################################################
# Algo de código sobre la identificación de intervalos para clasificar una variable
# numérica
# lo pruebo con la variable Fianzas_viviendas$stotalocal_14

# idea
# nbreaks <- pretty(range(kk), n = nclass.Sturges(kk),
#                   min.n = 1)

# Determinación del número de intervalos
nclass.Sturges(Fianzas_viviendas$stotalocal_14)
nclass.scott(Fianzas_viviendas$stotalocal_14)
nclass.FD(Fianzas_viviendas$stotalocal_14)

# Variable con los valores del número de intervalos
num_intervalos <- 5:10

# Vector para almacenar los resultados de tot.withinss
tot_withinss <- numeric(length(num_intervalos))
betweenss <- numeric(length(num_intervalos))

variable <- Fianzas_viviendas$stotalocal_14
df <- as.data.frame(variable)

# Bucle para iterar sobre los diferentes números de intervalos
for (i in 1:length(num_intervalos)) {
  # Aplicar kmeans con el número de intervalos actual
  kmeans_result <- kmeans(variable, centers = num_intervalos[i], nstart = 50)
  
  # Obtener los centroides de los grupos
  centroides <- kmeans_result$centers
  
  # Obtener los grupos resultantes
  grupos <- kmeans_result$cluster
  
  valores_maximos <- sapply(1:max(grupos), function(i) max(df[grupos == i, "variable"]))
  valores_minimos <- sapply(1:max(grupos), function(i) min(df[grupos == i, "variable"]))
  valores_medios <- (valores_maximos + valores_minimos) / 2
  
  # # Almacenar el valor de tot.withinss
  # tot_withinss[i] <- kmeans_result$tot.withinss
  # betweenss[i] <- kmeans_result$betweenss
}

# # Imprimir los resultados
# results <- data.frame(Num_Intervalos = num_intervalos,
#                       Tot_Withinss = tot_withinss,
#                       Betweenss = betweenss,
#                       coef = tot_withinss / Betweenss)
print(results)
? cuales serían las marcas de clase de los intervalos obtenidos?
  




