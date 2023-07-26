# A partir de los datos de avra genera 3 tablas con los datos que casan con
# 1 vivienda, los que casan con mas de 1 y los que no casan


#BORRAR todos los ELEMENTOS en memoria
rm(list = ls())

# paquetes y directorio de trabajo ----
#instala y carga los paquetes necesarios
paquetes_necesarios = c("readxl","RPostgres","sf","dplyr","writexl")

for (paq in paquetes_necesarios){
  if (!(paq %in% rownames(installed.packages()))){
    install.packages(paq, dependencies = T)}
  library(paq, character.only = T)
}
rm(paq, paquetes_necesarios)


#Carga datos de fianzas
avra <- read_excel("DATOS FIANZAS 2022.xls",     
                  sheet = "Página1_1") 




avra <- avra %>% mutate(id_ams = row_number())
colnames(avra) <- tolower(colnames(avra))
avra$duracion_contrato_años <- sub(",", ".", avra$duracion_contrato_años)
avra$duracion_contrato_años <- round(as.numeric(avra$duracion_contrato_años), 2)
avra$num_habitaciones <- as.integer(avra$num_habitaciones)
avra$número_806 <- as.integer(avra$número_806)
avra$importe_de_la_fianza <- as.integer(avra$importe_de_la_fianza)
avra$importe_de_la_renta <- as.integer(avra$importe_de_la_renta)
avra$nif_cif_arrendador_anonimizado <- as.character(avra$nif_cif_arrendador_anonimizado)
avra$rc_parcela <- substr(avra$referencia_catastral, 1, 14)

#Añado campos para facilitar la corrección de Ref Catastrales erróneas en la
#info de Avra
avra <- avra %>%
  group_by(referencia_catastral) %>%
  mutate( avra_rc_repet = n())

# Guardo una copia de los datos originales
avra_datos_originales <- avra
# AQUI SE PODRÍAN INSERTAR LAS CORRECCIONES

#Traerse los datos de parcelas o de viviendas desde postgree es inviable
#por la cantidad de tiempo que tarda. Es mejor realizar el join en la propia
#base de datos postgre. Para ello antes de nada hay que pasar los datos avra
#a postgre con el usuario sige_owner.
# La consulta la haré a continuación con el usuario sige_ajms 

# Conexión a PostgreSQL para grabar datos 
con_owner <- dbConnect(RPostgres::Postgres(), 
                 dbname = 'dbgis01',  ##Nombre de la BBDD
                 host= 'VMGIS04.cfv.junta-andalucia.es',
                 port= '5444',
                 user= 'sige_owner'  , 
                 password= 'MCG3NtPM'  )  

#BORRO el esquema si es que ya existe
#dbExecute(con_owner, "DROP SCHEMA tmp_avra_alquiler CASCADE")

# Nombre del esquema a verificar
esquema <- "tmp_avra_alquiler"

# Consulta SQL para verificar la existencia del esquema
consulta <- paste("SELECT EXISTS(SELECT 1 FROM information_schema.schemata 
                  WHERE schema_name = '", esquema, "')")

#consulta <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'tmp_avra_alquiler'"

resultado <- dbGetQuery(con_owner, consulta)

if (nrow(resultado) > 0) {
  existe_esquema = TRUE
} else {
  existe_esquema = FALSE
}

# Si no existe el esquema lo crea
if(!existe_esquema) {
  #Creo el esquema
  dbExecute(con_owner, "CREATE SCHEMA tmp_avra_alquiler 
                AUTHORIZATION sige_owner")
  
  dbExecute(con_owner, "GRANT ALL ON SCHEMA tmp_avra_alquiler TO sige_owner")
  dbExecute(con_owner, "GRANT USAGE ON SCHEMA tmp_avra_alquiler TO sige_consulta")
}

rm(consulta, esquema, existe_esquema, resultado)


#Mi objetivo es añadir a la información de avra los datos de cada vivienda
#de la información IECA Catastro
#Para el join de cada vivienda de avra con catastro necesito un campo que 
#contiene la referencia catastral de bien inmueble al que debo añadir el código
#de provincia y el código de municipio según la dirección general de catastro
#Como en la tabla de avra hay registros que no tienen la información de la 
#provincia y/o el municipio no puedo unir dichos campos en la tabla de avra
#sin editar manualmente. En vez de ello lo que voy a hacer es tomar las parcelas
#distintas que aparecen en la tabla de avra y consultar en la información del
#catastro cuales son la provincia y el municipio.
#OJO. Si asociado a alguna parcela obtengo más de un registro con municipio y
#provincia debo decidir a mano cuál de ellas es la correcta.

#Creo una estructura de tabla para añadir todas parcelas distintas
crea_tabla <- "
CREATE TABLE tmp_avra_alquiler.rc_distintas
(
  rc14 character varying(14) NOT NULL,
  CONSTRAINT rc_distintas_pkey PRIMARY KEY (rc14)
)
WITH (
  OIDS=FALSE
);
"

privileg1 <- "
  GRANT SELECT ON TABLE tmp_avra_alquiler.rc_distintas TO admingis_role
"

privileg2 <- "
  GRANT SELECT, REFERENCES ON TABLE tmp_avra_alquiler.rc_distintas TO sige_consulta;
"
# BORRO la tabla de referencias catastrales distintas si es que ya existe
# la borro para que cada vez la genere de nuevo. 

if(dbExistsTable(con_owner,
                 Id(schema = "tmp_avra_alquiler", 
                    table = "rc_distintas"))) {
  dbExecute(con_owner,"DROP TABLE tmp_avra_alquiler.rc_distintas")
}



  dbExecute(con_owner,crea_tabla)
  dbExecute(con_owner,privileg1)
  dbExecute(con_owner,privileg2)
  

  RC_distintas <- unique(avra$rc_parcela)
  RC_distintas <- data.frame(rc14 = unlist(RC_distintas))
  
  #y añado los registros a la tabla recién creada
  st_write(RC_distintas, dsn = con_owner, 
           Id(schema="tmp_avra_alquiler", table = "rc_distintas"),
           append = TRUE)
  rm(RC_distintas)



rm(crea_tabla,privileg1,privileg2)





dbDisconnect(con_owner)
rm(con_owner)

#ahora para realizar la consulta uso el usuario sige_ajms ya que el 
#usuario sige_owner en principio no podía leer los datos del esquema catastro2022
#ya si puede hacerse todo el proceso con el usuario con_owner pero dejo el 
#código como estaba

# Conexión
con_ajms <- dbConnect(RPostgres::Postgres(), 
                 dbname = 'dbgis01',
                 host= 'VMGIS04.cfv.junta-andalucia.es',
                 port= '5444',
                 user= 'sige_ajms', 
                 password= 'Yrc39pbk')


#Realizo la consulta para obtener cada referencia catastral con sus
#código de provincia y municipio del catastro,
#y tambien con las coordenadas x e y

consulta <- "SELECT t1.*, 
             t2.delegacio, t2.municipio
             FROM tmp_avra_alquiler.rc_distintas AS t1
             LEFT JOIN catastro2022.modelo_parcelas_r11 AS t2 ON t1.rc14 = t2.refcat"

# Ejecutar la consulta y obtener los resultados
resultado <- dbGetQuery(con_ajms, consulta)
rm(consulta)

dbDisconnect(con_ajms)
rm(con_ajms)


# En la capa de parcelas de los datos del IECA Catastro que hemos usado en el
# paso anterior hay parcelas repetidas
# Esto puede generar registros duplicados en el resultado, es decir, referencias
# catastrales que en el paso anterior han podido quedar asociadas a más de un
# registro de codigo de delgacion y municipi distinto
# Para eliminar esos duplicados en caso de que existan nos quedamos con los
# registros que son completamente distintos.

# Quedarse con los registros completamente distintos

resultado <- resultado %>%
  group_by(rc14,delegacio,municipio)%>%
  summarise() %>% 
  ungroup()


nregistros <- nrow(resultado)
nunicos <- length(unique(resultado$rc14))

cat(paste("El número de registros al que se ha asignado referencia catastral es",
          nregistros))
cat(paste("El número de Referencias castrales (14 dígitos) distintas",
          nunicos))
mensaje <- paste("ATENCIÓN: Alguna referencia catastral (", nregistros-nunicos,
            ")es ubicada en más de un municipio")
ifelse(nregistros-nunicos != 0, mensaje)


#En el caso del año 2022 se obtienen igual numero de registros en la consulta
#que en la consulta agrupada, luego ninguna de las referencias catastrales (rc14)
#aportadas casa con más de un registro en catastro, es decir, a cada parcela solo
#le añade catastro un municipio


#Pero hay algunos registros cuya referencia catastral no aparece en catastro
#son los siguientes

nreg <- nrow(avra)

avra <-  left_join(avra, resultado, by = c("rc_parcela" = "rc14") )

nreg2 <- nrow(avra)

mensaje <- paste("El número de registros en la tabla con datos originales de",
                 "AVRA pasa de",nreg,"a",nreg2)

ifelse(nregistros-nunicos != 0, mensaje)

rm(resultado, mensaje, nregistros, nunicos, nreg, nreg2)

avra_RC_no_casa <- avra %>% 
                      filter(is.na(delegacio))  %>%
                      select(-rc_parcela, -municipio, -delegacio)

#forma alternativa de hacer lo anterior
# Rc_nocasan <- resultado %>%
#   filter (is.na(delegacio))
# avra_RC_no_casa <- inner_join(avra, Rc_nocasan, by = c("rc_parcela" = "rc14") )
# rm(Rc_nocasan)


#Para continuar trabajando solo con los registros de viviendas de avra
#que si se encuentran en la base de datos IECA Catastro 

avra_RC_si_casa <- avra %>% 
                      filter(!is.na(delegacio)) 

#forma alternativa de hacer lo anterior
# Rc_sicasan <- resultado %>%
#   filter (!is.na(delegacio))
# avra_RC_casa <- right_join(avra, Rc_sicasan, by = c("rc_parcela" = "rc14") )
# rm(Rc_sicasan)


#ya tengo la tabla con las viviendas de Avra que si se encuentran en catastro
#Para unirla a la tabla de viviendas necesito crear el campo que contiene
#el id_bi (deleg || cmuntrib_dgc || parcela || nordenbifis) y
#el rfcd_parcela

avra_RC_si_casa <- avra_RC_si_casa %>%
           mutate(id_bi = paste0(
                           delegacio,
                           municipio,
                           substr(referencia_catastral, 1, 18)),
                  rfcd_parcela = paste0( 
                      delegacio,
                      municipio,
                      substr(referencia_catastral, 1, 14)))%>%
                  select(-rc_parcela)


##Renombro los registros a los que le quiero añadir información catastral
##Vuelvo a llamarle avra pero solo contendrá los registros que tienen Ref Catastral

avra <- avra_RC_si_casa
rm(avra_RC_si_casa)



#Para añadirle la información de las viviendas debo pasar la tabla a postgre antes
#Para crear la estructura de la tabla veo los tipos de campos y longitudes
#en el dataframe que quiero pasar a postgre
campos <- names(avra)
tipos <- sapply(avra, function(col) class(col))
tamanios_maximos <- sapply(avra, function(col) max(nchar(as.character(col))))
listado_campos <- paste(campos, tipos, sep = ": ", tamanios_maximos)
cat(paste(listado_campos, collapse = "\n"))
rm(campos, tipos,tamanios_maximos,listado_campos)



#Creo la estructura de la tabla

crea_tabla <- "
CREATE TABLE tmp_avra_alquiler.fianzas_2022 
(
  codigo_expediente__rue character varying(30) NOT NULL,
  numero_documento character varying(15),
  nif_cif_arrendador_anonimizado character varying(10),
  sexo_arrendador character varying(1),
  tipo_persona_arrendador character varying(1),
  tipo_entidad_arrendador character varying(100),
  sexo_arrendatario character varying(1),
  nacionalidad_arrendatario character varying(20),
  referencia_catastral character varying(20),
  municipio_806 character varying(50),
  provincia_806 character varying(10),
  codigo_ine character varying(5),
  cod_postal_806 character varying(5),
  dirección_806 character varying(255),
  número_806 integer,
  escalera_806 character varying(10),
  planta_806 character varying(10),
  puerta_806 character varying(10),
  duracion_contrato_años numeric(5,2),
  fecha_devengo date,
  num_habitaciones integer,
  tipo_de_arrendamiento character varying(15),
  tipo_actualizacion character varying(50),
  importe_de_la_renta integer,
  importe_de_la_fianza integer,
  id_ams integer,
  avra_rc_repet integer,
  delegacio character varying(2),
  municipio character varying(3),
  id_bi character varying(23),
  rfcd_parcela character varying (19),
  CONSTRAINT fianzas_2022_pkey PRIMARY KEY (id_ams)
)
WITH (
  OIDS=FALSE
);
"
privileg1 <- "
  GRANT SELECT ON TABLE tmp_avra_alquiler.fianzas_2022 TO admingis_role
"

privileg2 <- "
  GRANT SELECT, REFERENCES ON TABLE tmp_avra_alquiler.fianzas_2022 TO sige_consulta;
"

indice1 <- '
 CREATE INDEX fianzas_2022_id_bi_idx
 ON tmp_avra_alquiler.fianzas_2022
 USING btree
 (id_bi COLLATE pg_catalog."default")
 TABLESPACE idx01_admingis;'

indice2 <- '
 CREATE INDEX fianzas_2022_rfcd_parcela_idx
 ON tmp_avra_alquiler.fianzas_2022
 USING btree
 (rfcd_parcela COLLATE pg_catalog."default")
 TABLESPACE idx01_admingis;'

# Conexión a PostgreSQL para grabar datos 
con_owner <- dbConnect(RPostgres::Postgres(), 
                       dbname = 'dbgis01',  ##Nombre de la BBDD
                       host= 'VMGIS04.cfv.junta-andalucia.es',
                       port= '5444',
                       user= 'sige_owner'  , 
                       password= 'MCG3NtPM'  )


if(dbExistsTable(con_owner,
                  Id(schema = "tmp_avra_alquiler", 
                     table = "fianzas_2022"))) {
  dbExecute(con_owner,"DROP TABLE tmp_avra_alquiler.fianzas_2022")
}

dbExecute(con_owner,crea_tabla)
dbExecute(con_owner,privileg1)
dbExecute(con_owner,privileg2)
dbExecute(con_owner,indice1)
dbExecute(con_owner,indice2)
rm(crea_tabla,privileg1,privileg2,indice1,indice2)



st_write(avra, dsn = con_owner, 
         Id(schema="tmp_avra_alquiler", table = "fianzas_2022"),
         append = TRUE)

# Necesito añadir el campo de superficie de parcela catastral a cada vivienda
# para poder discriminar y no usar aquellas viviendas cuya superficie sea
# superior a 3 veces la superficie de parcela, que permite evitar en los cálculos
# las viviendas de avra que en realidad en catastro responden a un bien inmueble
# que contiene más de una vivienda. Tambien le quiero añadir las coordenadas
# de la parcela a cada vivienda
# La tabla de información de parcela suministrada por el IECA contiene muchas
# parcelas duplicadas por lo que necesito construir una tabla en la que cada
# parcela aparezca una sola vez. PAra ello me quedo con el registro de 
# información que contenga la parcela de mayor tamaño y tomo su superficie y
# coordenadas



orden <- "
  CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
SELECT DISTINCT ON (refcat_d) refcat_d, sup, coorx, coory,
  (SELECT COUNT(*)
   FROM catastro2022.modelo_parcelas_r11 AS t2
   WHERE t2.refcat_d = t1.refcat_d) AS num_repeticiones
FROM catastro2022.modelo_parcelas_r11 AS t1
ORDER BY refcat_d,sup DESC;
"

# Si me quedo con el registro de mayor superficie. pero no cuenta el numero de 
# veces que se repite en el catastro la referencia catastral
# orden <- "
# CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
# SELECT DISTINCT ON (refcat_d) refcat_d, sup, coorx, coory
# FROM catastro2022.modelo_parcelas_r11
# ORDER BY refcat_d, sup DESC;
# "

# El código que habia antes y que por los menos era rapido aunque 
# no se si hacia lo que yo pensaba que hacía

# orden <- "
#   CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
#    SELECT refcat_d, sup 
#      FROM catastro2022.modelo_parcelas_r11
#      GROUP BY refcat_d, sup
#      ;
# "



privileg1 <- "
  GRANT SELECT ON TABLE tmp_avra_alquiler.parcelas_supef_tmp TO admingis_role
"

privileg2 <- "
  GRANT SELECT, REFERENCES ON TABLE tmp_avra_alquiler.parcelas_supef_tmp TO sige_consulta;
"

indice1 <- '
 CREATE INDEX fianzas_2022_refcat_d_idx
 ON tmp_avra_alquiler.parcelas_supef_tmp
 USING btree
 (refcat_d COLLATE pg_catalog."default")
 TABLESPACE idx01_admingis;'

if(!dbExistsTable(con_owner,
                  Id(schema = "tmp_avra_alquiler", 
                     table = "parcelas_supef_tmp"))) {
  dbExecute(con_owner,orden)
  dbExecute(con_owner,privileg1)
  dbExecute(con_owner,privileg2)
  dbExecute(con_owner,indice1) #quizas mejor definir como primary key el campo refcat_d
 
}
rm(indice1, orden, privileg1, privileg2)
dbDisconnect(con_owner)
rm(con_owner)

#ahora para realizar la consulta debo usar el usuario sige_ajms ya que el 
#usuario sige_owner no puede leer los datos del esquema catastro2022

# Conexión
con_ajms <- dbConnect(RPostgres::Postgres(), 
                      dbname = 'dbgis01',
                      host= 'VMGIS04.cfv.junta-andalucia.es',
                      port= '5444',
                      user= 'sige_ajms', 
                      password= 'Yrc39pbk')


# consulta <- "SELECT t1.*,
#                  t2.idvcat, t2.tip_const4d_14, t2.stotalocal_14, t2.a_ant_bim
#              FROM tmp_avra_alquiler.fianzas_2022 AS t1
#              LEFT JOIN catastro2022.modelo_vivienda_20221011 AS t2
#                   ON t1.id_bi = t2.id_bi"

consulta <- "SELECT t1.*,
                     t2.idvcat,
                     t2.clase_bim_15,
                     t2.grupo_bim_15,
                     t2.destino_dgc_14,
                     t2.tip_const4d_14,
                     t2.categoria_const_14,
                     t2.tipviv,
                     t2.stotalocal_14, 
                     t2.a_ant_bim,
                     t2.h2o,
                     t2.ea,
                     t2.app,
                     
                     t3.sup AS sup_parcela, 
                     t3.coorx,
                     t3.coory
             FROM tmp_avra_alquiler.fianzas_2022 AS t1
             LEFT JOIN catastro2022.modelo_vivienda_20221011 AS t2
                  ON t1.id_bi = t2.id_bi
             LEFT JOIN tmp_avra_alquiler.parcelas_supef_tmp AS t3
                  ON t1.rfcd_parcela = t3.refcat_d"

# consulta3 <- "SELECT t1.*,
#                  t2.sup as sup_parcela
#              FROM tmp_avra_alquiler.fianzas_2022 AS t1
#              LEFT JOIN tmp_avra_alquiler.parcelas_supef_tmp AS t2
#                   ON t1.rfcd_parcela = t2.refcat_d"

# Ejecutar la consulta y obtener los resultados
resultado <- dbGetQuery(con_ajms, consulta)

rm(consulta)

dbDisconnect(con_ajms)
rm(con_ajms)

#Cuento el numero de enlaces que ha tenido cada registro
resultado <- resultado %>% 
              group_by(id_ams) %>%
              mutate (comp = n()) %>%
              ungroup()

#si el enlace ha dado un registro pero sin valores es que no existe dicha referencia
#pongo el valor a 0 ya que de lo contrario vale NA

resultado$comp <- as.integer(ifelse(is.na(resultado$idvcat), 0, resultado$comp))

#Por último añado los registros cuya Ref Catastral no era encontrada en la capa
#de parcelas
avra_catastro <- bind_rows(resultado, avra_RC_no_casa)
rm(avra, resultado, avra_RC_no_casa)


#calculo una tabla de frecuencias con los 3 tipos de enlaces (0, 1, N) que se 
#corresponden a no enlazados, enlazados 1 a 1, enlaces 1 a N entre registros
#de avra y registros de vivienda

resumen <- avra_catastro %>%
  group_by(id_ams,comp) %>%
  summarise() %>%
  mutate (comp_t = ifelse(comp > 1, "N", as.character(comp)))

frecuencias_absolutas <- table(resumen$comp_t)
frecuencias_relativas <- prop.table(frecuencias_absolutas)
tabla_frecuencias <- cbind(frecuencias_absolutas,100* frecuencias_relativas)
rm(resumen,frecuencias_absolutas,frecuencias_relativas)
# Imprimir la tabla de frecuencias
print(tabla_frecuencias)

# # Por último borro el esquema temporal que he creado en la base de datos.
# con_owner <- dbConnect(RPostgres::Postgres(),
#                        dbname = 'dbgis01',  ##Nombre de la BBDD
#                        host= 'VMGIS04.cfv.junta-andalucia.es',
#                        port= '5444',
#                        user= 'sige_owner'  ,
#                        password= 'MCG3NtPM'  )
# 
# #BORRO el esquema
# dbExecute(con_owner, "DROP SCHEMA tmp_avra_alquiler CASCADE")
# dbDisconnect(con_owner)
# rm(con_owner)

# #Añado campos para facilitar la corrección de Ref Catastrales erróneas en la
# #info de Avra
# avra_catastro <- avra_catastro %>%
#   group_by(referencia_catastral) %>%
#   mutate( avra.RC_repet = n())



#calculo moda, mediana, media, nº repeticiones del valor

library(modeest) # para calcular la moda (mfv)

calcula_frecuencias <- function(campo){
  tabla <- table(campo)
  # df <- as.data.frame(tabla)
  df <- as.character(tabla)
  fr <- paste(names(tabla), df, sep = ": ")
  fr <- paste(fr, collapse = ", ")
  return(fr)
}


avra_catastro <- avra_catastro %>%
  group_by(id_ams) %>%
  mutate(cat.media_superf = mean(stotalocal_14, na.rm = TRUE),
         cat.mediana_superf = median(stotalocal_14, na.rm = TRUE),
         cat.moda_superf = mean(mfv(stotalocal_14), na.rm = TRUE),
         cat.desv_tip_superf = sqrt(var(as.integer(stotalocal_14),na.rm=TRUE)),
         cat.coef_var = (cat.desv_tip_superf/cat.media_superf)*100,
         cat.frecuencias = calcula_frecuencias(stotalocal_14)) %>% 
  ungroup()

rm(calcula_frecuencias)

avra_catastro$cat.media_superf <- as.numeric(avra_catastro$cat.media_superf)
avra_catastro$cat.mediana_superf <- as.numeric(avra_catastro$cat.mediana_superf)
avra_catastro$stotalocal_14 <- as.integer(avra_catastro$stotalocal_14)
avra_catastro$sup_parcela <- as.integer(avra_catastro$sup_parcela)

# Obtener el listado de campos tipo numerico
campos_numericos <- sapply(avra_catastro, function(x) any(class(x) %in% c("numeric")))

# Obtener los nombres de los campos tipo numerico
nombres_campos_numericos <- names(avra_catastro)[campos_numericos]
#nombres_campos_numericos

#Redondear a 2 cifras decimales los campos numericos
avra_catastro <- avra_catastro %>%
  mutate_at(vars(all_of(nombres_campos_numericos)), ~ round(., digits = 2))

rm(campos_numericos, nombres_campos_numericos)

# añadir el campo codigo de INE del municipio. 
# Voy a añadir el codigo ine que le corresponde a la referencia catastral
# Previamente ya he añadido el código delegación y código municipio de catastro
# Ahora a través de ello con la ayuda de una tabla de catastro_gml_2022 añado
# el código ine del municipio.

# Conexión
con_ajms <- dbConnect(RPostgres::Postgres(), 
                      dbname = 'dbgis01',
                      host= 'VMGIS04.cfv.junta-andalucia.es',
                      port= '5444',
                      user= 'sige_ajms', 
                      password= 'Yrc39pbk')


consulta <- "SELECT *
            FROM catastro_gml_2022.metadatos"

# Ejecutar la consulta y obtener los resultados
Municipios_catastro <- dbGetQuery(con_ajms, consulta)
Municipios_catastro <- Municipios_catastro[,c("cod_dgc","cod_ine")]
rm(consulta)

avra_catastro <- avra_catastro %>%
                     mutate(cod_dgc = paste0(delegacio,municipio)) %>%
                            # nif_cif_arrendador_anonimizado = 
                            #   as.character(nif_cif_arrendador_anonimizado)) %>%
                     select(-delegacio, - municipio)

avra_catastro <- left_join(avra_catastro, Municipios_catastro,
                             by = c("cod_dgc" = "cod_dgc") )



dbDisconnect(con_ajms)
rm(con_ajms, Municipios_catastro)


# # Exportar el dataframe a Excel
# write_xlsx(avra_catastro, "avra_catastro.xlsx")
# 
# #salvo una imagen de los datos disponibles para poder partir de ellos sin
# #volver a realizar todas las operaciones
save.image("datos1.RData")
# 









######################################################################
# PASAMOS A SEPARAR LOS REGISTROS DE AVRA_CATASTRO EN GRUPOS
# 1) Los que no casan con información en catastro
#    (Su referencia catastral de parcela no existe o si existe no casa a nivel de
#     bien inmueble vivienda)
# 2) Los que casan con 1 bien inmueble vivienda o si casa con varias viviendas
#    distintas son de tamaño similar (coef. variación <2)
# 3) Los que casan con más de una vivienda. En este caso se ofrece la tabla con
#    los registros de AVRA a los que le pasa esto (1 registro x cada registro
#    origina de AVRA) y la tabla con los registros casados (N registros x cada
#    registro de AVRA) para poder analizar cuál de ellos es el correcto


rm(list =ls())
load("datos1.RData")
rm(tabla_frecuencias)

# GRUPO 1
# Me quedo con los registros que casan con mas de 1 vivienda y cuyo
# coeficiente de variación es inferior a 2
Fianzas_viviendas_N <- avra_catastro %>%
  filter ((comp > 1 & cat.coef_var < 2)) 

# Cambio en los registros el valor de superficie de cada vivienda por la media
# de todas las viviendas con las que casa.
# Y elimino el campo idvcat. 
# De esta forma todos los registros
# de una vivienda avra serán identicos
Fianzas_viviendas_N <- Fianzas_viviendas_N %>%
  mutate (stotalocal_14 = cat.media_superf) %>%
  select (-idvcat)

# Como todos los registros de cada vivienda son identicos me puedo quedar con 
# solo 1 de cada, para ello tomo los registros diferentes

# Los campos numéricos dan problemas para ser comparados por la precisión con
# que se almacenan. Para evitar dichos problemas los redondeo a dos cifras


# Obtener el listado de campos de tipo numerico (real) y redondearlos a dos dígitos
campos_numericos <- sapply(Fianzas_viviendas_N, function(x) any(class(x) %in% c("numeric")))
# Obtener los nombres de los campos numericos
nombres_campos_numericos <- names(Fianzas_viviendas_N)[campos_numericos]
# Redondeo
Fianzas_viviendas_N <- Fianzas_viviendas_N %>%
  mutate_at(vars(all_of(nombres_campos_numericos)), ~ round(., digits = 2))

# Obtener el dataframe con filas verdaderamente distintas
Fianzas_viviendas_N <- distinct(Fianzas_viviendas_N, .keep_all = TRUE)

rm(campos_numericos, nombres_campos_numericos)

# GRUPO 2
# Obtener tambien cuales se pierden para saber cuales son
# Se pierden pero se podrian vincular a traves de bloque portal escalera
Fianzas_viviendas_N_dist_tamanos <- avra_catastro %>%
  filter ((comp > 1 & cat.coef_var >= 2)) 

# Me quedo con solo un registro para cada una de las viviendas tengo (el primero que pilla)
Fianzas_viviendas_N_dist_tamanos_ID <- Fianzas_viviendas_N_dist_tamanos %>%
  distinct(id_ams, .keep_all = TRUE)

# y elimino todos los campos añadidos
# compruebo el lugar del último campo de la tabla original de avra
posicion <- which(names(Fianzas_viviendas_N_dist_tamanos_ID) == "rfcd_parcela")
# y me quedo solo con los campos originales
Fianzas_viviendas_N_dist_tamanos_ID <- 
  Fianzas_viviendas_N_dist_tamanos_ID[,1:posicion]

rm(posicion)



# GRUPO 3
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


# GRUPO 4
# Las viviendas de AVRA que casan con 1 vivienda en los datos IECA Catastro
Fianzas_viviendas_1 <- avra_catastro %>%
  filter (comp == 1)

#Vamos a presentar los resultados

# Los registros que encontramos y coinciden son 1 vivienda o con varias
# pero si es con varias son de tamaño muy similar (coef. variación < 2)
Fianzas_casan_1_vivienda <- bind_rows(Fianzas_viviendas_1, Fianzas_viviendas_N)
rm(Fianzas_viviendas_1, Fianzas_viviendas_N)
# Elimino campos estadísticos
Fianzas_casan_1_vivienda <- Fianzas_casan_1_vivienda %>%
  select(-cat.media_superf, -cat.mediana_superf,
         -cat.moda_superf, -cat.desv_tip_superf,
         -cat.frecuencias, -cat.coef_var)

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

save.image("datos2.RData")

# Exportar el dataframe a Excel
write_xlsx(Fianzas_casan_1_vivienda, "Fianzas_casan_1_vivienda.xlsx")
write_xlsx(Fianzas_no_casan_catastro, "Fianzas_no_casan_catastro.xlsx")
write_xlsx(Fianzas_casan_distintas_viviendas, "Fianzas_casan_distintas_viviendas.xlsx")
write_xlsx(Fianzas_casan_distintas_viviendas_case,
           "Fianzas_casan_distintas_viviendas_cases.xlsx")
