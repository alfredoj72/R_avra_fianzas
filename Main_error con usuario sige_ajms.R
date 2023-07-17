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

#Establece directorio de trabajo
#setwd("Q:/Inspeccion/_TRABAJOS/133 Datos estadísticos ARRU/Analisis")

#Carga datos de fianzas
avra <- read_excel("DATOS FIANZAS 2022.xls",     
                   sheet = "Página1_1") 
avra <- avra %>% mutate(id_ams = row_number())
colnames(avra) <- tolower(colnames(avra))
avra$duracion_contrato_años <- sub(",", ".", avra$duracion_contrato_años)
avra$duracion_contrato_años <- as.numeric(avra$duracion_contrato_años)
avra$num_habitaciones <- as.numeric(avra$num_habitaciones)
avra$número_806 <- as.numeric(avra$número_806)


#Traerse los datos de parcelas o de viviendas desde postgree es inviable
#por la cantidad de tiempo que tarda. Es mejor realizar el join en la propia
#base de datos postgre. Para ello antes de nada hay que pasar los datos avra
#a postgre con el usuario sige_owner.
# La consulta la haré a continuación con el usuario sige_ajms o no, ya veremos


# Conexión a PostgreSQL para grabar datos 
con_owner <- dbConnect(RPostgres::Postgres(), 
                       dbname = 'dbgis01',  ##Nombre de la BBDD
                       host= 'VMGIS04.cfv.junta-andalucia.es',
                       port= '5444',
                       user= 'sige_owner'  , 
                       password= 'MCG3NtPM'  )  

#BORRO el esquema si es que ya existe
dbExecute(con_owner, "DROP SCHEMA tmp_avra_alquiler CASCADE")

#Creo el esquema
dbExecute(con_owner, "CREATE SCHEMA tmp_avra_alquiler 
                AUTHORIZATION sige_owner")

dbExecute(con_owner, "GRANT ALL ON SCHEMA tmp_avra_alquiler TO sige_owner")
dbExecute(con_owner, "GRANT USAGE ON SCHEMA tmp_avra_alquiler TO admingis_role")
dbExecute(con_owner, "GRANT ALL ON SCHEMA tmp_avra_alquiler TO sige_ajms")
dbDisconnect(con_owner)
rm(con_owner)


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
#OJO. Si asociada a alguna parcela obtengo más de un registro con municipio y
#provincia debo decidir a mano cuál de ellas es la correcta.

#ahora ya con el usuario sige_ajms creo las tablas
#ahora entro como sige_ajms y creo una tabla
con_ajms <- dbConnect(RPostgres::Postgres(), 
                      dbname = 'dbgis01',
                      host= 'VMGIS04.cfv.junta-andalucia.es',
                      port= '5444',
                      user= 'sige_ajms', 
                      password= 'Yrc39pbk')

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

dbExecute(con_ajms,crea_tabla)
dbExecute(con_ajms,privileg1)
dbExecute(con_ajms,privileg2)
rm(crea_tabla,privileg1,privileg2)

#y añado los registros a la tabla recién creada
avra$rc_parcela <- substr(avra$referencia_catastral, 1, 14)
RC_distintas <- unique(avra$rc_parcela)
RC_distintas <- data.frame(rc14 = unlist(RC_distintas))

st_write(RC_distintas, dsn = con_ajms, 
         Id(schema="tmp_avra_alquiler", table = "rc_distintas"),
         append = TRUE)

rm(RC_distintas)


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


#En la capa de parcelas de los datos del IECA Catastro hay parcelas repetidas
#Esto genera registros duplicados en el resultado, para eliminar esos duplicados
#nos quedamos con los registros que son completamente distintos.

# Quedarse con los registros completamente distintos

resultado <- resultado %>%
  group_by(rc14,delegacio,municipio)%>%
  summarise()


cat(paste("Número de registros", nrow(resultado)))
cat(paste("Número de Referencias castrales 14dígitos distintas",
          (length(unique(resultado$rc14)))))

#En el caso del año 2022 se obtienen igual numero de registros en la consulta
#que en la consulta agrupada, luego ninguna de las referencias catastrales (rc14)
#aportadas casa con más de un registro en catastro, es decir, a cada parcela solo
#le añade catastro un municipio

#Pero hay algunos registros cuya referencia catastral no aparece en catastro
#son los siguientes


avra <-  left_join(avra, resultado, by = c("rc_parcela" = "rc14") )
rm(resultado)
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
#Para unirla a la tabla de viviendas y a la de parcelas necesito crear los
#campos que contienen
#el id_bi (deleg || cmuntrib_dgc || parcela || nordenbifis) y
#el rfcd_parcela (deleg || cmuntrib_dgc || parcela )

avra_RC_si_casa <- avra_RC_si_casa %>%
  mutate(id_bi = paste0(
    delegacio,
    municipio,
    substr(avra_RC_si_casa$referencia_catastral, 1, 18)),
    rfcd_parcela = paste0( 
      delegacio,
      municipio,
      substr(avra_RC_si_casa$referencia_catastral, 1, 14)))%>%
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
  nif_cif_arrendador_anonimizado integer,
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
  duracion_contrato_años real,
  fecha_devengo date,
  num_habitaciones integer,
  tipo_de_arrendamiento character varying(15),
  tipo_actualizacion character varying(50),
  importe_de_la_renta real,
  importe_de_la_fianza real,
  id_ams integer,
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

privileg3 <- "
  GRANT ALL ON TABLE tmp_avra_alquiler.fianzas_2022 TO sige_ajms;
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


dbExecute(con_ajms,crea_tabla)
dbExecute(con_ajms,privileg1)
dbExecute(con_ajms,privileg2)
dbExecute(con_ajms,privileg3)
FALLA AQUI
dbExecute(con_ajms,indice1)
dbExecute(con_ajms,indice2)
rm(crea_tabla,privileg1,privileg2,indice1,indice2)



st_write(avra, dsn = con_owner, 
         Id(schema="tmp_avra_alquiler", table = "fianzas_2022"),
         append = TRUE)

# Necesito añadir el campo de superficie de parcela catastral a cada vivienda
# para poder discriminar y no usar aquellas viviendas cuya superficie sea
# superior a 3 veces la superficie de parcela, que permite evitar en los cálculos
# las viviendas de avra que en realidad en catastro responden a un bien inmueble
# que contiene más de una vivienda
# La tabla de información de parcela suministrada por el IECA contiene muchas
# parcelas duplicadas por lo que necesito construir una tabla en la que cada
# parcela aparezca una sola vez para añadir el campo de superficie de parcela
# a cada vivienda de avra

orden <- "
  CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
   SELECT refcat_d, sup 
     FROM catastro2022.modelo_parcelas_r11
     GROUP BY refcat_d, sup
     ;
"
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

dbExecute(con_owner,orden)
dbExecute(con_owner,privileg1)
dbExecute(con_owner,privileg2)
dbExecute(con_owner,indice1) #quizas mejor definir como primary key el campo refcat_d

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

consulta2 <- "SELECT t1.*,
                     t2.idvcat, t2.tip_const4d_14, t2.stotalocal_14, t2.a_ant_bim,
                     t3.sup AS sup_parcela
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
# resultado <- dbGetQuery(con_ajms, consulta)
resultado <- dbGetQuery(con_ajms, consulta2)
# resultado3 <- dbGetQuery(con_ajms, consulta3)
rm(consulta2)

dbDisconnect(con_ajms)
rm(con_ajms)

#Cuento el numero de enlaces que ha tenido cada registro
resultado <- resultado %>% group_by(id_ams) %>% mutate (comp = n())

#si el enlace ha dado un registro pero sin valores es que no existe dicha referencia
#pongo el valor a 0 ya que de lo contrario vale NA

resultado$comp <- ifelse(is.na(resultado$idvcat), 0, resultado$comp)

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

#Añado campos para facilitar la corrección de Ref Catastrales erróneas en la
#info de Avra
avra_catastro <- avra_catastro %>%
  group_by(referencia_catastral) %>%
  mutate( avra.RC_repet = n())



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
         cat.frecuencias = calcula_frecuencias(stotalocal_14))

rm(calcula_frecuencias)

avra_catastro$stotalocal_14 <- as.integer(avra_catastro$stotalocal_14)
avra_catastro$cat.media_superf <- as.integer(avra_catastro$cat.media_superf)
#Fianzas_viviendas$cat.moda_superf <- as.integer(Fianzas_viviendas$cat.moda_superf)
avra_catastro$cat.mediana_superf <- as.integer(avra_catastro$cat.mediana_superf)
avra_catastro$a_ant_bim <- as.integer(avra_catastro$a_ant_bim)




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
  mutate(cod_dgc = paste0(delegacio,municipio),
         nif_cif_arrendador_anonimizado = 
           as.character(nif_cif_arrendador_anonimizado)) %>%
  select(-delegacio, - municipio)

avra_catastro <- inner_join(avra_catastro, Municipios_catastro,
                            by = c("cod_dgc" = "cod_dgc") )

dbDisconnect(con_ajms)
rm(con_ajms)




# Exportar el dataframe a Excel
write_xlsx(avra_catastro, "avra_catastro.xlsx")

#salvo una imagen de los datos disponibles para poder partir de ellos sin
#volver a realizar todas las operaciones
save.image("datos.RData")

