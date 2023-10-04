# Funcion hecha en casa para mostrar los decimales que queremos:
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

carga_datos_entrada <- function(){
  ##################################################################
  #Carga datos de fianzas años 2018 a 2021
  ##################################################################
  
  dir_actual <- getwd()
  fichero <- paste0( dir_actual,"/datos_input/Expedientes fianzas 2018-2021.xlsx")

  # dir_actual <- getwd()
  # fichero <- glue::glue("{base_dir}/datos_input/Expedientes fianzas 2018-2021.xlsx")
  
  # Especificar los tipos de datos para cada columna
  # lo uso para que no de problemas con la fecha
  types <- c(
    SERIE_ESTADISTICA = "text",
    CODIGO_EXPEDIENTE__RUE = "text",
    NUMERO_DOCUMENTO = "text",               
    NIF_CIF_ARRENDADOR_ANONIMIZADO = "text",
    SEXO_ARRENDADOR = "text",         
    TIPO_PERSONA_ARRENDADOR = "text",        
    TIPO_ENTIDAD_ARRENDADOR = "text",
    SEXO_ARRENDATARIO = "text",
    NACIONALIDAD_ARRENDATARIO = "text",
    REFERENCIA_CATASTRAL = "text",   
    MUNICIPIO_806 = "text",           
    PROVINCIA_806 = "text",                
    CODIGO_INE = "text",        
    COD_POSTAL_806 = "text",     
    DIRECCIÓN_806 = "text",                  
    NÚMERO_806 = "text",        
    ESCALERA_806 = "text",       
    PLANTA_806 = "text",                     
    PUERTA_806 = "text",    
    DURACION_CONTRATO_AÑOS = "text",  
    FECHA_DEVENGO = "date",        
    NUM_HABITACIONES = "text",      
    TIPO_DE_ARRENDAMIENTO = "text",   
    TIPO_ACTUALIZACION = "text",             
    IMPORTE_DE_LA_RENTA = "text",     
    IMPORTE_DE_LA_FIANZA = "text")   
  
  #Carga datos de fianzas
  avra_1 <- read_excel(fichero,     
                       sheet = "Página1_1", col_types = types) 
  
  avra_2 <- read_excel(fichero,     
                       sheet = "Página1_2", col_types = types) 
  
  avra_3 <- read_excel(fichero,     
                       sheet = "Página1_3", col_types = types) 
  
  avra <- rbind(avra_1, avra_2, avra_3)
  
  
  avra <- avra %>% 
    mutate(serie = as.character(format(avra$FECHA_DEVENGO, "%Y")))
  
  comprobacion_serie <- avra %>% 
    filter(SERIE_ESTADISTICA != serie) %>% nrow()
  
  cat(paste("El campo SERIE_ESTADISTICA recoge el año de la FECHA_DEVENGO",
            "salvo en", comprobacion_serie, "casos"))
  
  avra2018_2022 <- avra %>% select(-SERIE_ESTADISTICA,-serie)
  
  rm(dir_actual, fichero, types, comprobacion_serie,avra, avra_1, avra_2, avra_3)
  
  #avra2018_2022 <- rbind(avra2018_2021, avra2022)
  #rm(avra2018_2021, avra2022)
  return(avra2018_2022)
  # Fin carga de datos
}









###########################################################################

añade_campos_catastro <- function(datos_entrada) {
  #browser()
  print("Inicio")
  tiempo_inicio <- Sys.time()
  
  avra <- datos_entrada %>% mutate(id_ams = row_number())
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
    mutate( avra_rc_repet = n()) %>%
    ungroup()
  
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
  consulta <- paste0("SELECT EXISTS(SELECT 1 FROM information_schema.schemata 
                  WHERE schema_name = '", esquema, "')")
  
  #consulta <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'tmp_avra_alquiler'"
  
  resultado <- dbGetQuery(con_owner, consulta)
  
  if (resultado == TRUE) {
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
  #provincia debo decidir cuál de ellas es la correcta.
  
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
  
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Subiendo RC distintas")
  tiempo_inicio <- Sys.time()
  
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
  #código de delegación y municipio del catastro,
  
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
  # registro de codigo de delgacion y municipio distinto
  # Para eliminar esos duplicados en caso de que existan nos quedamos con los
  # registros que son completamente distintos.
  
  # Quedarse con los registros completamente distintos
  
  resultado <- resultado %>%
    group_by(rc14,delegacio,municipio)%>%
    summarise(.groups = "drop")
  
  
  nregistros <- nrow(resultado)
  nunicos <- length(unique(resultado$rc14))
  
  # print(paste("El número de registros al que se ha asignado referencia catastral es",
  #             nregistros))
  # print(paste("El número de Referencias castrales (14 dígitos) distintas",
  #             nunicos))
  mensaje <- paste("ATENCIÓN: Alguna referencia catastral (", nregistros-nunicos,
                   ")es ubicada en más de un municipio")
  
  if (nregistros-nunicos != 0) print(mensaje)
  
  
  #En el caso del año 2022 se obtienen igual numero de registros en la consulta
  #que en la consulta agrupada, luego ninguna de las referencias catastrales (rc14)
  #aportadas casa con más de un registro en catastro, es decir, a cada parcela solo
  #le añade catastro un municipio
  
  
  #Pero hay algunos registros cuya referencia catastral no aparece en catastro
  #son los siguientes
  
  nreg <- nrow(avra)
  
  avra <-  left_join(avra, resultado, by = c("rc_parcela" = "rc14") )
  
  nreg2 <- nrow(avra)
  
  mensaje <- paste("El número de registros que intentaremos casar pasa de",
                   nreg,"a",nreg2 )
  
  if (nregistros-nunicos != 0) print(mensaje)
  
  # Cuando se eliminen los registros codigo de municipio atribuido a traves de la
  # referencia catastral distinto al codigo de municipio de la tabla avra se 
  # eliminarán los registros cuyo código de municipio es incorrecto
  
  rm(resultado, mensaje, nregistros, nunicos, nreg, nreg2)
  #browser()
  avra_RC_no_casa <- avra %>% 
    filter(is.na(delegacio))  %>%
    select(-rc_parcela, -municipio, -delegacio)
  
  #forma alternativa de hacer lo anterior
  # Rc_nocasan <- resultado %>%
  #   filter (is.na(delegacio))
  # avra_RC_no_casa <- inner_join(avra, Rc_nocasan, by = c("rc_parcela" = "rc14") )
  # rm(Rc_nocasan)
  
  #browser()
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
  # campos <- names(avra)
  # tipos <- sapply(avra, function(col) class(col))
  # tamanios_maximos <- sapply(avra, function(col) max(nchar(as.character(col))))
  # listado_campos <- paste(campos, tipos, sep = ": ", tamanios_maximos)
  # cat(paste(listado_campos, collapse = "\n"))
  # rm(campos, tipos,tamanios_maximos,listado_campos)
  
  
  
  #Creo la estructura de la tabla
  
  # crea_tabla <- "
  # CREATE TABLE tmp_avra_alquiler.fianzas
  # (
  #   codigo_expediente__rue character varying(30) NOT NULL,
  #   numero_documento character varying(15),
  #   nif_cif_arrendador_anonimizado character varying(10),
  #   sexo_arrendador character varying(1),
  #   tipo_persona_arrendador character varying(1),
  #   tipo_entidad_arrendador character varying(100),
  #   sexo_arrendatario character varying(1),
  #   nacionalidad_arrendatario character varying(20),
  #   referencia_catastral character varying(20),
  #   municipio_806 character varying(50),
  #   provincia_806 character varying(10),
  #   codigo_ine character varying(5),
  #   cod_postal_806 character varying(5),
  #   dirección_806 character varying(255),
  #   número_806 integer,
  #   escalera_806 character varying(10),
  #   planta_806 character varying(10),
  #   puerta_806 character varying(10),
  #   duracion_contrato_años numeric(5,2),
  #   fecha_devengo date,
  #   num_habitaciones integer,
  #   tipo_de_arrendamiento character varying(15),
  #   tipo_actualizacion character varying(50),
  #   importe_de_la_renta integer,
  #   importe_de_la_fianza integer,
  #   id_ams integer,
  #   avra_rc_repet integer,
  #   delegacio character varying(2),
  #   municipio character varying(3),
  #   id_bi character varying(23),
  #   rfcd_parcela character varying (19),
  #   CONSTRAINT fianzas_pkey PRIMARY KEY (id_ams)
  # )
  # WITH (
  #   OIDS=FALSE
  # );
  # "
  
  crea_tabla <- "
CREATE TABLE tmp_avra_alquiler.fianzas 
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
  rfcd_parcela character varying (19)
)
WITH (
  OIDS=FALSE
);
"
  
  privileg1 <- "
  GRANT SELECT ON TABLE tmp_avra_alquiler.fianzas TO admingis_role
"
  
  privileg2 <- "
  GRANT SELECT, REFERENCES ON TABLE tmp_avra_alquiler.fianzas TO sige_consulta;
"
  
  indice1 <- '
 CREATE INDEX fianzas_id_bi_idx
 ON tmp_avra_alquiler.fianzas
 USING btree
 (id_bi COLLATE pg_catalog."default")
 TABLESPACE idx01_admingis;'
  
  indice2 <- '
 CREATE INDEX fianzas_rfcd_parcela_idx
 ON tmp_avra_alquiler.fianzas
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
                      table = "fianzas"))) {
    dbExecute(con_owner,"DROP TABLE tmp_avra_alquiler.fianzas")
  }
  
  dbExecute(con_owner,crea_tabla)
  dbExecute(con_owner,privileg1)
  dbExecute(con_owner,privileg2)
  dbExecute(con_owner,indice1)
  dbExecute(con_owner,indice2)
  rm(crea_tabla,privileg1,privileg2,indice1,indice2)
  
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Subiendo tabla de fianzas")
  tiempo_inicio <- Sys.time()
  
  st_write(avra, dsn = con_owner, 
           Id(schema="tmp_avra_alquiler", table = "fianzas"),
           append = TRUE)
  
  # Necesito añadir el campo de superficie de parcela catastral a cada vivienda
  # para poder discriminar y no usar aquellas viviendas cuya superficie sea
  # superior a 3 veces la superficie de parcela, que permite evitar en los cálculos
  # las viviendas de avra que en realidad en catastro responden a un bien inmueble
  # que contiene más de una vivienda. Tambien le quiero añadir las coordenadas
  # de la parcela a cada vivienda
  # La tabla de información de parcela suministrada por el IECA contiene muchas
  # parcelas duplicadas por lo que necesito construir una tabla en la que cada
  # parcela aparezca una sola vez. Para ello me quedo con el registro de 
  # información que contenga la parcela de mayor tamaño y tomo su superficie y
  # coordenadas
  # Esta tabla no es preciso recalcularla. Solo se calcula una vez cuando se 
  # cargan los datos de catastro del IECA
  
  
  orden <- "
  CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
SELECT DISTINCT ON (refcat_d) refcat_d, sup, coorx, coory, n_bi, n_bi_sindh,
  (SELECT COUNT(*)
   FROM catastro2022.modelo_parcelas_r11 AS t2
   WHERE t2.refcat_d = t1.refcat_d) AS num_repeticiones
FROM catastro2022.modelo_parcelas_r11 AS t1
ORDER BY refcat_d,sup DESC;
"
  
  
  #   orden <- "
  #   CREATE TABLE tmp_avra_alquiler.parcelas_supef_tmp AS
  # SELECT DISTINCT ON (refcat_d) refcat_d, sup, coorx, coory,
  #   (SELECT COUNT(*)
  #    FROM catastro2022.modelo_parcelas_r11 AS t2
  #    WHERE t2.refcat_d = t1.refcat_d) AS num_repeticiones
  # FROM catastro2022.modelo_parcelas_r11 AS t1
  # ORDER BY refcat_d,sup DESC;
  # "
  
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
 CREATE INDEX fianzas_refcat_d_idx
 ON tmp_avra_alquiler.parcelas_supef_tmp
 USING btree
 (refcat_d COLLATE pg_catalog."default")
 TABLESPACE idx01_admingis;'
  
  if(!dbExistsTable(con_owner,
                    Id(schema = "tmp_avra_alquiler", 
                       table = "parcelas_supef_tmp"))) {
    
    transcurrido <-   Sys.time() - tiempo_inicio
    print(transcurrido)
    print("Creando tabla auxiliar de parcelas con info de superficie")
    tiempo_inicio <- Sys.time()
    
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
  #              FROM tmp_avra_alquiler.fianzas AS t1
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
                     t3.coory, 
                     t3.n_bi, 
                     t3.n_bi_sindh,
                     
                     t4.nbi_tot,
                     t4.nbi_v,
                     t4.nviv
             FROM tmp_avra_alquiler.fianzas AS t1
             LEFT JOIN catastro2022.modelo_vivienda_20221011 AS t2
                  ON t1.id_bi = t2.id_bi
             LEFT JOIN tmp_avra_alquiler.parcelas_supef_tmp AS t3
                  ON t1.rfcd_parcela = t3.refcat_d
             LEFT JOIN catastro2022.modelo_edificio_20221011 AS t4
                  ON t2.idef = t4.idef"
  
  # consulta3 <- "SELECT t1.*,
  #                  t2.sup as sup_parcela
  #              FROM tmp_avra_alquiler.fianzas AS t1
  #              LEFT JOIN tmp_avra_alquiler.parcelas_supef_tmp AS t2
  #                   ON t1.rfcd_parcela = t2.refcat_d"
  
  # Ejecutar la consulta y obtener los resultados
  resultado <- dbGetQuery(con_ajms, consulta)
  
  rm(consulta)
  
  dbDisconnect(con_ajms)
  rm(con_ajms)
  
  #Cuento el numero de enlaces que ha tenido cada registro y lo llamo comp
  resultado <- resultado %>% 
    group_by(id_ams) %>%
    mutate (comp = n()) %>%
    ungroup()
  
  #en la tabla resultado aparecen todos los registros de la tabla avra ya que
  #el enlace se hace con left_join
  #si en la tabla resultado un registro tiene el campo idvcat como NA es 
  #porque el registro no ha enlazado con la tabla de viviendas y por tanto
  #el campo comp debe valer 0
  #browser()
  #resultado$comp <- as.integer(ifelse(is.na(resultado$idvcat), 0, resultado$comp))
  
  #Por último añado los registros cuya Ref Catastral no era encontrada en la capa
  #de parcelas
  avra_catastro <- bind_rows(resultado, avra_RC_no_casa)
  rm(avra, resultado, avra_RC_no_casa)
  
  avra_catastro$comp <- as.integer(ifelse(is.na(avra_catastro$idvcat), 0,
                                          avra_catastro$comp))
  
  #calculo una tabla de frecuencias con los 3 tipos de enlaces (0, 1, N) que se 
  #corresponden a no enlazados, enlazados 1 a 1, enlaces 1 a N entre registros
  #de avra y registros de vivienda
  
  resumen <- avra_catastro %>%
    group_by(id_ams,comp) %>%
    summarise(.groups = "drop") %>%
    mutate (comp_t = ifelse(comp > 1, "N", as.character(comp)))
  #browser()
  frecuencias_absolutas <- table(resumen$comp_t)
  frecuencias_relativas <- prop.table(frecuencias_absolutas)
  tabla_frecuencias <- cbind(frecuencias_absolutas,100* frecuencias_relativas)
  tabla_frecuencias <- as.data.frame(tabla_frecuencias)
  names(tabla_frecuencias) <- c("Frec. Absoluta", "Frec. Relativa")
  tabla_frecuencias$enlace <- rownames(tabla_frecuencias)
  tabla_frecuencias <- tabla_frecuencias %>% select(enlace, everything())
  rm(resumen,frecuencias_absolutas,frecuencias_relativas)
  # Imprimir la tabla de frecuencias
  #print(tabla_frecuencias)
  
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
  
  # calcula_frecuencias <- function(campo){
  #   tabla <- table(campo)
  #   # df <- as.data.frame(tabla)
  #   df <- as.character(tabla)
  #   fr <- paste(names(tabla), df, sep = ": ")
  #   fr <- paste(fr, collapse = ", ")
  #   return(fr)
  # }
  
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Calculando estadísticas de superficie para cada vivienda")
  tiempo_inicio <- Sys.time()
  
  # avra_catastro <- avra_catastro %>%
  #   group_by(id_ams) %>%
  #   mutate(cat.media_superf = mean(stotalocal_14, na.rm = TRUE),
  #          cat.mediana_superf = median(stotalocal_14, na.rm = TRUE),
  #          cat.moda_superf = mean(mfv(stotalocal_14), na.rm = TRUE),
  #          cat.desv_tip_superf = sqrt(var(as.integer(stotalocal_14),na.rm=TRUE)),
  #          cat.coef_var = (cat.desv_tip_superf/cat.media_superf)*100,
  #          cat.frecuencias = calcula_frecuencias(stotalocal_14)) %>% 
  #   ungroup()
  
  avra_catastro <- avra_catastro %>%
    group_by(id_ams) %>%
    mutate(cat.media_superf = mean(stotalocal_14, na.rm = TRUE),
           cat.desv_tip_superf = sqrt(var(as.integer(stotalocal_14),na.rm=TRUE)),
           cat.coef_var = (cat.desv_tip_superf/cat.media_superf)*100) %>% 
    ungroup()
  
  #rm(calcula_frecuencias)
  
  avra_catastro$cat.media_superf <- as.numeric(avra_catastro$cat.media_superf)
  #avra_catastro$cat.mediana_superf <- as.numeric(avra_catastro$cat.mediana_superf)
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
  
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Añadiendo código municipal a partir de la referencia catastral")
  tiempo_inicio <- Sys.time()
  
  avra_catastro <- left_join(avra_catastro, Municipios_catastro,
                             by = c("cod_dgc" = "cod_dgc") )
  
  
  
  dbDisconnect(con_ajms)
  rm(con_ajms, Municipios_catastro)
  
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
  #browser()
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Separando la información de fianzas atendiendo a su conexión con datos de catastro")
  tiempo_inicio <- Sys.time()
  
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
  # Fianzas_casan_1_vivienda <- Fianzas_casan_1_vivienda %>%
  #   select(-cat.media_superf, -cat.mediana_superf,
  #          -cat.moda_superf, -cat.desv_tip_superf,
  #          -cat.frecuencias, -cat.coef_var)
  
  Fianzas_casan_1_vivienda <- Fianzas_casan_1_vivienda %>%
    select(-cat.media_superf, 
           -cat.desv_tip_superf,
           -cat.coef_var)
  
  
  
  # Creo campos calculados
  
  # CREO CAMPO precio por m2
  Fianzas_casan_1_vivienda$renta_m2 <- Fianzas_casan_1_vivienda$importe_de_la_renta /
    Fianzas_casan_1_vivienda$stotalocal_14
  # CREO CAMPO
  # proporcion existente entre la superficie de la vivienda y la de la parcela
  Fianzas_casan_1_vivienda$tasa_superf <- Fianzas_casan_1_vivienda$stotalocal_14 /
    Fianzas_casan_1_vivienda$sup_parcela
  # CREO CAMPO superficie por número de habitaciones
  Fianzas_casan_1_vivienda <- Fianzas_casan_1_vivienda %>%
    mutate(superf_hab = stotalocal_14/num_habitaciones)
  
  # PASAR TODAS LAS COORDENADAS A HUSO 30
  # Los datos que nos pasa el IECA a cada parcela le pone coordendas en el huso
  # que le corresponde al municipio. Voy a pasarlas todas a huso 30 para ello
  # convierto el dataframe en un objeto sfc y obtengo las coordenadas
  
  # Separar los registros con coorx mayor de 630000 (tienen srs 25829)
  Fianzas_viviendas_25830 <- Fianzas_casan_1_vivienda %>% filter(coorx <= 630000)
  Fianzas_viviendas_25829 <- Fianzas_casan_1_vivienda %>% filter(coorx > 630000)
  
  # Crear las capas sf para cada grupo
  capa_sf_25830 <- st_as_sf(Fianzas_viviendas_25830, coords = c("coorx", "coory"), crs = 25830)
  capa_sf_25829 <- st_as_sf(Fianzas_viviendas_25829, coords = c("coorx", "coory"), crs = 25829)
  
  # Transformar las coordenadas de la capa en srs 25829 a srs 25830
  capa_sf_25829_transformada <- st_transform(capa_sf_25829, crs = 25830)
  
  # Unir las capas en una sola capa sf
  Fianzas_casan_1_vivienda <- rbind(capa_sf_25830, capa_sf_25829_transformada)
  
  # Obtengo las coordenadas a partir de los elementos de la capa
  coords_25830 <- st_coordinates(Fianzas_casan_1_vivienda)
  
  # Agregar las coordenadas al dataframe de la capa
  Fianzas_casan_1_vivienda$coorx_25830 <- coords_25830[, 1]
  Fianzas_casan_1_vivienda$coory_25830 <- coords_25830[, 2]
  
  #Hago algunos calculos necesarios 
  Fianzas_casan_1_vivienda <- Fianzas_casan_1_vivienda %>% 
    mutate(n_bi = coalesce(n_bi, n_bi_sindh),
           categoria = as.numeric(
             ifelse(substr(categoria_const_14,5,5) %in% c("A","B","C"),
                    "0",
                    substr(categoria_const_14,5,5))),
           a_ant_bim = as.numeric(a_ant_bim)
           
    ) %>% 
    select(-n_bi_sindh)

  rm(Fianzas_viviendas_25829, Fianzas_viviendas_25830, capa_sf_25829_transformada,
     coords_25830, capa_sf_25829, capa_sf_25830)
  
  
  # Verificar el SRS de la capa sf final
  #print(st_crs(Fianzas_casan_1_vivienda))
  
  
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
  
  casan_0 <- nrow(Fianzas_no_casan_catastro)
  casan_1 <- nrow(Fianzas_casan_1_vivienda)
  casan_N <- nrow(Fianzas_casan_distintas_viviendas)
  tabla_frecuencias_final <- data.frame(
    tipo = c("No casan", "Casan 1 (coef var <2)", "Casan N"),
    casos = c(casan_0, casan_1, casan_N)
  )
  tabla_frecuencias_final$frec_relativa <- 100 * tabla_frecuencias_final$casos /
    sum(tabla_frecuencias_final$casos)
  
  
  transcurrido <-   Sys.time() - tiempo_inicio
  print(transcurrido)
  print("Fin de la conexión con datos de catastro")
  tiempo_inicio <- Sys.time()
  
  return(list(originales = avra_datos_originales,
              avra_catastro = avra_catastro,
              tabla_frecuencias = tabla_frecuencias,
              tabla_frecuencias_final = tabla_frecuencias_final,
              Fianzas_casan_1_vivienda = Fianzas_casan_1_vivienda,
              Fianzas_no_casan_catastro = Fianzas_no_casan_catastro,
              Fianzas_casan_distintas_viviendas = Fianzas_casan_distintas_viviendas,
              Fianzas_casan_distintas_viviendas_case = Fianzas_casan_distintas_viviendas_case))
  
  
  
}




##########################################################################
##########################################################################
# Funcion que salva los ficheros generados con la función añade_campos_catastro
salva_tablas_avra_catastro <- function(datos) {
  #browser()
  avra_datos_originales <- datos[["originales"]]
  avra_catastro <- datos[["avra_catastro"]]
  tabla_frecuencias  <- datos[["tabla_frecuencias"]]
  tabla_frecuencias_final  <- datos[["tabla_frecuencias_final"]]
  Fianzas_casan_1_vivienda <- datos[["Fianzas_casan_1_vivienda"]]
  Fianzas_no_casan_catastro <- datos[["Fianzas_no_casan_catastro"]]
  Fianzas_casan_distintas_viviendas <- datos[["Fianzas_casan_distintas_viviendas"]]
  Fianzas_casan_distintas_viviendas_case <- datos[["Fianzas_casan_distintas_viviendas_case"]]
  
  ###################################################
  # SALVADO DE DATOS
  
  # Exportar el dataframe a Excel
  #print(tabla_frecuencias)
  
  nombre_objeto <- deparse(substitute(datos))
  camino <- paste0("./datos_output/",nombre_objeto,"_")
  
  write_xlsx(avra_datos_originales, paste0(camino,"_1_originales.xlsx"))
  write_xlsx(avra_catastro, paste0(camino,"_2_avra_catastro.xlsx"))
  write_xlsx(tabla_frecuencias, paste0(camino,"_3_resumen_enlaces.xlsx"))
  write_xlsx(tabla_frecuencias_final, paste0(camino,"_7resumen_enlaces_final.xlsx"))
  write_xlsx(Fianzas_casan_1_vivienda, paste0(camino,"_5_casan_1_vivienda.xlsx"))
  write_xlsx(Fianzas_no_casan_catastro, paste0(camino,"_4_no_casan_catastro.xlsx"))
  write_xlsx(Fianzas_casan_distintas_viviendas, paste0(camino,"_6_casan_distintas_viviendas.xlsx"))
  write_xlsx(Fianzas_casan_distintas_viviendas_case,
             paste0(camino,"_6b_casan_distintas_viviendas_cases.xlsx"))
  
  # rm(avra_datos_originales,
  #    avra_catastro,
  #    tabla_frecuencias,
  #    Fianzas_casan_1_vivienda,
  #    Fianzas_no_casan_catastro, 
  #    Fianzas_casan_distintas_viviendas,
  #    Fianzas_casan_distintas_viviendas_case)
}


##########################################################################
##########################################################################
# Funcion que salva los ficheros generados con la función añade_campos_catastro
salva_tablas_avra_catastro_alt <- function(datos, anyo) {
  #browser()
  avra_datos_originales <- datos[["originales"]]
  avra_catastro <- datos[["avra_catastro"]]
  tabla_frecuencias  <- datos[["tabla_frecuencias"]]
  tabla_frecuencias_final  <- datos[["tabla_frecuencias_final"]]
  Fianzas_casan_1_vivienda <- datos[["Fianzas_casan_1_vivienda"]]
  Fianzas_no_casan_catastro <- datos[["Fianzas_no_casan_catastro"]]
  Fianzas_casan_distintas_viviendas <- datos[["Fianzas_casan_distintas_viviendas"]]
  Fianzas_casan_distintas_viviendas_case <- datos[["Fianzas_casan_distintas_viviendas_case"]]
  
  ###################################################
  # SALVADO DE DATOS
  
  # Exportar el dataframe a Excel
  #print(tabla_frecuencias)
  
  #nombre_objeto <- deparse(substitute(datos))
  nombre_objeto <- glue("avra_catastro_{anyo}")
  camino <- paste0("./datos_output/",nombre_objeto,"_")
  
  write_xlsx(avra_datos_originales, paste0(camino,"_1_originales.xlsx"))
  write_xlsx(avra_catastro, paste0(camino,"_2_avra_catastro.xlsx"))
  write_xlsx(tabla_frecuencias, paste0(camino,"_3_resumen_enlaces.xlsx"))
  write_xlsx(tabla_frecuencias_final, paste0(camino,"_7resumen_enlaces_final.xlsx"))
  write_xlsx(Fianzas_casan_1_vivienda, paste0(camino,"_5_casan_1_vivienda.xlsx"))
  write_xlsx(Fianzas_no_casan_catastro, paste0(camino,"_4_no_casan_catastro.xlsx"))
  write_xlsx(Fianzas_casan_distintas_viviendas, paste0(camino,"_6_casan_distintas_viviendas.xlsx"))
  write_xlsx(Fianzas_casan_distintas_viviendas_case,
             paste0(camino,"_6b_casan_distintas_viviendas_cases.xlsx"))
  
  # rm(avra_datos_originales,
  #    avra_catastro,
  #    tabla_frecuencias,
  #    Fianzas_casan_1_vivienda,
  #    Fianzas_no_casan_catastro, 
  #    Fianzas_casan_distintas_viviendas,
  #    Fianzas_casan_distintas_viviendas_case)
}


################################################################################
##########################################################################
# Prepara para el análisis,
# Solo procesa la tabla de viviendas que casan con 1 vivienda
# Realiza varias acciones: FILTRA los registros válidos para el análisis, 
# añade campos de POTA y secciones censales y crea factores
# Usa solo los registros que casan con 1 vivienda
# Elimina los registros considerados errores


preparacion_datos <- function(datos_entrada){
  datos <- datos_entrada[["Fianzas_casan_1_vivienda"]]
  
  # Eliminar los registros cuyos de tipologia de construcción no esté entre los
  # correctos
  borrados1 <- datos %>%
    filter(!tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131")) %>%
    nrow()
  print(paste("El número de registros eliminados por disponer tipo de construcción no válido es",borrados1))
  
  datos <- datos %>%
    filter(tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131"))
  
  
  datos <- datos %>% 
    mutate(codigo_ine = ifelse(nchar(codigo_ine) == 4, 
                               paste0("0", codigo_ine),
                               codigo_ine))
  
  borrados2 <- datos %>%
    filter(codigo_ine != cod_ine) %>%
    nrow()
  
  print(paste("El número de registros eliminados por incongruencia entre el código de municipio asignado por AVRA y el código de municipio correspondiente a la referencia catastral es",borrados2))
  
  datos <- datos %>%
    filter(codigo_ine == cod_ine)
  
  
  #Eliminar registros cuya superficie sea inferior a 25.5 metros cuadrados
  
  borrados3 <- datos %>%
    filter(stotalocal_14 <= 25.5) %>%
    nrow()
  
  print(paste("El número de registros eliminados por tamaño inferior al exigido para vivienda es",borrados3))
  
  datos <- datos %>% 
    filter(stotalocal_14 > 25.5)
  
  
  # Alquileres que corresponden a Referencias Catastrales que se repiten
  # Si una referencia catastral se repite es porque se ha alquilado más de una
  # vez en un año o porque en realidad la referencia catastral corresponde
  # a un bien inmueble que contiene muchas viviendas no separadas individualmente
  
  # Tras un análisis considerando los factores:
  # * se repite la referencia catastral en los registros de avra
  # * existe o no división horizontal 
  # * tipología de vivienda
  
  # Decido eliminar del estudio (parte 1/2):
  # Cuando se repite la ref cat  y no hay división horizontal elimino todos los 
  # registros en los que tampoco el IECA ha encontrado viviendas ( o dicho de otra
  # forma, salvo aquellos en los que el IECA ha detectado la existencia de más de 
  # una vivienda)
  
  borrados4 <- datos %>%
    filter(avra_rc_repet > 1 & n_bi == 1 & nviv == 1) %>%
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de RC repetidas y referidas edificios sin división horizontal y viviendas única",borrados4))
  
  datos <- datos %>% 
    filter(!(avra_rc_repet > 1 & n_bi == 1 & nviv == 1))
  
  # Decido eliminar del estudio (parte 2/2):
  # Cuando se repite la ref cat, hay división horizontal y la tipología de 
  # vivienda es colectiva y sin embargo el número de viviendas es 1 (es decir, 
  # aunque hay división horizontal solo se ha detectado una vivienda y el resto es
  # garaje o comercial, etc ). En eses caso elimino dichos registros
  
  borrados5 <- datos %>%
    filter(avra_rc_repet > 1 & 
             n_bi > 1 &
             substr(tip_const4d_14, 1, 3) == "011" &
             nviv == 1) %>% 
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de RC repetidas ubicadas en edificios de viviendas colectivas con división horizontal pero con solo una vivienda es",borrados5))
  
  datos <- datos %>% 
    filter(!(avra_rc_repet > 1 & 
               n_bi > 1 &
               substr(tip_const4d_14, 1, 3) == "011" &
               nviv == 1))
  
  # Los siguientes filtros se deben a combinaciones de calidad, tamaño y renta/m2
  # que se entienden muy poco probables y en consecuencia se piensa que la referencia
  # catastral recogida contiene más elementos del que realmente se alquila
  Q05 <- quantile(datos$renta_m2, 0.05, na.rm = TRUE, type = 4)
  Q025 <- quantile(datos$renta_m2, 0.025, na.rm = TRUE, type = 4)
  Q95 <- quantile(datos$renta_m2, 0.95, na.rm = TRUE, type = 4)
  
  # pongo el límite de 150 porque entiendo que en viviendas de dicho tamaño 
  # es muy poco probable el alquiler social y sin embargo el precio renta_m2 
  # es con toda probabilidad de un alquiler social
  borrados6 <- datos %>%
    filter( stotalocal_14 > 150 & renta_m2 < Q05 & categoria <= 4 ) %>% 
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de viviendas de buena/muy buena calidad (<=4), tamaño grande (>150m2) y renta/m2 baja (< Q025)",borrados6))
  
  datos <- datos %>% 
    filter(!( stotalocal_14 > 150 & renta_m2 < Q05 & categoria <= 4 ))
  
  
  borrados7 <- datos %>%
    filter( renta_m2 < Q05 & categoria <= 3 ) %>% 
    nrow()
  
  print(paste("El número de registros eliminados por tratarse de viviendas de muy buena calidad (<=3) y renta/m2 baja (< Q05)",borrados7))
  
  datos <- datos %>% 
    filter(!( renta_m2 < Q05 & categoria <= 3 ))

  
  # borrados8 <- datos %>%
  #   filter( renta_m2 > Q95 & categoria > 6 ) %>% 
  #   nrow()
  # 
  # print(paste("El número de registros eliminados por tratarse de viviendas de baja calidad (>6) y renta/m2 alta (> Q95)",borrados8))
  # 
  # datos <- datos %>% 
  #   filter(!( renta_m2 > Q95 & categoria > 6 ))


  
  
  tabla_borrados <- data.frame(
    Tipo = c("Tipo de construcción no válido",
             "Incongruencia Cod_INE AVRA y Cod_INE recuperado de referencia catastral",
             "Superficie de inferior a mínima permitida en vivienda",
             "RC repetidas en edif sin div horizontal y vivienda unica",
             "RC repetida en vivienda con div horizontal pero solo 1 vivienda ",
             "Buena/muy buena calidad (<=4), grande (>150m2) y renta/m2 baja(< Q025) ",
             "Muy buena calidad (<=4) y renta/m2 baja (< Q05)",
             "Sirven para el análisis"),
    casos = c(borrados1, borrados2, borrados3,borrados4,
              borrados5, borrados6, borrados7, nrow(datos))
  )
  
  rm( borrados1, borrados2, borrados3, borrados4, borrados5, borrados6, borrados7)
  
  #browser()
  # INCORPORACIÓN DE INFORMACIÓN DE ADSCRIPCIÓN DE CADA MUNICIPIO A 
  # LA ESTRUCTURA DEL POTA
  # Se añaden los campos que indican la jerarquía del municipio y
  # la unidad territorial a la que pertenece el municipio
  # Además se define el campo como factor con todas unidades territoriales
  # para que aunque alguna no tenga casos aparezca en los listados
  
  adsc_mun <- read_excel("datos_aux/adscripcion municipal definitiva CON POB Y SUP.xls",
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  #Añade un cero y se queda con los 5 últimos dígitos
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.tipo_unidad = tipo_de_unidad_territorial,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.tipo_unidad, pota.unidad_territorial)
  
  datos <-  left_join(datos, 
                      adsc_mun, 
                      by = c("cod_ine" = "codigo_municipal") )
  
  
  listado_POTA <- unique(adsc_mun$pota.unidad_territorial)
  
  datos <- datos %>% 
    mutate( pota.unidad_territorial = factor(pota.unidad_territorial, levels = listado_POTA))
            
  rm(campos, codigo, adsc_mun)
  
  # INCORPORACIÓN DE INFORMACIÓN DEL BARRIO Y LA SECCION EN LA QUE SE ENCUENTRA
  # CADA VIVIENDA  
  
  # Se añaden los campos codigo y nombre del barrio y nombre del distrito de la capa de barrios
  # Según el IECA la capa de Barrios contiene una delimitación aproximada de los
  # Distritos y Barrios de las grandes ciudades andaluzas
  # Tambien se añade el codigo de sección censal de la capa de secciones censales
  # que si recoge todos los municipios de Andalucia
  # Además se define el campo como factor tanto las secciones como los distritos
  # para que aunque alguna no tenga casos aparezca en los listados
  # Si no existe el archivo que contiene las capas con los atributos, lo crea

  if (!file.exists(here("datos_output","capas_para_mapas.Rdata"))) {
    source("Funciones.R")
    # Ejecuta el script para crear las capas 
    Pasar_capas_shp_a_R()
    } 
  
  # Carga las capas
  load(file = here("datos_output","capas_para_mapas.Rdata"))
  
  barrios_sf <- barrios_sf %>% select (barrio.cod_ine, 
                                       barrio.codigo, 
                                       barrio.nombre,
                                       barrio.distrito)
  secciones_sf <- secciones_sf %>%  select(seccion.codigo,
                                           seccion.distrito)
  
  datos <- st_join(datos, barrios_sf)
  datos <- st_join(datos, secciones_sf)
  
 
# definición de factores

  ######## DEFINICION DE FACTORES ############
  #
  
  # Si convierto los NA en un nivel del factor comenzará a ordenarse como el resto
  # de los niveles, si lo dejo como NA se van siempre al último de la lista.
  
  # datos <- datos %>%
  #   mutate (tipo_persona_arrendador = as.character(tipo_persona_arrendador),
  #           sexo_arrendador = as.character(sexo_arrendador))
  
  # Para obtener los resultados de datos para BADEA es preciso contar con todos
  # los municipios, todas las secciones censales y todos los distritos censales
  # Para ello voy a definir los factores con todos ellos y así podré obtenerlos
  # en los resultados aunque no tengan datos asociados
  # Los factores de secciones censales, distritos censales, unidades POTA y barrios
  # lo hago en la función preparacion_datos al tiempo que añado la información.
  # El factor de los municipios lo hago aqui
  
  # Leo los municipios de la capa de municipios de datos_aux. En los script de 
  # creación de mapas leo los datos de WMF. Aquí lo hago distinto.
  
  # Aqui poner la creación del factor a partir de una tabla de municipios del IECA
  # en vez de hacerlo desde la capa
  #browser()
  Municipios <- st_drop_geometry(municipio_sf) %>% 
    arrange(provincia, nombre) %>% 
    select(cod_mun, nombre) %>% 
    distinct(cod_mun, nombre)
  
  datos <- left_join(datos, 
                     Municipios,
                     by = c("cod_ine" = "cod_mun"))  
  
  datos <- datos %>% 
    mutate(cod_ine = factor(cod_ine, levels = Municipios$cod_mun),
           nombre= factor(nombre, levels = Municipios$nombre)
    )
 
  #browser()  
  #rm(Municipios_sf, Municipios)
  

  
  #Creo factor con todos los barrios
  # barrios <- barrios_sf %>% 
  #       st_drop_geometry(barrios_sf)
  # 
  # datos <- datos %>% 
  #   mutate(barrio.codigo = factor(barrio.codigo, levels = barrios$barrio.codigo)
  #   )
  # 
  # rm(barrios_sf, barrios)
  
  datos <- datos %>%
    #mutate (tipo_persona_arrendador = replace_na(tipo_persona_arrendador,"NEspec")) %>% # no hay casos
    mutate (tipo_persona_arrendador = factor(tipo_persona_arrendador, 
                                             levels = c("F", "J"), 
                                             labels = c("Física", "Jurídica")),
            tipo_entidad_arrendador = as.factor(tipo_entidad_arrendador),
            sexo_arrendatario = factor(sexo_arrendatario,
                                       levels = c("M", "V"),
                                       labels = c("Mujeres", "Hombres")),
            nacionalidad_arrendatario = factor(nacionalidad_arrendatario),
            provincia_806 = factor (provincia_806, 
                                    labels = c("Almería", "Cádiz","Córdoba","Granada","Huelva","Jaén","Málaga","Sevilla"))
    )
  
  datos <- datos %>% 
    mutate(
      sexo_arrendador = ifelse(tipo_persona_arrendador == "Jurídica", 
                               "No procede", sexo_arrendador ),
      sexo_arrendador = factor(sexo_arrendador,
                               levels = c("M", "V", "No procede"),
                               labels = c("Mujeres", "Hombres", "No procede"))
    )
  
  
  datos <- datos %>%
    mutate (  tipo_de_arrendamiento = case_when(     #otra forma podría ser usando na_if()
      tipo_de_arrendamiento == "AMUEBLADO"       ~ "Amueblado", 
      tipo_de_arrendamiento == "SIN AMUEBLAR"    ~ "Sin Amueblar",
      TRUE                                 ~ NA_character_) , 
      tipo_de_arrendamiento = factor(tipo_de_arrendamiento))
  
  
  anyos <-cut(datos$duracion_contrato_años,
              breaks = c(0,1,3,5,max(datos$duracion_contrato_años, na.rm = TRUE)),
              right = TRUE,    #Intervalos cerrados por la derecha
              include.lowest = TRUE,  # Para que incluya el valor máximo
              dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("1 año",">1 - 3 años",">3 - 5 años",">5 años")
  
  datos <- datos %>% mutate(f.durac_contrato = factor(anyos, labels =etiquetas))
  
  
  #max <- max(datos$num_habitaciones,  na.rm = TRUE)
  habit_agrupa <- 4  # A partir de 4 habitaciones los agrupa en una sola barra
  cortes <- c(seq(0, habit_agrupa, 1),max(datos$num_habitaciones, na.rm = T))
  etiquetas <- c(seq(1,habit_agrupa,1),glue("{habit_agrupa+1} o más"))
  n_hab <-  cut(datos$num_habitaciones, 
                breaks = cortes,
                rigth = TRUE,
                include.lowest = TRUE)
  
  datos <- datos %>% mutate(f.hab = factor(n_hab, labels =etiquetas))
  
  
  rentas <-cut(datos$importe_de_la_renta,
               breaks = c(0,300,500,700,900,1200,max(datos$importe_de_la_renta, na.rm = TRUE)),
               right = FALSE,    #Intervalos cerrados por la izquierda
               include.lowest = TRUE,  # Para que incluya el valor máximo
               dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("<300€","300€ - 499€","500€ - 699€",
                 "700€ - 899€","900€ - 1.199€",">=1.200€")
  
  datos <- datos %>% mutate(f.renta_alq = factor(rentas, labels =etiquetas))


  
######## DEFINICION DE FACTORES ############
# FACTORES CORRESPONDIENTES A CAMPOS AÑADIDOS A LA TABLA ORIGINAL DE DATOS DE AVRA
  
  superf <-cut(datos$stotalocal_14,
               breaks = c(min(datos$stotalocal_14, na.rm = TRUE),
                          45,65,85,105,120,150,max(datos$stotalocal_14, na.rm = TRUE)),
               right = FALSE,    #Intervalos cerrados por la izquierda
               include.lowest = TRUE,  # Para que incluya el valor máximo
               dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("Hasta 45","45 - <65","65 - <85",
                 "85 - <105","105 - <120", "120 - <150","150 o más")
  
  datos <- datos %>% mutate(f.super = factor(superf, labels =etiquetas))
  
  
  renta_aux <-cut(datos$renta_m2,
                  breaks = c(min(datos$renta_m2, na.rm = TRUE),
                             2,4,6,8,10,max(datos$renta_m2, na.rm = TRUE)),
                  right = FALSE,    #Intervalos cerrados por la izquierda
                  include.lowest = TRUE,  # Para que incluya el valor máximo
                  dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("Menos 2","2 a menos de 4","4 a menos de 6",
                 "6 a menos de 8","8 a menos de 10","10 o más")
  
  datos <- datos %>% mutate(f.renta_m2 = factor(renta_aux, labels =etiquetas))
  
  
  antig <-cut(datos$a_ant_bim,
              breaks = c(min(datos$a_ant_bim, na.rm = TRUE),
                         1960,1970,1980,1990,2000,2010,
                         max(datos$a_ant_bim, na.rm = TRUE)),
              right = FALSE,    #Intervalos cerrados por la izquierda
              include.lowest = TRUE,  # Para que incluya el valor máximo
              dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("Antes 1960","1960 - <1970","1970 - <1980",
                 "1980 - <1990","1990 - <2000","2000 - <2010", "Desde 2010")
  
  datos <- datos %>% mutate(f.antig_bi = factor(antig, labels =etiquetas))
  
  
  
  
  datos <- datos %>%
    mutate(f.tipolog = case_when(substr(tip_const4d_14, 1, 3) == "011"  ~ "Plurifamiliar",
                                 substr(tip_const4d_14, 1, 3) %in% c("012","013") ~ "Unifamiliar"),
           f.tipolog = factor(f.tipolog))
  
  
  
  antig <-cut(datos$a_ant_bim,
              breaks = c(min(datos$a_ant_bim, na.rm = TRUE),
                         1960,1970,1980,1990,2000,2010,
                         max(datos$a_ant_bim, na.rm = TRUE)),
              right = TRUE,    #Intervalos cerrados por la izquierda
              include.lowest = TRUE,  # Para que incluya el valor máximo
              dig.lab = 10)  #dígitos usados sin que se muestren en formato científico
  
  etiquetas <- c("Hasta 1960","1961 - 1970","1971 - 1980",
                 "1981 - 1990","1991 - 2000","2001 - 2010", "Desde 2011")
  
  datos <- datos %>% mutate(f.antig_bi = factor(antig, labels =etiquetas))
  
  ####
  datos_poblacion_2022 <- read.csv("datos_aux/datos_poblacion_2022.txt", 
                                   header= TRUE,
                                   sep = ";",
                                   colClasses = "character")
  
  datos_poblacion_2022$Valor = as.numeric(datos_poblacion_2022$Valor)
  
  pob_aux <- cut(datos_poblacion_2022$Valor,
                 breaks = c(min(datos_poblacion_2022$Valor),
                            5000,10000,20000,50000,100000,500000,
                            max(datos_poblacion_2022$Valor)),
                 right = FALSE,
                 include.lowest = TRUE,
                 dig.lab = 10)
  
  Etiquetas <- c("Menos de 5.000","5.000 - <10.000","10.000 - <20.000",
                 "20.000 - <50.000", "50.000 - <100.000", "100.000 - <500.000",
                 "500000 o más")
  
  datos_poblacion_2022 <- datos_poblacion_2022 %>% 
    mutate(f.tam_pob = factor(pob_aux, labels = Etiquetas)) %>% 
    select(CODIGO_INE3, f.tam_pob)
  
  datos <-  left_join(datos, 
                      datos_poblacion_2022, 
                      by = c("codigo_ine" = "CODIGO_INE3") )
  
  #######
  
  
  datos <- datos %>% 
    mutate(pota.jerarquia = factor(pota.jerarquia,
                                   levels = c("Ciudad principal","Ciudad media 1", "Ciudad media 2",
                                              "Centro rural o pequeña ciudad 1", "Centro rural o pequeña ciudad 2",
                                              "Asentamiento cabecera municipal")))
  
  
  #   Constructo de Persona Física + Tipo Persona Jurídica 
  
  datos <- datos %>% 
    mutate(f.persona_fj = ifelse(
      tipo_persona_arrendador == "Física", "Persona Física",as.character(tipo_entidad_arrendador)),
      f.persona_fj = factor(f.persona_fj, levels = c("Persona Física", 
                                                     unique(as.character(tipo_entidad_arrendador))))
    )
  
  
  datos <- datos %>%  mutate(calidad = substr(categoria_const_14,5,5))
  datos <- datos %>% 
    mutate(calidad = factor(calidad,
                            levels = c("A","B","C",as.character(seq(1,9,1)))))
  
  datos <- datos %>% 
    mutate(h2o = ifelse(is.na(h2o),0,h2o),
           h2o = factor(h2o, levels = c(0,1), labels = c("Sin", "Con")))
  
  
  
  datos <- datos %>% 
    mutate(ea = ifelse(is.na(ea),0,ea),
           ea = factor(ea, levels = c(0,1), labels = c("Sin", "Con")))
  
  datos <- datos %>% 
    mutate(app = ifelse(is.na(app),0,app),
           app = factor(app, levels = c(0,1), labels = c("Sí", "N")))
  
  
  
  # FIN DE DEFINICION DE FACTORES
  
  
  
  # datos <- datos %>%
  #   mutate (municipio_806 = factor(municipio_806),
  #           cod_postal_806 = factor(cod_postal_806),
  #           tip_const4d_14 = factor(tip_const4d_14),
  #           cod_ine = factor(cod_ine),
  #           tipviv = factor(tipviv),
  #           seccion.codigo = factor(seccion.codigo),
  #           barrio.nombre = factor(barrio.nombre),
  #           barrio.distrito = factor(barrio.distrito))
  
  # Añado a los resultados la tabla con todo el proceso, el resumen de datos borrados
  # y la tabla de datos originales
  originales  <- datos_entrada[["originales"]]
  return(list(datos = datos,
              tabla_borrados = tabla_borrados,
              originales = originales))
  
}



#> ############## FUNCION CARGAR CAPAS Y AÑADIR CAMPOS  ########################
#> Lee la información de las capas shape de provincias, municipios, barrios y
#> secciones. Se renombra los campos de dichas capas y se queda solo con los
#> códigos de identificación. A continuación crea las capas de unión de barrios,
#> distritos censales y unidades del pota.
#> También genera una datos básicos de resumen para cada capa y añade dichos 
#> campos a las capas
cargar_capas_y_añadir_campos <- function(){
  
  if (!file.exists(here("datos_output","capas_para_mapas.Rdata"))) {
    source("Funciones.R")
    # Ejecuta el script para crear las capas 
    Pasar_capas_shp_a_R()
  } 
  # Carga las capas
  load(file = here("datos_output","capas_para_mapas.Rdata"))
  

  
  # Obtener resúmenes de datos para todos los niveles de información

  datos_provincia <- datos %>%
    group_by(provincia_806) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_provincia <- datos_provincia %>% 
    mutate(codigo = c("04","11","14","18","21","23","29","41"))
  
  datos_POTA <- datos %>%
    group_by(pota.unidad_territorial) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_municipio <- datos %>%
    group_by(codigo_ine) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")

  #browser()
  
  datos_barrios <- datos %>%
    group_by(barrio.codigo) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_barrios_tipolog <- datos %>%
    group_by(barrio.codigo, f.tipolog ) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop") %>% 
    pivot_wider(names_from = f.tipolog,
                values_from = c(casos, mediana_renta_m2, 
                                mediana_renta, mediana_superf),
                names_glue = "{f.tipolog}_{.value}",)
  
  
  datos$tamanio <- ifelse(datos$stotalocal_14 >= 90, "Grande","Pequeña")
  datos_barrios_tamanio <- datos %>%
    group_by(barrio.codigo, tamanio ) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop") %>% 
    pivot_wider(names_from = tamanio,
                values_from = c(casos, mediana_renta_m2, 
                                mediana_renta, mediana_superf),
                names_glue = "{tamanio}_{.value}",)
  datos$tamanio <- NULL
  
  datos_secciones <- datos %>%
    group_by(seccion.codigo) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_barrios_union <- datos %>%
    group_by(barrio.distrito) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  datos_distrito <- datos %>%
    group_by(seccion.distrito) %>%
    summarise(casos = n(),
              mediana_renta_m2 = median(renta_m2),
              mediana_renta = median(importe_de_la_renta),
              mediana_superf = median(stotalocal_14),
              .groups = "drop")
  
  
  
  
  # Añadir los datos numéricos a las capas para poder representarlos en mapas

  provincia_sf <- provincia_sf %>% 
    left_join (datos_provincia, by = c("codigo" = "codigo")) # %>% 
  # mutate(etiqueta = paste(provincia,"\n",casos))
  
  
  municipio_sf <- municipio_sf %>% 
    left_join (datos_municipio, by = c("cod_mun" = "codigo_ine"))    %>%
    mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0
  
  POTA_sf <- POTA_sf %>% 
    left_join (datos_POTA, by = c("pota.unidad_territorial" = "pota.unidad_territorial"))%>%
    mutate(casos = coalesce(casos, 0))  #sustituye los NA por 0
  
  barrios_sf <- barrios_sf %>% 
    left_join (datos_barrios, by = c("barrio.codigo" = "barrio.codigo"))%>%
    left_join (datos_barrios_tipolog, by = c("barrio.codigo" = "barrio.codigo")) %>% 
    left_join (datos_barrios_tamanio, by = c("barrio.codigo" = "barrio.codigo")) %>% 
    mutate(casos = coalesce(casos, 0)) 
  
  secciones_sf <- secciones_sf %>% 
    left_join (datos_secciones, by = c("seccion.codigo" = "seccion.codigo"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  
  
  union_barrios_sf <- union_barrios_sf %>% 
    left_join (datos_barrios_union, by = c("distrito" = "barrio.distrito"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  distritos_sf <- distritos_sf %>% 
    left_join (datos_distrito, by = c("seccion.distrito" = "seccion.distrito"))%>%
    mutate(casos = coalesce(casos, 0)) 
  
  
  rm(datos_provincia, datos_municipio, datos_POTA, datos_barrios, 
     datos_barrios_union, datos_secciones, datos_distrito)
  
  
  save(provincia_sf, municipio_sf, POTA_sf, barrios_sf,
       secciones_sf, union_barrios_sf, distritos_sf,
       file = "datos_output/capas_con_datos_para_mapas.Rdata")
  
}


#> ############## FUNCION PARA CARGAR CAPAS   ########################
#> Lee la información de las capas shape de provincias, municipios, barrios y
#> secciones. Se renombra los campos de dichas capas y se queda solo con los
#> códigos de identificación. A continuación crea las capas de unión de barrios,
#> distritos censales y unidades del pota.

Pasar_capas_shp_a_R <- function(){
  # Leer capas en formato shapefiles 
  # ## Carga de capas shp ubicadas en directorio local
  provincia_sf <- st_read(dsn = "./datos_aux/13_01_Provincia.shp", quiet = TRUE)
  municipio_sf <- st_read(dsn = "./datos_aux/13_01_TerminoMunicipal.shp", quiet = TRUE)
  
  barrios_sf <- st_read(dsn = "./datos_aux/13_24_BarrioUrbano.shp", quiet = TRUE)
  
  # añado un campo identificador a la capa de barrios que no lo tiene
  barrios_sf <- barrios_sf %>% 
    group_by(cod_mun, municipio, nombre, distrito ) %>% 
    summarize(.groups = "drop") %>% 
    mutate(barrio.codigo = row_number(),
           barrio.cod_ine = cod_mun,
           barrio.nombre = nombre,
           barrio.distrito = distrito) %>% 
    select(cod_mun, municipio, barrio.cod_ine, barrio.codigo,
           barrio.nombre, barrio.distrito)
  
  # Añade al nombre del barrio el del distrito en aquellos casos que hay 2
  # barrios con el mismo nombre pero distinto distrito
  barrios_sf <- barrios_sf %>% 
    group_by(cod_mun, barrio.nombre) %>% 
    mutate(rep = n(),
           barrio.nombre = ifelse(rep > 1, 
                                  paste0(barrio.nombre,". ", barrio.distrito),
                                  barrio.nombre),
           rep = NULL
    ) %>% 
    ungroup()
  
  # hago un "dissolve" de barrios atendiendo al distrito
  # y añado cod_mun y municipio para que la capa resultado contenga dichos campos
  union_barrios_sf <- barrios_sf %>% 
    group_by(barrio.distrito, cod_mun, municipio) %>%
    summarize(.groups = "drop") 
  
# Secciones del DERA  
# secciones_sf <- st_read(dsn = "./datos_aux/13_27_SeccionCensal.shp", quiet = TRUE)
  
 #Cargar el seccionado del padrón de 2022.
  secciones_sf <- st_read(dsn = "./datos_aux/secciones_censales_2022_01_padron.shp",
                          quiet = TRUE)
  
  
  secciones_sf <- secciones_sf %>% 
    mutate(cod_mun = substr(codsecc,1,5),
           seccion.codigo = codsecc,
           seccion.distrito = substr(codsecc,1,7)) %>% 
    select(cod_mun, municipio, seccion.codigo, seccion.distrito)
  
  distritos_sf <- secciones_sf %>%
    group_by(seccion.distrito, cod_mun, municipio ) %>%
    summarize(.groups = "drop") 
  
  # La capa del POTA no tiene un campo código y los nombres de los ambitos
  # territoriales no coinciden con los del dataframe datos que procede de la 
  # tabla de adscripciones de municipios al POTA.
  # Por tanto si agrupo la información de los alquileres atendiendo al campo
  # nombre de la unidad territorial no lo puedo casar con la capa.
  # Es decir, no me sirve la capa de unidades del POTA del DERA. Tengo que
  # construirme una capa de unidades POTA con los nombres de unidades territoriales
  # que tengo en datos, para ello hago un "dissolve" de los municipios
  # En R, el dissolve se consigue simplemente agrupando en un objeto de tipo sf
  
  
  # obtengo la tabla de municipios con la adscripción al pota
  adsc_mun <- read_excel("datos_aux/adscripcion municipal definitiva CON POB Y SUP.xls",
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  #Añade un cero y se queda con los 5 últimos dígitos
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.unidad_territorial)
  
  # añado a los municipios la información de adscripción al POTA  
  municipio_sf <-  left_join(municipio_sf, 
                             adsc_mun, 
                             by = c("cod_mun" = "codigo_municipal") )
  
  rm(campos, codigo)
  
  # disuelvo los municipios un unidades territoriales del POTA
  POTA_sf <- municipio_sf %>% group_by(pota.unidad_territorial) %>% summarize() 
  
  # plot(provincia_sf)
  
  # # Simplificar los polígonos para acelerar los proceso
  # municipio_sf <- st_simplify(municipio_sf, dTolerance = 1)
  # provincia_sf <- st_simplify(provincia_sf, dTolerance = 1)
  # # No lo uso porque no veo diferencia de tiempo y sin embargo si se producen algunos
  # # cambios en el comportamiento de la capa ya que pasa de MULTIPOLYGON a GEOMETRY

  
  save(provincia_sf, municipio_sf, POTA_sf, barrios_sf,
       secciones_sf, union_barrios_sf, distritos_sf,
       file = here("datos_output","capas_para_mapas.Rdata"))
  
}
