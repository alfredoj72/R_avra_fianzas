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
  # registro de codigo de delgacion y municipi distinto
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
                     t3.coory
             FROM tmp_avra_alquiler.fianzas AS t1
             LEFT JOIN catastro2022.modelo_vivienda_20221011 AS t2
                  ON t1.id_bi = t2.id_bi
             LEFT JOIN tmp_avra_alquiler.parcelas_supef_tmp AS t3
                  ON t1.rfcd_parcela = t3.refcat_d"
  
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
  print("Fin")
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




################################################################################
##########################################################################
# Prepara para el análisis,
# Solo procesa la tabla de viviendas que casan con 1 vivienda
# Realiza varias acciones: genera campos calculados, FILTRA los registros
# válidos para el análisis, añade campos de POTA y secciones censales y crea factores
# Usa solo los registros que casan con 1 vivienda
# Elimina los registros considerados errores
preparacion_datos <- function(datos_entrada){
  
  # Tomo el dataframe que voy a utilizar
  Fianzas_viviendas <- datos_entrada[["Fianzas_casan_1_vivienda"]]

  # CREO CAMPO precio por m2
  Fianzas_viviendas$renta_m2 <- Fianzas_viviendas$importe_de_la_renta /
    Fianzas_viviendas$stotalocal_14
  # CREO CAMPO
  # proporcion existente entre la superficie de la vivienda y la de la parcela
  Fianzas_viviendas$tasa_superf <- Fianzas_viviendas$stotalocal_14 / 
    Fianzas_viviendas$sup_parcela
  # CREO CAMPO superficie por número de habitaciones
  Fianzas_viviendas <- Fianzas_viviendas %>%
    mutate(superf_hab = stotalocal_14/num_habitaciones)
  
  # PASAR TODAS LAS COORDENADAS A HUSO 30
  # Los datos que nos pasa el IECA a cada parcela le pone coordendas en el huso
  # que le corresponde al municipio. Voy a pasarlas todas a huso 30 para ello
  # convierto el dataframe en un objeto sfc y obtengo las coordenadas
  
  # Separar los registros con coorx mayor de 630000 (tienen srs 25829)
  Fianzas_viviendas_25830 <- Fianzas_viviendas %>% filter(coorx <= 630000)
  Fianzas_viviendas_25829 <- Fianzas_viviendas %>% filter(coorx > 630000)
  
  # Crear las capas sf para cada grupo
  capa_sf_25830 <- st_as_sf(Fianzas_viviendas_25830, coords = c("coorx", "coory"), crs = 25830)
  capa_sf_25829 <- st_as_sf(Fianzas_viviendas_25829, coords = c("coorx", "coory"), crs = 25829)
  
  # Transformar las coordenadas de la capa en srs 25829 a srs 25830
  capa_sf_25829_transformada <- st_transform(capa_sf_25829, crs = 25830)
  
  # Unir las capas en una sola capa sf
  Fianzas_viviendas <- rbind(capa_sf_25830, capa_sf_25829_transformada)
  
  # Obtengo las coordenadas a partir de los elementos de la capa
  coords_25830 <- st_coordinates(Fianzas_viviendas)
  
  # Agregar las coordenadas al dataframe de la capa
  Fianzas_viviendas$coorx_25830 <- coords_25830[, 1]
  Fianzas_viviendas$coory_25830 <- coords_25830[, 2]
  
  rm(Fianzas_viviendas_25829, Fianzas_viviendas_25830, capa_sf_25829_transformada,
     coords_25830, capa_sf_25829, capa_sf_25830)
  
  
  # Verificar el SRS de la capa sf final
  #print(st_crs(capa_sf_final))
  
  # ELIMINACION DE REGISTROS CONSIDERADOS NO VALIDOS PARA EL ANALISIS DE
  # EVOLUCION DE LAS FIANZAS RECOGIDAS EN EL REGISTRO DE FIANZAS
  
  # Eliminar los registros cuyos de tipologia de construcción no esté entre los
  # correctos
  borrados1 <- Fianzas_viviendas %>%
    filter(!tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131")) %>%
    nrow()
  print(paste("El número de registros eliminados por disponer tipo de construcción no válido es",borrados1))
  
  Fianzas_viviendas <- Fianzas_viviendas %>%
    filter(tip_const4d_14 %in% c("0111", "0112", "0121", "0122", "0131"))
  
  # Eliminar los registros en los que el codigo INE asignado según la localización
  # de la parcela catatral (cod_ine) sea distinto del asignado por AVRA (codigo_ine)
  #
  # primero tengo que arreglar los codigos de AVRA a los que le falta el 0
  # indicar a paco que esto debería venir arreglado en la tabla que me pasa el
  # tb indicar que hay rc con menos de 20 dígitos que debería cribar
  
  # Cambiar el campo cod_ine agregando un "0" por delante cuando la longitud sea 4
  Fianzas_viviendas <- Fianzas_viviendas %>% 
    mutate(codigo_ine = ifelse(nchar(codigo_ine) == 4, 
                               paste0("0", codigo_ine),
                               codigo_ine))
  
  borrados2 <- Fianzas_viviendas %>%
    filter(codigo_ine != cod_ine) %>%
    nrow()
  
  print(paste("El número de registros eliminados por incongruencia entre el código de municipio asignado por AVRA y el código de municipio correspondiente a la referencia catastral es",borrados2))
  
  Fianzas_viviendas <- Fianzas_viviendas %>%
    filter(codigo_ine == cod_ine)
  
  # un tipo de error que hay que evitar es el de referencias catastrales de avra
  # que en catastro casen pero se refieran a bienes inmuebles que contengan más de 
  # 1 vivienda. El IECA ha separado los bienes inmuebles en más de una vivienda 
  # cuando no existe division horizontal en dicho inmueble, pero si existe división
  # horizontal no separa sus bienes inmuebles en más viviendas auqnue en la tabla 14
  # tengan más direcciones. Estas situaciones deben ser en las que asociado a una
  # renta de vivienda "normal" encontremos una vivienda excesivamente grande.
  # analicemos pues el tamaño en relación a la renta de alquiler, es decir la renta_m2
  
  # 5907109TG3450N0001XY es un ejemplo para AVRA de un bien inbmueble que debería
  # contener varias viviendas en los datos IECA Catastro y solo contiene 1.
  
  # Sería deseable que en el modelo 806 se recogiera un check para indicar si el
  # alquiler se refiere a la totalidad del biene inmueble para el que se recoge
  # la referencia catastral o solo a una parte.
  
  # El corte por los percentiles 0.025 y 0.975 lo he elegido porque obtengo
  # valores de la variable asumibles como correctos y tiene una interpretación fácil:
  #   Quedarte con el 95% central de observaciones
  
  # Q025 <- quantile(Fianzas_viviendas$renta_m2, 0.025, na.rm = TRUE)
  # Q975 <- quantile(Fianzas_viviendas$renta_m2, 0.975, na.rm = TRUE)
  # 
  # borrados <-  Fianzas_viviendas %>%
  #    filter(renta_m2 < Q025 | renta_m2 > Q975) %>%
  #    nrow()
  # 
  # print(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados))
  # 
  # Fianzas_viviendas <- Fianzas_viviendas %>%
  #   filter(renta_m2 > Q025 & renta_m2 < Q975)
  # 
  # rm(Q025, Q975, borrados)
  
  # Hago una versión actualizada en la que se eliminan todos los registros cuya
  # referencia catastral queda en cualquiera de las 2 colas
  
  Q025 <- quantile(Fianzas_viviendas$renta_m2, 0.025, na.rm = TRUE)
  Q975 <- quantile(Fianzas_viviendas$renta_m2, 0.975, na.rm = TRUE)
  
  ref_cat_a_borrar <-  Fianzas_viviendas %>%
    filter(renta_m2 < Q025 | renta_m2 > Q975) %>% 
    distinct(referencia_catastral)
  
  
  borrados3 <- inner_join(Fianzas_viviendas, 
                         ref_cat_a_borrar,
                         by = c("referencia_catastral" = "referencia_catastral")) %>% 
    nrow()
  
  
  Fianzas_viviendas <- anti_join(Fianzas_viviendas, 
                                 ref_cat_a_borrar,
                                 by = c("referencia_catastral" = "referencia_catastral"))
  
  print(paste("El número de registros eliminados por valores extremos en renta/m2 es",borrados3))
  
  tabla_borrados <- data.frame(
    Tipo = c("Tipo de construcción no válido",
             "Incongruencia Cod_INE AVRA y Cod_INE recuperado de referencia catastral",
             "Valores extremos en renta/m2 ", "Sirven para el análisis"),
    casos = c(borrados1, borrados2, borrados3,nrow(Fianzas_viviendas))
  )
  
  rm(ref_cat_a_borrar, borrados1, borrados2, borrados3, Q025, Q975)
  
  
  # INCORPORACIÓN DE INFORMACIÓN DE ADSCRIPCIÓN DE CADA MUNICIPIO A 
  # LA ESTRUCTURA DEL POTA
  # Se añaden los campos que indican la jerarquía del municipio y
  # la unidad territorial a la que pertenece el municipio
  
  adsc_mun <- read_excel("datos_aux/adscripcion municipal definitiva CON POB Y SUP.xls",
                         sheet = "adscripcion municipios",
                         col_types = rep("text", times = 11))
  campos <- tolower(colnames(adsc_mun))
  campos <- gsub(" ", "_", campos)
  colnames(adsc_mun) <- campos
  
  codigo <- paste0("0",adsc_mun$codigo_municipal)
  codigo <- substr(codigo, nchar(codigo)-4, nchar(codigo) )
  
  adsc_mun <- adsc_mun %>% 
    mutate(codigo_municipal = codigo,
           pota.jerarquia = jerarquía_sistema_ciudades,
           pota.unidad_territorial = unidad_territorial) %>% 
    filter(!is.na(provincia)) %>% 
    select(codigo_municipal, pota.jerarquia, pota.unidad_territorial)
  
  Fianzas_viviendas <-  left_join(Fianzas_viviendas, 
                                  adsc_mun, 
                                  by = c("cod_ine" = "codigo_municipal") )
  rm(campos, codigo, adsc_mun)
  
  # INCORPORACIÓN DE INFORMACIÓN DEL BARRIO Y LA SECCION EN LA QUE SE ENCUENTRA
  # CADA VIVIENDA  
  
  # Se añaden los campos nombre del barrio y nombre del distrito de la capa de barrios
  # Según el IECA la capa de Barrios contiene una delimitación aproximada de los
  # Distritos y Barrios de las grandes ciudades andaluzas
  # Tambien se añade el codigo de sección censal de la capa de secciones censales
  # que si recoge todos los municipios de Andalucia
  
  
  # Leer los shapefiles
  Barrios_sf <- st_read(dsn = "datos_aux/13_24_BarrioUrbano.shp", quiet = TRUE)
  Secciones_sf <- st_read(dsn = "datos_aux/13_27_SeccionCensal.shp", quiet = TRUE)
  
  Barrios_sf <- Barrios_sf %>% 
    mutate(barrio.nombre = nombre,
           barrio.distrito = distrito) %>% 
    select(barrio.nombre, barrio.distrito)
  
  Secciones_sf <- Secciones_sf %>% 
    mutate(seccion.codigo = codigo) %>% 
    select(seccion.codigo)
  
  Fianzas_viviendas <- st_join(Fianzas_viviendas, Barrios_sf)
  Fianzas_viviendas <- st_join(Fianzas_viviendas, Secciones_sf)
  
  rm(Barrios_sf, Secciones_sf)
  # FALTA SUSTITUIR NA POR VALORES Y DAR ORDEN A LOS FACTORES
  #Declaro cuáles son los factores. OJO FALTAN ALGUNOS AUN
  

  
  
            
  Fianzas_viviendas <- Fianzas_viviendas %>%
    mutate (sexo_arrendador = factor(sexo_arrendador),
            tipo_persona_arrendador = factor(tipo_persona_arrendador),
            tipo_entidad_arrendador = factor(tipo_entidad_arrendador),
            sexo_arrendatario = factor(sexo_arrendatario),
            nacionalidad_arrendatario = factor(nacionalidad_arrendatario),
            municipio_806 = factor(municipio_806),
            provincia_806 = factor(provincia_806),
            cod_postal_806 = factor(cod_postal_806),
            tipo_de_arrendamiento = factor(tipo_de_arrendamiento),
            tip_const4d_14 = factor(tip_const4d_14),
            cod_ine = factor(cod_ine),
            tipviv = factor(tipviv),
            seccion.codigo = factor(seccion.codigo),
            pota.unidad_territorial = factor(pota.unidad_territorial),
            pota.jerarquia = factor(pota.jerarquia),
            barrio.nombre = factor(barrio.nombre),
            barrio.distrito = factor(barrio.distrito))
  
  # Añado a los resultados la tabla con todo el proceso, el resumen de datos borrados
  # y la tabla de datos originales
  originales  <- datos_entrada[["originales"]]
  return(list(Fianzas_viviendas = Fianzas_viviendas,
              tabla_borrados = tabla_borrados,
              originales = originales))
}

