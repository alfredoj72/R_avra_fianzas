library("ggplot2")
dd = data.frame(x = 0:10, y = 0:10)
g = ggplot(dd, aes(x, y)) + 
  geom_point()

png("figure1-400.png", width = 700, height = 400, type = "cairo-png")
g
dev.off()

jpeg("figure1-400.jpeg", width = 700, height = 400, type = "cairo")
g
dev.off()


dim(png::readPNG("figure1-400.png", TRUE))




library("imager")
original = imager::load.image("figure1-400.jpeg")
d = dim(original)[1:2]
d

KK <- imager::load.image("figure1-400.png")
d <- dim(KK)[1:2]
d

file_info("kkkkk.def", fail= F)

####################################################3
res <- 700
library("imager")
original = imager::load.image("office.jpeg")
d = dim(original)[1:2]
scale = max(d / res)
img = imager::resize(original, 
                     imager::width(original) / scale, 
                     imager::height(original) / scale,
                     interpolation_type = 6)
img
imager::save.image(img, file = "office_max.jpeg")
# Pero esta imagen obtenida no tiene aún el tamaño deseado, es necesario añadir píxeles blancos
square_img = imager::pad(img, 
                         nPix = res - height(img), 
                         axes = "y", 
                         val = "white")
imager::save.image(square_img, file = "office_square.jpeg")
square_img

El tamaño de archivo también se ve reducido
fs::file_info("office_square.jpeg")$size






library(officer)
doc <- read_docx(path="./Publicaciones/word template/pandoc_template.docx")
estilos <- officer::styles_info(doc)  #muestra los estilos existentes


dir <- system.file(package = "officedown", "examples", "bookdown")
file.copy(dir, getwd(), recursive = TRUE, overwrite = TRUE)
rmarkdown::render_site("bookdown")







load("./datos_output/avra_catastro_2022.RData")
contenedor <- avra_catastro_2022
rm(avra_catastro_2022)
#avra_datos_originales <- contenedor[["originales"]]
#avra_catastro <- contenedor[["avra_catastro"]]
tabla_frecuencias  <- contenedor[["tabla_frecuencias"]]
tabla_frecuencias_final  <- contenedor[["tabla_frecuencias_final"]]


load("./datos_output/datos_para_analisis_2022.RData")
datos <- datos_para_analisis_2022[["datos"]]
tabla_frecuencia_filtros <- datos_para_analisis_2022[["tabla_borrados"]]

datos_con_geometria <- datos
datos <- st_drop_geometry(datos)







Barrios_sf <- st_read(dsn = "datos_aux/13_24_BarrioUrbano.shp", quiet = TRUE)

Barrios_sf <- Barrios_sf %>% 
  group_by(cod_mun, nombre, distrito ) %>% 
  summarize(.groups = "drop") %>% 
  mutate(barrio.codigo = row_number(),
         barrio.nombre = nombre,
         barrio.distrito = distrito) %>% 
  select(cod_mun, barrio.codigo, barrio.nombre, barrio.distrito)

# Añade al nombre del barrio el del distrito en aquellos casos que hay 2
# barrios con el mismo nombre pero distinto distrito
Barrios_sf <- Barrios_sf %>% 
  group_by(cod_mun, barrio.nombre) %>% 
  mutate(rep = n(),
         barrio.nombre = ifelse(rep > 1, 
                                paste0(barrio.nombre,". ", barrio.distrito),
                                barrio.nombre),
         rep = NULL
  )







# Gráfico de barras con etiquetas de altura
ggplot(datos, aes(x = variable)) +
  geom_bar(aes(y = (..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(label = ..count..),
            vjust = +1, size = 3,
            color = "cornsilk4") +
  geom_text(stat = "count", aes(y = (..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +2.5, size = 3, color = "cornsilk4") +
  labs(x = "", 
       y = "Frecuencia",
       title = Etiqueta(variable)) +
  theme_minimal()




# Reemplaza "ruta/al/paquete.zip" con la ruta completa a tu archivo ZIP
install.packages("./TinyTeX-1.zip", repos = NULL, type = "source")



tinytex::install_tinytex()
tinytex:::install_prebuilt()

#mirando el log y metiendo el error en chatgpt
tinytex::tlmgr_install("multirow")




datos_para_analisis_anyo <- preparacion_datos(glue("avra_catastro_{anyo_sel}"))

  

  load("./datos_output/avra_catastro_2018.RData")
  datos_para_analisis_2018 <- preparacion_datos("avra_catastro_2018")

  # Salva en el dirctorio datos_output la información
  # Salva en el dirctorio datos_output la información
  save(datos_para_analisis_2018,
       file = "./datos_output/datos_para_analisis_2018.RData")
  write_xlsx(datos_para_analisis_2018[[1]],
             glue("./datos_output/avra_catastro_2018_8_datos_para_analisis.xlsx"))
  write_xlsx(datos_para_analisis_2018[[2]],
             glue("./datos_output/avra_catastro_2018_8b_resumen_del_filtrado.xlsx"))
  
  