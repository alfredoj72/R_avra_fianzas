

table(A,B)  # crea tabla de contingencia
xtabs(count ~A + B)  # crea tablas contingencia más complejas
ftable() # crea la tabla de contingencia plana, es decir n

prop.table() # añade proporciones

gmodels::CrossTable(avra_catastro$sexo_arrendador, avra_catastro$sexo_arrendatario)




#------------------------------------------------------------------------------

#Obtener tablas de contingencia a partir de la reclasificación
#de variables continuas.

x <- c(12, 1, 25, 12, 65, 2, 6, 17)
x_agrupada <- cut(x, breaks = c(0, 3, 12, 15, 20, 80),
                  labels = c("Primero", "Segundo", "Tercero", "Cuarto", "Quinto"))
#por def el intervalo es cerrado por la derecha ( right = TRUE),
#con right = FALSE sera cerrado por la izquierda y abierto por la derecha

hemisphere = cut(crateres$Lat, breaks=c(-90, 0, 90))
head(hemisphere)
craterSize = cut(crateres$Diam_km,
                 breaks=c(seq(20, 80, 20), max(crateres$Diam_km)),
                 include.lowest=TRUE)
tabla_crateres = table(hemisphere, craterSize)

#------------------------------------------------------------------------------


el porcentaje que representa un valor
df_tt_2 <- df %>% group_by(Country) %>% 
  summarise(NN = n(), percent = n()/nrow(.) )

otra forma el porcentaje que representa un valor
df_tt_2 <- df %>% group_by(Country) %>%
  summarise (NN = n()) %>%
  mutate(percent = NN / sum(NN))

yo queria meter el q1 y q3
sería
df_tt_2 <- df %>% group_by(Country) %>% 
  summarise(NN = n(), q1= quantile(., 0.25), q3=quantile(.,0.75))

#------------------------------------------------------------------------------

# https://rstudio-pubs-static.s3.amazonaws.com/747687_9703955da60144c8b950014c3b020be7.html
# Voy a probar con tabyl de janitor

library(janitor)

ues <- avra_catastro %>%
  tabyl(sexo_arrendador)

ues

ues %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()



t1 <- avra_catastro %>%
  tabyl(sexo_arrendador, sexo_arrendatario) #, show_na = FALSE)
t1

t2 <- avra_catastro %>%
  tabyl(sexo_arrendador, sexo_arrendatario, show_na = FALSE)
t2

t1 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()   # añade los valores absolutos entre paréntesis

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()   # añade los valores absolutos entre paréntesis


#3 variables
t3 <- avra_catastro %>%
  tabyl(sexo_arrendador,  provincia_806, sexo_arrendatario)

# El resultado es un tabyl de ESCUELA x HTA, filtrado con una
#lista por GENERO
t3


avra_catastro %>%
  tabyl(sexo_arrendador, sexo_arrendatario, provincia_806, show_na = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%    # añade valores absolutos entre parentesis
  adorn_title     # añade el titulo de la variable en columnas


avra_catastro %>%
  tabyl(sexo_arrendador, provincia_806, show_na = FALSE) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title %>%
  knitr::kable()

 library(janitor)
 library(flextable)
library(tidyverse)

TABLA <- avra_catastro %>% 
  tabyl(sexo_arrendador, provincia_806, show_na = FALSE) %>%
  #mutate(sexo_arrendador=factor(sexo_arrendador)) %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting() %>%
  adorn_ns() %>% 
  flextable() %>% 
  fontsize(size = 14, part = "all") %>%
  autofit()

TABLA

TABLA  %>% 
  htmltools_value() # Imprescindible para que se muestre en transparencias html


# esto es un grafico
avra_catastro %>%
  tabyl(sexo_arrendador, provincia_806, show_na = FALSE) %>%
  adorn_percentages("col") %>% 
  pivot_longer(cols =-1, names_to = "provincia_806", values_to = "proporcion") %>% 
  mutate(porcentaje=100*proporcion) %>% 
  mutate(sexo_arrendador=factor(sexo_arrendador)) %>% 
  ggplot(aes(x=provincia_806, y=porcentaje, fill=sexo_arrendador)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=sprintf("%.2f%%",porcentaje)), 
            position=position_dodge(width=0.9),
            vjust=1.5, col="white", fontface="bold") +
  labs(fill="sexo_arrendador")


#------------------------------------------------------------------------------
# EN ANALISIS ARRU USE

library(flextable)
# Establezco parámetros por defecto para tablas
init_flextable_defaults() #reinicia los valores por defecto del paq flextable
set_flextable_defaults(
  big.mark = ".",
  decimal.mark = ",",
  digits = 0,
  # theme_fun = theme_box,
  font.family = "calibri",  
  line_spacing = .8,
  font.color = "gray8", # "#333333",
  font.size = 8,
  border.color = "gray12", # "#333333",
  padding = 4
)
#library(kableExtra)

to_num <- function(x){
  formatC(x, format= "f", digits = 1, big.mark = ".", decimal.mark = ",")
}

camino <- "Q:/Inspeccion/_TRABAJOS/133 Datos estadísticos ARRU/Analisis"
f <- paste0(camino, "/datos_elab/Datos_ARRU.RData" )

load(f) 
rm(f, camino)


table_caption <- c("Tabla 1", "Relación de Áreas de Rehabilitación vigentes. Junio 2022.")

Relacion_ARRU %>%
  mutate( Delimitación = Delimitacion) %>%
  select(id, Municipio, Nombre, Delimitación) %>%
  flextable() %>% 
  autofit() %>%
  add_header_lines(values = rev(table_caption)) %>%
  bold(part = "header", i = 1) %>%
  italic(part = "header", i = c(2:length(table_caption))) %>%
  align(part = "header", i = c(1:length(table_caption)), align = "left") %>%
  border(part = "head", i = c(1:length(table_caption)),
         border = list("width" = 0, color = "black", style = "solid"))


#------------------------------------------------------------------------------

#dAR UN BUEN REPASO A https://epirhandbook.com/es/descriptive-tables.html 







































#------------------------------------------------------------------------------