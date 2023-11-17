datos_poblacion_2018_2022 <- read.csv("datos_aux/datos_poblacion_2018_2022.txt", 
                                 header= TRUE,
                                 sep = ";",
                                 colClasses = "character")

datos_poblacion_2018_2022$Valor = as.numeric(datos_poblacion_2018_2022$Valor)

datos_poblacion_2018_2022 <- pivot_wider(datos_poblacion_2018_2022,
                                         names_from = Anual,
                                         names_glue = "pob_{Anual}",
                                         values_from = Valor)
            

# Lista de campos de población para los años 2019 hasta 2022
campos_poblacion <- c("pob_2018", "pob_2019", "pob_2020", "pob_2021", "pob_2022")

# Aplicar la operación cut a cada columna
datos_poblacion_2018_2022 <- datos_poblacion_2018_2022 %>%
  mutate(across(all_of(campos_poblacion), 
                ~ cut(.,
                      breaks = c(0, 5000, 10000, 20000, 50000, 100000, 500000,
                                 800000), # max(., na.rm = TRUE)
                      right = FALSE,
                      include.lowest = TRUE,
                      dig.lab = 10),
                .names = "f.tam_{col}"
  ))

Etiquetas <- c("Menos de 5.000","5.000 - <10.000","10.000 - <20.000",
               "20.000 - <50.000", "50.000 - <100.000", "100.000 - <500.000",
               "500000 o más")

campos_poblacion <- paste0("f.tam_",campos_poblacion)

datos_poblacion_2018_2022 <- datos_poblacion_2018_2022 %>%
  mutate(across(all_of(campos_poblacion), 
                ~ factor(., labels = Etiquetas),
                .names = "{col}"
  )) %>% 
  select(CODIGO_INE3, all_of(campos_poblacion))

datos <-  left_join(datos, 
                    datos_poblacion_2018_2022, 
                    by = c("codigo_ine" = "CODIGO_INE3") )

