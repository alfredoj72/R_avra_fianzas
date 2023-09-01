
bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}


tema_ams <- theme_bw() + theme(text = element_text(family = "Asap-Bold", color = "#25636e"), 
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), 
                               plot.caption=element_text(hjust=1,size=9,colour="grey30"),
                               plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
                               plot.title=element_text(size=12,face="bold", color = "red"),
                               axis.text.x = element_text(family = "Asap-Bold", color = "grey40"),
                               axis.text.y = element_text(family = "Asap-Bold", color = "grey40"), 
                               #legend.position = "none" # Removemos la leyenda. 
)


theme_gppr <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_minimal() +    #replace elements we want to change
    
    theme(
      
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}





kk <- # Gráfico de barras de las proporciones con etiquetas de altura
  ggplot(datos, aes(x = nacionalidad_arrendatario)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "cornsilk1", color = "cornsilk2") +
  geom_text(stat = "count", aes(y = (..count..)/sum(..count..), 
                                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            vjust = +0.5, size = 3, color = "cornsilk4") +
  labs(x = "",
       y = "Frecuencia", 
       title = "Nacionalidad Arrendatario") +
  #scale_x_discrete(labels = c(levels(datos$nacionalidad_arrendatario), "No especificado"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Configurar etiquetas en porcentaje
  theme_minimal()



kk +bbc_style()
kk + tema_ams
kk + theme_gppr()

# paqutes colourpicker, ggThemeAssist, esquisse


# Obtener todos los valores por defecto de los parámetros de temas
valores_por_defecto <- theme_get()

# Imprimir todos los valores por defecto
print(valores_por_defecto)







ruta_al_zip <- "C:/Users/alfredoj.martin/Downloads/rmarkdown_2.24.zip"
install.packages(ruta_al_zip, repos = NULL, type = "win.binary")















# tablas 
init_flextable_defaults() #reinicia los valores por defecto del paq flextable
set_flextable_defaults(
  big.mark = ".",
  decimal.mark = ",",
  digits = 1,
  # theme_fun = theme_box,
  font.family = "calibri",  
  line_spacing = 1.3,
  font.color = "grey8", # "#333333",
  font.size = 11,
  border.color = "grey60", # "#333333",
  padding = 4
)

# define el estilo del borde
border_style1 = officer::fp_border(color="grey60", width=2)
border_style2 = officer::fp_border(color="grey60", width=0.5)