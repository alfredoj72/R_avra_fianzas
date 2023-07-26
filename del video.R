# Es todo de creación de mapas

library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(tmap)
library(leaflet)
library(sp)

# explore Europe data (polygon)
data(Europe)
summary(Europe)

str(Europe, max.level = 2)

head(Europe@data)

# display population density with default (pretty) classification
tm_shape(Europe) +
  tm_fill("pop_est_dens", id="name", title="Population Density") +
  tm_borders("grey20") + tm_text("iso_a3", size = 0.5) + tm_layout(legend.bg.color = "white")

# display population density with quantile classification
tm_shape(Europe) +
  tm_fill("pop_est_dens", id="name", title="Population Density", style = "quantile") +
  tm_borders("grey20") + tm_text("iso_a3", size = 0.5) + tm_layout(legend.bg.color = "white")

# display categocial economy
tm_shape(Europe) +
  tm_fill("economy", id="name", title="Economy Category") +
  tm_borders("grey20") + tm_text("iso_a3", size = 0.5) + tm_layout(legend.bg.color = "white")

# display line data
data(rivers)
summary(rivers)

tm_shape(Europe) +
  tm_fill() + tm_borders("grey20") +
  tm_shape(rivers) + tm_lines(col = "dodgerblue3") +
  tm_add_legend(type = "line", col = "dodgerblue3", labels = "Rivers", title = "Europe River Map") +
  tm_layout(legend.bg.color = "white")

data(metro)
summary(metro)

tm_shape(Europe) +
  tm_polygons("pop_est_dens", id="name", title="Population Density", style = "quantile") +
  tm_shape(metro) +  tm_bubbles("pop2030", col = "blue", title.size = "City Population 2030") +
  tm_text("name", size = 0.5, legend.size.show = FALSE, root = 8, size.lowerbound = .7, auto.placement = TRUE) +
  tm_layout(legend.bg.color = "white")

# transform projection of the europe sp to wgs84
proj4string(metro)

proj4string(Europe)

europe_wgs84 <- spTransform(Europe, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

europe_wgs84 <- st_transform(Europe, '+proj=longlat +datum=WGS84')
# show all ColorBrewer ramps
display.brewer.all()

# create a color palette for population density
pal_pop_dens <- colorQuantile("viridis", europe_wgs84$pop_est_dens, n = 7)











# sort the metro data descending by pop2030 to display small circles on top of large ones
metro_sorted <- metro[order(-metro$pop2030), ]

# use earth quake point data as example for clustered markers
str(quakes)


quakes_outline <- quakes[chull(quakes$long, quakes$lat), ]

leaflet(width = "100%", height = "800px") %>%
  # base layers
  addTiles(group = "OpenStreeMap.Default") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OpenStreetMap.BlackAndWhite") %>%
  addProviderTiles("OpenStreetMap.DE", group = "OpenStreetMap.DE") %>%
  addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap.HOT") %>%
  addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
  addProviderTiles("Thunderforest.Transport", group = "Thunderforest.Transport") %>%
  addProviderTiles("OpenMapSurfer.Roads", group = "OpenMapSurfer.Roads") %>%
  addProviderTiles("OpenMapSurfer.Grayscale", group = "OpenMapSurfer.Grayscale") %>%
  addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
  addProviderTiles("Stamen.TonerBackground", group = "Stamen.TonerBackground") %>%
  addProviderTiles("Stamen.TonerLite", group = "Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TerrainBackground", group = "Stamen.TerrainBackground") %>% 
  addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "Esri.WorldGrayCanvas") %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
  addProviderTiles("CartoDB.PositronNoLabels", group = "CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB.DarkMatter") %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB.DarkMatterNoLabels") %>%
  # data layers
  # countries of Europe with population density
  addPolygons(data = europe_wgs84, weight = 1, color = "black", smoothFactor = 0.3,
              fillOpacity = 0.8, fillColor = ~pal_pop_dens(pop_est_dens),
              label = ~paste0(name, ": ", formatC(pop_est_dens, big.mark = "'")),
              group = "Population Density (Europe)") %>%
  addLegend(pal = pal_pop_dens, values = europe_wgs84$pop_est_dens, opacity = 0.8,
            labFormat = labelFormat(big.mark = "'"),
            title = "Population Density") %>%
  # largest cities in the World with estimated population in 2030
  addCircles(data = metro_sorted, radius = ~sqrt(pop2030)*50, stroke = FALSE, col = "red",
             label = ~paste0(name, ": ", formatC(pop2030/1000000, big.mark = "'"), " Mio."),
             group = "Cities (Population 2030)") %>%
  addLegend(labels = "Pop. Est. 2030", color = "red", title = "City Population 2030") %>%
  # earth quake data with magnitude
  addMarkers(data = quakes, lng = ~long, lat = ~lat,
             popup = ~paste0("Magnitude: ", mag),
             clusterOptions = markerClusterOptions(),
             group = "Earthquakes") %>%
  addPolygons(data = quakes_outline, lng = ~long, lat = ~lat, fill = FALSE,
              weight = 3, color = "#FFFFCC", group = "Earthquakes Outline") %>%
  # layers control
  addLayersControl(
    baseGroups = c("OpenStreeMap.Default", "OpenStreetMap.BlackAndWhite", "OpenStreetMap.DE",
                   "OpenStreetMap.HOT", "OpenTopoMap", "Thunderforest.Transport", "OpenMapSurfer.Roads",
                   "OpenMapSurfer.Grayscale", "Stamen.Toner", "Stamen.TonerBackground",
                   "Stamen.TonerLite", "Stamen.TerrainBackground", "Esri.WorldStreetMap",
                   "Esri.WorldTopoMap", "Esri.WorldImagery", "CartoDB.Positron",
                   "CartoDB.PositronNoLabels", "CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels"),
    overlayGroups = c("Population Density (Europe)", "Cities (Population 2030)",
                      "Earthquakes", "Earthquakes Outline"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE)
  )
