library(htmltools)
library(htmlwidgets)
#library(rcartocolor)
library(leaflet)
library(leaflet.extras)
library(rStegomyia)
library(sf)
library(tidyverse)

# Niveles
levels=c("Optimo",
         "Bueno",
         "Alarma",
         "Emergencia") # Definimos los niveles del GM


capa_ageb <- st_read("data-raw/maps/ejercicio_sectores_hermosillo.shp") 
capa_ageb <- st_transform(capa_ageb, crs = 4326)
capa_ageb <- capa_ageb %>%
  rename(Sector = SECCION)%>%
  mutate(Sector = as.factor(Sector))

#capa munisipios de inegi
# capa_mun <- st_read("data-raw/maps/26mun.shp")
# capa_mun <- st_transform(capa_mun, crs = 4326)

#capa_sectors
capa_sectors <- st_read("data-raw/maps/ejercicio_sectores_hermosillo2.shp")
capa_sectors <- st_transform(capa_sectors, crs = 4326)
capa_sectors <- capa_sectors %>%
  rename(Sector = SECCION)%>%
  mutate(Sector = as.factor(Sector))

## Agregamos la información del GM a la capa geográfica

capa_ageb <- capa_ageb %>%
  left_join(df_for_map,
             by = "Sector")
capa_ageb <- capa_ageb %>% drop_na()

colores_id <- colorFactor(
  palette = c("blue", "green", "yellow", "red"),
  levels = c("Optimo", "Bueno", "Alarma", "Emergencia"),
  na.color = alpha("#e8e6e6", 0)
)

# Preparamos el texto para popup 
popup <- paste0(
  "<b>", "Sector: ", "</b>", as.character(capa_ageb$Sector), "<br>",  
  "<b>", "HI: ", "</b>", round(as.numeric(capa_ageb$HI), 1), "%", "<br>",  
  "<b>", "index_status_HI: ", "</b>", as.character(capa_ageb$index_status_HI), "<br>",  
  "<b>", "CI: ", "</b>", round(as.numeric(capa_ageb$CI), 1), "%", "<br>",  
  "<b>", "index_status_CI: ", "</b>", as.character(capa_ageb$index_status_CI), "<br>",  
  "<b>", "BI: ", "</b>", round(as.numeric(capa_ageb$BI), 1), "%", "<br>",  
  "<b>", "index_status_BI: ", "</b>", as.character(capa_ageb$index_status_BI), "<br>"
) %>% lapply(htmltools::HTML)  # Se aplica formato HTML

map_stegomyia_indices <- leaflet(capa_ageb) %>% 
  addPolygons(data= capa_sectors, # Se carga capa de referencia municipal
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = "transparent", #En este caso es la base de la división municipal 
              color= "black",
              fillOpacity = 0,
              smoothFactor = 1,
              group= "Sector") %>%
  addProviderTiles(providers$CartoDB.Voyager) %>% # Elegimos fondo del mapa
  addPolygons(data= capa_ageb, #Carga de capa
              stroke= TRUE, # Si se dibujan los bordes o no
              weight=1,  # Peso de la línea de los bordes                 
              opacity=1, #Opacidad de la línea de borde
              color= "black", # Color de la línea de borde
              fillColor = ~colores_id(capa_ageb$index_status_HI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
              fillOpacity = 0.6, # Opacidad del relleno
              smoothFactor = 1, # Suaviza los bordes del polígono para un mejor rendimiento 
              highlightOptions = highlightOptions(color = "black", # Sobresaltar los polígonos donde pasa el mouse y las características
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popup, # Popup (en este caso es al dar click, para hover usamos label)
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, #opciones del popup
                                          style = list( # características del popup (css)
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "index_status_HI") %>% 
  # Nombre de la capa para referencia en menú
  addPolygons(data= capa_ageb, #Carga de capa
              stroke= TRUE, # Si se dibujan los bordes o no
              weight=1,  # Peso de la línea de los bordes                 
              opacity=1, #Opacidad de la línea de borde
              color= "black", # Color de la línea de borde
              fillColor = ~colores_id(capa_ageb$index_status_CI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
              fillOpacity = 0.6, # Opacidad del relleno
              smoothFactor = 1, # Suaviza los bordes del polígono para un mejor rendimiento 
              highlightOptions = highlightOptions(color = "black", # Sobresaltar los polígonos donde pasa el mouse y las características
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popup, # Popup (en este caso es al dar click, para hover usamos label)
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, #opciones del popup
                                          style = list( # características del popup (css)
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "index_status_CI") %>%
  # Nombre de la capa para referencia en menú
  addPolygons(data= capa_ageb, #Carga de capa
              stroke= TRUE, # Si se dibujan los bordes o no
              weight=1,  # Peso de la línea de los bordes                 
              opacity=1, #Opacidad de la línea de borde
              color= "black", # Color de la línea de borde
              fillColor = ~colores_id(capa_ageb$index_status_BI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
              fillOpacity = 0.6, # Opacidad del relleno
              smoothFactor = 1, # Suaviza los bordes del polígono para un mejor rendimiento 
              highlightOptions = highlightOptions(color = "black", # Sobresaltar los polígonos donde pasa el mouse y las características
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popup, # Popup (en este caso es al dar click, para hover usamos label)
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, #opciones del popup
                                          style = list( # características del popup (css)
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "index_status_BI") %>% 
  # Nombre de la capa para referencia en menú
  # addPolygons(data= capa_sectors, # Se carga capa de referencia municipal
  #             stroke= TRUE,
  #             weight=0.2,                   
  #             opacity=1,
  #             fillColor = "transparent", #En este caso es la base de la división municipal 
  #             color= "black",
  #             fillOpacity = 0,
  #             smoothFactor = 1,
  #             group= "Sector") %>%
  # addPolygons(data= capa_mun, # Se carga capa de referencia municipal
  #             stroke= TRUE,
  #             weight=0.2,                   
  #             opacity=1,
  #             fillColor = "transparent", #En este caso es la base de la división municipal 
  #             color= "black",
  #             fillOpacity = 0,
  #             smoothFactor = 1,
  #             group= "Municipal") %>%
  # A manera de ejemplo
  addLegend(position = "bottomleft",  pal = colores_id, values = ~levels, opacity=1, group= "ESTADO DE RIESGO ENTOMOLOGICO", # Leyenda de referencia
            title = "Estatus de los Indices de Stegomyia", na.label = "No aplica") %>%
  addLayersControl(
    baseGroups = c("index_status_HI", "index_status_CI", "index_status_BI"),
    options = layersControlOptions(collapsed = FALSE, position = "bottomleft"))
  
  
  # addLegend(position = "bottomleft", pal = colores_id, values = ~levels, opacity = 1,
  #           title = "Estatus de los Indices de Stegomyia", na.label = "No aplica") %>% 
  # addLayersControl(
  #   overlayGroups = c("index_status_HI", "index_status_CI", "index_status_BI"), 
  #   options = layersControlOptions(collapsed = FALSE, position = "bottomleft")
  # )

map_stegomyia_indices

saveWidget(mapaagebmarg,"Indices_Stegomyia.html", title= "Sonora: Indies Estegomia", selfcontained = T, libdir = "lib")
