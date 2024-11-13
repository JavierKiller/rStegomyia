#' get_1map_stegomyia_indices
#'
#'
#' This function generates maps for three Stegomyia indices (Entomological Risk
#' Indices) of dataframe make to funtion
#' get_stegomyia_indices_by_type_of_study_and_geo_is.
#' This data frame containing information about sectors and their corresponding
#' indices.
#'
#' @param df the dataframe with information processed by the function
#' "get_stegomyia_indices_by_type_of_study_and_geo_is" or
#' "get_stegomyia_indices_by_type_of_study_and_star_date".
#' @param m1 is a shapefile"ejercicio_sectores_hermosillo.shp" with geographic
#'  information of hermosillo city for join dataframe with information of
#'  indices stegomyia
#' @param m0 is a shapefile "ejercicio_sectores_hermosillo2.shp" with limit of
#' sectors of hermosillo city. This shapefile use to fution with m1 and give
#' limits of the sectors and the hermosillo city
#'
#'   print(maps$HI)  # Print the map for the House Index, the Container Index 
#'   or the Breteau Index
#'   
#'   
#' @return one map representing the different Stegomyia indices.
#'
#' @examples
#'
#' Sindx_map <- get_1map_stegomyia_indices(df_for_map)
#'
#' Sindx_map
#'

get_1map_stegomyia_indices <- function(
    df,
    m1 = shm1,
    m0 = shm2
){
  names_df <- names(df)
  condition_names_index_error <-  "index_status_HI" %in% names_df
  if (isFALSE(condition_names_index_error)) {
    stop("dataframe o path is incorrect")
  }
  condicion_nrows <- nrow(df)>0
  if(isFALSE(condicion_nrows)){
    stop("dataframe is empty")
  }
  
  w2 <- st_transform(m1,
                     crs =4326)
  #rename of variable
  w2 <- w2 %>%
    rename(Sector = SECCION)%>%
    mutate(Sector = as.factor(Sector))
  #join to data table
  w2_ <- w2 %>%
    left_join(df,
                         by = "Sector")
  w2_ <- w2_ %>% drop_na()
  
  w2_ <- st_transform(w2_,
                     4326)
  
  # Niveles
  levels=c("Optimo",
           "Bueno",
           "Alarma",
           "Emergencia")
  
  colores_id <- colorFactor(
    palette = c("blue", "green", "yellow", "red"),
    levels = c("Optimo", "Bueno", "Alarma", "Emergencia"),
    na.color = alpha("#e8e6e6", 0)
  )
  
  # Preparamos el texto para popup 
  popup <- paste0(
    "<b>", "Sector: ", "</b>", as.character(w2_$Sector), "<br>",  
    "<b>", "HI: ", "</b>", round(as.numeric(w2_$HI), 1), "%", "<br>",  
    "<b>", "index_status_HI: ", "</b>", as.character(w2_$index_status_HI), "<br>",  
    "<b>", "CI: ", "</b>", round(as.numeric(w2_$CI), 1), "%", "<br>",  
    "<b>", "index_status_CI: ", "</b>", as.character(w2_$index_status_CI), "<br>",  
    "<b>", "BI: ", "</b>", round(as.numeric(w2_$BI), 1), "%", "<br>",  
    "<b>", "index_status_BI: ", "</b>", as.character(w2_$index_status_BI), "<br>"
  ) %>% lapply(htmltools::HTML)  # Se aplica formato HTML
  
  m0 <- st_transform(m0,
                     crs = 4326
                     )
  
  map_stegomyia_indices <- leaflet(w2_) %>% 
    addPolygons(data= m0, # Se carga capa de referencia municipal
                stroke= TRUE,
                weight=0.2,                   
                opacity=1,
                fillColor = "transparent", #En este caso es la base de la división municipal 
                color= "black",
                fillOpacity = 0,
                smoothFactor = 1,
                group= "Sector") %>%
    addProviderTiles(providers$CartoDB.Voyager) %>% # Elegimos fondo del mapa
    addPolygons(data= w2_, #Carga de capa
                stroke= TRUE, # Si se dibujan los bordes o no
                weight=1,  # Peso de la línea de los bordes                 
                opacity=1, #Opacidad de la línea de borde
                color= "black", # Color de la línea de borde
                fillColor = ~colores_id(w2_$index_status_HI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
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
    addPolygons(data= w2_, #Carga de capa
                stroke= TRUE, # Si se dibujan los bordes o no
                weight=1,  # Peso de la línea de los bordes                 
                opacity=1, #Opacidad de la línea de borde
                color= "black", # Color de la línea de borde
                fillColor = ~colores_id(w2_$index_status_CI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
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
    addPolygons(data= w2_, #Carga de capa
                stroke= TRUE, # Si se dibujan los bordes o no
                weight=1,  # Peso de la línea de los bordes                 
                opacity=1, #Opacidad de la línea de borde
                color= "black", # Color de la línea de borde
                fillColor = ~colores_id(w2_$index_status_BI), # Color de relleno de los polígonos, se hace uso de la paleta de colores
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
    addLegend(position = "bottomleft",  pal = colores_id, values = ~levels, opacity=1, group= "ESTADO DE RIESGO ENTOMOLOGICO", # Leyenda de referencia
              title = "Estatus de los Indices de Stegomyia", na.label = "No aplica") %>%
    addLayersControl(
      baseGroups = c("index_status_HI", "index_status_CI", "index_status_BI"),
      options = layersControlOptions(collapsed = FALSE, position = "bottomleft"))
  
  if (!dir.exists("visualization")) {
    dir.create("visualization")
  }
  

   
  saveWidget(map_stegomyia_indices,"visualization/Indices_Stegomyia.html", title= "Sonora: Indies Estegomia", selfcontained = T, libdir = "lib")
  
  
  return(map_stegomyia_indices)
  
}
