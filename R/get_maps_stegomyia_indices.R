#' get_maps_stegomyia_indices
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
#'   print(maps$HI)  # Print the map for the House Index
#'   print(maps$CI)  # Print the map for the Container Index
#'   print(maps$BI)  # Print the map for the Breteau Index

#' @return listmaps: A list of three ggplot objects representing the maps for
#' different Stegomyia indices.
#'
#' @examples
#'
#' Sindx_maps <- get_maps_stegomyia_indices(df_for_map)
#'
#' Sindx_maps
#'

get_maps_stegomyia_indices <- function(
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
                     4326)
  #rename of variable
  w2 <- w2 %>%
    rename(Sector = SECCION)%>%
    mutate(Sector = as.factor(Sector))
  #join to data table
  w2_ <- w2 %>%
    right_join(df,
               by = "Sector")
  w2_ <- w2_ %>% drop_na()
  
  w2_ <- st_transform(w2_,
                      4326)

  colores_id <- c(
    "Optimo" = "blue",
    "Bueno" = "green",
    "Alarma" = "yellow",
    "Emergencia" = "red"
  )

  p_HI <- ggplot() +
    geom_sf(data = m0,
            fill = NA,
            size = 0.5) +
    geom_sf(data = w2_,
            aes(fill = index_status_HI),
            color = "black",
            size = 0.5) +
    coord_sf() +
    labs(title = "Stegomyia House Index in Hermosillo") +
    scale_fill_manual(values = colores_id)

  p_CI <- ggplot() +
    geom_sf(data = m0,
            fill = NA,
            size = 0.5) +
    geom_sf(data = w2_,
            aes(fill = index_status_CI),
            color = "black",
            size = 0.5) +
    coord_sf() +
    labs(title = "Stegomyia Container Index in Hermosillo") +
    scale_fill_manual(values = colores_id)


  p_BI <- ggplot() +
    geom_sf(data = m0,
            fill = NA,
            size = 0.5) +
    geom_sf(data = w2_,
            aes(fill = index_status_BI),
            color = "black",
            size = 0.5) +
    coord_sf() +
    labs(title = "Stegomyia Breteau Index in Hermosillo") +
    scale_fill_manual(values = colores_id)


  listmaps<-list(p_HI, p_CI, p_BI)

  ggsave("p_HI.png", path = "visualization")
  ggsave("p_CI.png", path = "visualization")
  ggsave("p_BI.png", path = "visualization")

  return(listmaps)

}
