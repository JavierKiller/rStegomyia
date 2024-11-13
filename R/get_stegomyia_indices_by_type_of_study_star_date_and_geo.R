#' Get stegomyia indices by type of study, date and geo
#'
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type, date and geographic variable using the
#' following formulas:
#'       - Container Index (CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#' @param df The dataframe with entomology information processed by the function
#' "clean_raw_data".
#' @param st a string of type of study selected, "Encuesta" or "Verificacion". By
#'  default, it is set to "Verificacion".
#' @param var receive the "Sector" number, which is the geographic variable 
#' used to calculate Stegomyia indexes.
#' @param date a string with the date used to calculate the stegomyia indices. By default,
#' it is set to "2021/01/07".
#'
#' @return The dataframe with stegomyia indices of the selection of type of study date,
#' and geographic variable of sector.
#'
#' @examples
#'
#' df_dg <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df_indx,
#' st = "Encuesta",
#' date = "2021/01/06",
#' var = "390")
#'
#' df_dg
#'

get_stegomyia_indices_by_type_of_study_star_date_and_geo <- function(
    df,
    st = "Verificacion",
    date = "2021/01/07",
    var
){
  df$Fecha_de_Inicio <- as.Date(df$Fecha_de_Inicio, format = "%d/%m/%Y")

  filtered_df <- df %>%
    filter(Tipo_de_Estudio == st,
           Fecha_de_Inicio == as.Date(date),
           Sector %in% var)

  if (nrow(filtered_df) == 0) {
    stop("These filters don't have data in this data.frame")
  }

  if (any(filtered_df$Casas_Revisadas == 0)) {
    warning("Casa_Revisada with 0")
    filtered_df <- filtered_df %>%
      filter(Casas_Revisadas != 0)
  }

  dfti <- filtered_df %>%
    dplyr::select(Sector,
                  Casas_Revisadas,
                  Casas_Positivas,
                  Total_de_Recipientes_con_Agua,
                  Total_de_Recipientes_Positivos) %>%
    group_by(Sector) %>%
    summarize(
      HI = sum(Casas_Positivas) / sum(Casas_Revisadas) * 100,
      CI = if (sum(Total_de_Recipientes_Positivos) > 0) {
        sum(Total_de_Recipientes_Positivos) / sum(Total_de_Recipientes_con_Agua) * 100
      } else {0},
      BI = sum(Total_de_Recipientes_Positivos) / sum(Casas_Revisadas) * 100
    ) %>%
    mutate(
      index_status_HI = case_when(
        HI == 0 ~ "Optimo",
        HI > 0 & HI <= 1 ~ "Optimo",
        HI > 1 & HI <= 3 ~ "Bueno",
        HI > 3 & HI <= 5 ~ "Alarma",
        is.na(HI) ~ "n",
        TRUE ~ "Emergencia"
      ),
      index_status_CI = case_when(
        CI == 0 ~ "Optimo",
        CI > 0 & CI <= 1 ~ "Optimo",
        CI > 1 & CI <= 3 ~ "Bueno",
        CI > 3 & CI <= 5 ~ "Alarma",
        is.na(CI) ~ "n",
        TRUE ~ "Emergencia"
      ),
      index_status_BI = case_when(
        BI == 0 ~ "Optimo",
        BI > 0 & BI <= 1 ~ "Optimo",
        BI > 1 & BI <= 3 ~ "Bueno",
        BI > 3 & BI <= 5 ~ "Alarma",
        is.na(BI) ~ "n",
        TRUE ~ "Emergencia"
      )
    ) %>%
    ungroup()

  risk_levels <- c("Optimo", "Bueno", "Alarma", "Emergencia")

  dfti <- dfti %>%
    mutate(index_status_HI = factor(index_status_HI, levels = risk_levels)) %>%
    mutate(index_status_CI = factor(index_status_CI, levels = risk_levels)) %>%
    mutate(index_status_BI = factor(index_status_BI, levels = risk_levels))

  return(as.data.frame(dfti))
}
