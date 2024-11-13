#' Get stegomyia indices by type of study and star date
#'
#'
#' The Stegomyia indices are calculated for each sampling. Once the data have
#' been loaded with function "load_rwa_data" and changed type data of function
#' "clean_raw_data", select study type and date variable using the following
#'  formulas:
#'       - Container Index(CI): (number of infected containers/total number of
#'       containers) * 100.
#'       - House Index (HI): (number of infected houses /total number of
#'       houses) * 100.
#'       - Breteau Index (BI): (number of positive containers/number of houses
#'       explored) * 100.
#'
#' @param df the dataframe with information processed by the function
#' "clean_raw_data".
#' @param st a string of type of study selected, "Encuesta" or "Verificacion". By
#'  default, it is set to "Verificacion".
#' @param ew number of the epidemiological week used to calculate the stegomyia indices. By default,
#' it is set to "1".
#'
#' @return
#'
#' The dataframe with Stegomyia indices of the selection of type of study and
#' date.
#'
#' @examples
#'
#' df_stegomyia_sd <- get_stegomyia_indices_by_type_of_study_and_ew(df_indx,
#'                                                      st = "Verificacion",
#'   
#'                                                      ew = "2")
#' head(df_stegomyia_sd)
#'

get_stegomyia_indices_by_type_of_study_and_ew <- function(
    df,
    st = "Verificacion",
    ew = "1"
){
  #df$Fecha_de_Inicio <- as.Date(df$Fecha_de_Inicio, format = "%d/%m/%Y")

  dfd <- df %>%
    filter(Tipo_de_Estudio %in% st, Semana_Epidemiologica  %in% ew) #ymd(date))

  if (nrow(dfd) == 0){
    stop("These filters don´t have data in this data.frame")
  }

  if (any(dfd$Casas_Revisadas == 0)){
    warning("Casa_Revisada with 0")
    dfd <- dfd %>%
      filter(Casas_Revisadas > 0)
  }

  dfti <- dfd %>%
    dplyr::select(Sector,
                  Casas_Revisadas,
                  Casas_Positivas,
                  Total_de_Recipientes_con_Agua,
                  Total_de_Recipientes_Positivos) %>%
    group_by(Sector) %>%
    summarize(
      HI = sum(Casas_Positivas)/ sum(Casas_Revisadas)*100,
      CI = if(sum(Total_de_Recipientes_Positivos)>0){
        sum(Total_de_Recipientes_Positivos)/
          sum(Total_de_Recipientes_con_Agua)*100
      } else {0},
      BI = sum(Total_de_Recipientes_Positivos)/ sum(Casas_Revisadas)*100
    )%>%
    mutate(
      index_status_HI = case_when(
        HI == 0 ~ "Optimo",
        HI > 0 & HI <= 1 ~ "Optimo",
        HI > 1 & HI <= 3 ~ "Bueno",
        HI > 3 & HI <= 5 ~ "Alarma",
        is.na(HI) ~ "n",
        TRUE ~  "Emergencia"
      ),
      index_status_CI = case_when(
        CI == 0 ~ "Optimo",
        CI > 0 & CI <= 1 ~ "Optimo",
        CI > 1 & CI <= 3 ~ "Bueno",
        CI > 3 & CI <= 5 ~ "Alarma",
        is.na(CI) ~ "n",
        TRUE ~  "Emergencia"
      ),
      index_status_BI = case_when(
        BI == 0 ~ "Optimo",
        BI > 0 & BI <= 1 ~ "Optimo",
        BI > 1 & BI <= 3 ~ "Bueno",
        BI > 3 & BI <= 5 ~ "Alarma",
        is.na(BI) ~ "n",
        TRUE ~  "Emergencia"
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
