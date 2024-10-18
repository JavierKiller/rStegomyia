#'  clean raw data from a entomology study
#'
#'
#' @description
#' The function creates a data.frame that contains data and changes type data. The
#' data is assumed to have been previously loaded using the function
#' "load_raw_data" from a .txt file associated with an entomology study
#' conducted on the platform "Vigilancia Entomológica y Control Integral del
#' Vector". This function aims to generate new data.frame suitable for
#' further analysis and manipulation. It also saves the data from the
#' data.frame to a .csv file
#'
#' @param df a data.frame with raw data processed by the function
#' "load_raw_data".
#' @param path_out a string with path for that cleaned data to save.
#' @param col_name selected variable names, as deemed necessary to analyze,
#' previously selected with the function "load_raw_data".
#'
#' @return
#'
#' Data.frame and .csv file with cleaned data and correct format in variables
#' selected to function "load_raw_data".
#'
#' @examples
#'
#' path_of_example = c(system.file("extdata",
#'                                 "qr.csv",
#'                                 package = "rStegomyia"
#'                                 )
#'                    )
#'
#'
#' df_clean <- clean_raw_data(dfsm,
#'               path_out = path_of_example
#'               )
#'
#' head(df_clean)
#'

clean_raw_data <- function(
    df,
    path_out = "data-raw/qr.csv",
    col_name = cols( #describir porque se le da este formato a las variables
      Tipo_de_Estudio = col_factor(levels = c(
        "Encuesta",
        "Verificacion")),
      Clave_Jurisdiccion = col_factor(levels = as.character(c(
        2601,
        2602,
        2603,
        2604,
        2605,
        2606))),
      Jurisdiccion = col_factor(levels = c(
        "Hermosillo",
        "Caborca",
        "Cananea",
        "Cajeme",
        "Navojoa",
        "San_Luis_Río_Colorado"
      )),
      Clave_Municipio = col_factor(levels = as.character(1:72)),
      Municipio = col_factor(),
      Clave_Localidad = col_factor(),
      Localidad = col_factor(),
      Sector = col_factor(levels = as.character(1:2000)),
      Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
      Semana_Epidemiologica = col_factor(levels = as.character(1:53)),
      Casas_Revisadas = col_double(),
      Casas_Positivas = col_double(),
      Total_de_Recipientes_con_Agua = col_double(),
      Total_de_Recipientes_Positivos = col_double()
    )
){
  write_csv(df, file = path_out)

  df_aux <- read_csv(
    path_out,
    col_types = col_name
  )
  df <- df_aux
  df<- as.data.frame(df)
  write_csv(df, file = path_out)
  return(df)
}
