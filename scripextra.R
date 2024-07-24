library(devtools)
#crear el libro de quarto
pkgdown::build_site()


#path_raw_data <- "estudio_entomologico1.txt"
#cargar un dataframe con la funsion load_raw_data

dfsm <- load_raw_data("inst/extdata/estudio_entomologico1.txt")
dfsm_lew <- load_raw_data("inst/extdata/estudio_entomologico1.txt",
    col_name = labels_sub
                      )
dfsm_full <- load_raw_data("inst/extdata/estudio_entomologico1.txt",
                           col_name = labels_full
                      )

usethis::use_data_raw(internal = TRUE)
#guardar el dfsm para el uso del paquete
usethis::use_data(dfsm)
usethis::use_data(dfsm_lew,  overwrite = TRUE)
usethis::use_data(dfsm_full)

devtools::document()
devtools::load_all()
system.file("extdata", package = "rStegomyia") |> list.files()


path_raw_data <- system.file("extdata",
                             "estudio_entomologico1.txt",
                             package = "rStegomyia"
                             )

path_raw_datacsv <- system.file("extdata",
                             "estudio_entomologico1.csv",
                             package = "rStegomyia"
)




#verificar que se guardo el path
system.file("extdata",
            "estudio_entomologico1.txt",
            package = "rStegomyia"
            )

labels_sub <- c(
    "Tipo de Estudio",
    "Semana Epidemiologica",
    "Casas Revisadas",
    "Casas Positivas",
    "Total de Recipientes con Agua",
    "Total de Recipientes Positivos")
usethis::use_data(labels_sub,  overwrite = TRUE)

labels <- c(
    "Tipo de Estudio",
    "Clave Jurisdiccion",
    "Jurisdiccion",
    "Clave Localidad",
    "Localidad",
    "Sector",
    "Fecha de Inicio",
    "Semana Epidemiologica",
    "Casas Revisadas",
    "Casas Positivas",
    "Total de Recipientes con Agua",
    "Total de Recipientes Positivos")
usethis::use_data(labels,  overwrite = TRUE)

labels_full <-c("Tipo de Estudio",
                "Jurisdiccion",
                "Municipio",
                "Localidad",
                "Sector",
                "Fecha de Inicio",
                "Semana Epidemiologica",
                "Casas Revisadas",
                "Casas Positivas",
                "Total de Recipientes con Agua",
                "Total de Recipientes Positivos",
                "Total de Recipientes Positivos a Pupas",
                "No. Total de Pupas en Recipientes",
                "Recipientes Tratables",
                "Recipientes Controlables",
                "Recipientes Eliminables"
)



clippy.xlsx


# crear una funcion para el paquete
use_r("get_stegomyia_indices_by_type_of_study_and_star_date")

# crear un test de la funcion tal
usethis::use_test("get_maps_stegomyia_indices")
usethis::use_test("get_stegomyia_indices_by_type_of_study_and_star_date")

#crear un vingette
usethis:vignette("clean_raw_data")


path_data <- "data-raw/qr.csv"
path_data_lew <- "data-raw/qr_lew.csv"
path_data_full <- "data-raw/qr_full.csv"

df_indx <- clean_raw_data(dfsm, path_out = path_data)
df_typ <- clean_raw_data(dfsm_full, path_out = path_data_full)

#get_stegomyia_indices_by_type_of_study_and_geo_is


usethis::use_data(df_typ)

sectors <-c(df_indx$Sector)

df_for_map <- get_stegomyia_indices_by_type_of_study_and_geo_is(df_indx,
                                                            var= c(sectors)
                                                            )
df_for_map

usethis::use_data(df_for_map, overwrite = TRUE)
#' use dataframe "dfsm" with change names of variable with function
#' "load_raw_data"
#'
#'
#' use dataframe "df_indx" with entomology information to calculation Stegomyia
#' indices change names of variable with functions "load_raw_data" and "clean_raw_data"
#'


shm1 <- st_read("data-raw/maps/ejercicio_sectores_hermosillo.shp")
shm1
usethis::use_data(shm1, overwrite = TRUE)
#ejercicio_sectores_hermosillo2.shp
plot(shm1)
#shm2=readShapePoly("data-raw/maps/ejercicio_sectores_hermosillo2.shp")
shm2 <- st_read("data-raw/maps/ejercicio_sectores_hermosillo2.shp")

shm2
plot(shm2)
usethis::use_data(df_for_map)

get_maps_stegomyia_indices(df_for_map)
