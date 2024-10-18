
test_that("assess_load_raw_data",
  {
    expected <- load_raw_data(
      test_path(
        "extdata",
        "estudio_entomologico1.txt"
      )
    )
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(42, 12))
    expect_named(expected, labels)
          }
)
test_that(
  "assess_load_raw_data_labels_sub",
  {
    col_select_sub <- c("Tipo de Estudio",
                        "Semana Epidemiologica",
                        "Casas Revisadas",
                        "Casas Positivas",
                        "Total de Recipientes con Agua",
                        "Total de Recipientes Positivos")
    expected <- load_raw_data(test_path(
      "extdata",
      "estudio_entomologico1.txt"
    ), col_name = col_select_sub)
    expect_s3_class(expected, "data.frame")
    expect_equal(dim(expected), c(42, 6))
    expect_named(expected, labels_sub)
  }
)

test_that(
  "assess_load_raw_data_error_Tipo_de_Estudio",
  {
    expect_error(
      load_raw_data(test_path(
        "extdata",
        "estudio_entomologico_equivocado.txt"
      )
      ),
      "data file .txt dont have Tipo de Estudio o path is incorrect"
    )
    expect_error(
      load_raw_data(test_path(
        "extdata",
        "estudio_entomologico_sin_datos.txt"
      )
      ),
      "data file .txt is empty"
    )
  }
)
