path_data <- test_path("extdata", "qr.csv")
path_data_lew <- test_path("extdata", "qr_lew.csv")
path_data_full <- test_path("extdata", "qr_full.csv")

colt= list(
  Tipo_de_Estudio = col_factor(c("Encuesta", "Verificacion")),
  Clave_Jurisdiccion = "f",
  Jurisdiccion = "f",
  Clave_Localidad = "f",
  Localidad = "f",
  Sector = "f",
  Fecha_de_Inicio = col_date(format = "%d/%m/%Y"),
  Semana_Epidemiologica = "f",
  Casas_Revisadas = "d",
  Casas_Positivas = "d",
  Total_de_Recipientes_con_Agua = "d",
  Total_de_Recipientes_Positivos = "d"
)
test_that("assess_clean_raw_data", {
  expected <- clean_raw_data(dfsm, path_out = path_data)
  expect_identical(str(colt), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(42, 12))
}
)
test_that("assess_clean_raw_data_sub", {
  expected <- clean_raw_data(dfsm_lew, path_out = path_data_lew)
  expect_identical(str(colt), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(42, 6))
}
)
test_that("assess_clean_raw_data_extra", {
  expected <- clean_raw_data(dfsm_full, path_out = path_data_full)
  expect_identical(str(colt), str(expected))
  expect_s3_class(expected, "data.frame")
  expect_equal(dim(expected), c(42, 19))
}
)
