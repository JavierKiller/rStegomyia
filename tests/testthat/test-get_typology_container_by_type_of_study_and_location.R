# Make DataFrame
dftest <- data.frame(Localidad = factor("HERMOSILLO"),
                     Pct_Recipientes_Tratables = (9/45*100),
                     Pct_Recipientes_Controlables = (36/45*100),
                     Pct_Recipientes_Eliminables = (0/45*100)
)

test_that("calculation_of_typology_container_date_of_data.frame", {
  expected <- get_typology_container_by_type_of_study_and_location(df = df_typ,
                                                                   st = "Verificacion",
                                                                   var = "HERMOSILLO"
  )
  expect_s3_class(expected, "data.frame")
  expect_equal(expected, dftest, tolerance = 0.01, ignore_attr = TRUE)

}
)
test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
  expect_warning(get_typology_container_by_type_of_study_and_location(df = df_typ,
                                                                      st = "Verificacion",
                                                                      var = "NAVOJOA"
  ),
  "Casa_Revisada with 0")
}
)
