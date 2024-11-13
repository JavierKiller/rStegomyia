# Make DataFrame
dftest <- data.frame(
  Sector = factor(c(500, 513, 914, 915, 919, 1305)),
  HI = c((15/147*100), (0/36*100), (0/36*100), (15/150*100), (15/147*100), (15/150*100)),
  CI = c((15/468*100), (0/123*100), (0/123*100), (12/390*100), (15/468*100), (12/390*100)),
  BI = c((15/147*100), (0/36*100), (0/36*100), (12/150*100), (15/147*100), (12/150*100)),
  index_status_HI = factor(c("Emergencia", "Optimo", "Optimo", "Emergencia", "Emergencia", "Emergencia")),
  index_status_CI = factor(c("Alarma", "Optimo", "Optimo", "Alarma", "Alarma", "Alarma")),
  index_status_BI = factor(c("Emergencia", "Optimo", "Optimo", "Emergencia", "Emergencia", "Emergencia"))
)
dftest0 <- data.frame(Sector = factor(1248),
                      HI = 0,
                      CI = 0,
                      BI = 0,
                      index_status_HI =factor("Optimo"),
                      index_status_CI = factor("Optimo"),
                      index_status_BI =  factor("Optimo"))

test_that("calculation_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_ew(df_indx,
                                                                   st = "Encuesta",
                                                                   ew = 2
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)

}
)
test_that("calculation_0_of_stegomyia_indices_date_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_ew(df_indx,
                                                                   st = "Verificacion",
                                                                   ew = 3
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
}
)
test_that("Error_in_calculation_of_stegomyia_indices_date_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_ew(df_indx,
                                                                    st = "Verificacion",
                                                                    ew = 5
  ),
  "These filters don´t have data in this data.frame")
}
)
test_that("warning_in_calculation_of_stegomyia_indices_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_and_ew(df_indx,
                                                                      st = "Verificacion",
                                                                      ew = 3
  ),
  "Casa_Revisada with 0")
}
)
