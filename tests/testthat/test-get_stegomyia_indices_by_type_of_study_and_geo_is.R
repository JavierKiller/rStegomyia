# Make DataFrame
dftest <- data.frame(Sector = factor(401),
                     HI = (3/66*100),
                     CI = (3/156*100),
                     BI = (3/66*100),
                     index_status_HI =factor("Alarma"),
                     index_status_CI = factor("Bueno"),
                     index_status_BI =  factor("Alarma")
)
dftest0 <- data.frame(Sector = factor(390),
                      HI = 0,
                      CI = 0,
                      BI = 0,
                      index_status_HI =factor("Optimo"),
                      index_status_CI = factor("Optimo"),
                      index_status_BI =  factor("Optimo")
)

path_ttest <- test_path("extdata", "statusindicesector.csv")

test_that("calculation_of_stegomyia_indices_by_type_of_study_and_geo_is_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_geo_is(df = df_indx,
                                                                st = "Verificacion",
                                                                var = "401",
                                                                path_out = path_ttest
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)
}
)
test_that("calculation_of_stegomyia_indices_0_by_type_of_study_and_geo_is_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_geo_is(df = df_indx,
                                                                st = "Verificacion",
                                                                var = "390",
                                                                path_out = path_ttest
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
}
)
test_that("Error_in_calculation_of_stegomyia_indices_by_type_of_study_and_geo_is_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_geo_is(df = df_indx,
                                                                 st = "Verificacion",
                                                                 var = "001",
                                                                 path_out = path_ttest
  ),
  "These filters don't have data in this data.frame")
}
)
test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_and_geo_is(df = df_indx,
                                                                   st = "Verificacion",
                                                                   var = "928",
                                                                   path_out = path_ttest
  ),
  message = "Casa_Revisada with 0")
}
)
