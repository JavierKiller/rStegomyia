# Make DataFrame
dftest <- data.frame(Localidad = factor("HERMOSILLO"),
                     HI = (42/1356*100),
                     CI = (45/3255*100),
                     BI = (45/1356*100),
                     index_status_HI =factor("Alarma"),
                     index_status_CI = factor("Bueno"),
                     index_status_BI =  factor("Alarma")
)
dftest0 <- data.frame(Localidad = factor("HERMOSILLO"),
                      HI = 0,
                      CI = 0,
                      BI = 0,
                      index_status_HI =factor("Optimo"),
                      index_status_CI = factor("Optimo"),
                      index_status_BI =  factor("Optimo")
)

path_ttest <- test_path("extdata", "statusindicesloc.csv")

test_that("calculation_of_stegomyia_indices_by_type_of_study_and_geo_is_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_and_loc(df = df_indx,
                                                                st = "Verificacion",
                                                                var = "HERMOSILLO",
                                                                path_out = path_ttest
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)
}
)
# test_that("calculation_of_stegomyia_indices_0_by_type_of_study_and_geo_is_of_data.frame", {
#   expected <- get_stegomyia_indices_by_type_of_study_and_loc(df = df_indx,
#                                                                 st = "Verificacion",
#                                                                 var = "HERMOSILLO",
#                                                                 path_out = path_ttest
#   )
#   expect_s3_class(expected, "data.frame")
#   expect_equivalent(expected, dftest0, tolerance = 0.01)
# }
# )
test_that("Error_in_calculation_of_stegomyia_indices_by_type_of_study_and_geo_is_of_data.frame", {
  expect_error(get_stegomyia_indices_by_type_of_study_and_loc(df = df_indx,
                                                                 st = "Verificacion",
                                                                 var = "ATLANTIS",
                                                                 path_out = path_ttest
  ),
  "These filters don't have data in this data.frame")
}
)
test_that("Error_in_calculation_of_typology_container_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_and_loc(df = df_indx,
                                                                   st = "Verificacion",
                                                                   var = "CIUDAD_OBREGÓN",
                                                                   path_out = path_ttest
  ),
  message = "Casa_Revisada with 0")
}
)
