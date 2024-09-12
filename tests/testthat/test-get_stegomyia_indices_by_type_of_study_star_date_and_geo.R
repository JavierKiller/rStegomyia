# Make DataFrame
dftest <- data.frame(Sector = factor(540),
                     HI = (9/75*100),
                     CI = (9/168*100),
                     BI = (9/75*100),
                     index_status_HI =factor("Emergencia"),
                     index_status_CI = factor("Emergencia"),
                     index_status_BI =  factor("Emergencia")
                     )
dftest0 <- data.frame(Sector = factor(390),
                      HI = 0,
                      CI = 0,
                      BI = 0,
                      index_status_HI =factor("Optimo"),
                      index_status_CI = factor("Optimo"),
                      index_status_BI =  factor("Optimo")
                      )
test_that("calculation_of_stegomyia_indices_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df_indx,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/06",
                                                                       var = "540"
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest, tolerance = 0.01)
}
)
test_that("calculation_0_of_stegomyia_indices_of_data.frame", {
  expected <- get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df_indx,
                                                                       st = "Verificacion",
                                                                       date = "2021/01/08",
                                                                       var = 390
  )
  expect_s3_class(expected, "data.frame")
  expect_equivalent(expected, dftest0, tolerance = 0.01)
}
)
test_that("Error_in_calculation_of_stegomyia_indices_of_data.frame",
          {
            expect_error(
              get_stegomyia_indices_by_type_of_study_star_date_and_geo(
                df = df_indx,
                st = "Verificacion",
                date = "2021/01/08",
                var = "000"
              ),
              "These filters don't have data in this data.frame"
            )
          })

test_that("Warrning_in_calculation_of_stegomyia_indices_of_0_Casa_Revisada", {
  expect_warning(get_stegomyia_indices_by_type_of_study_star_date_and_geo(df = df_indx,
                                                                          st = "Verificacion",
                                                                          date = "2021/01/12",
                                                                          var = "928"
  ),
  "Casa_Revisada with 0")
}
)
