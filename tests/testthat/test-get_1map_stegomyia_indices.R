dfempty <- data.frame(Sector = factor(),
                      HI = numeric(),
                      CI = numeric(),
                      BI = numeric(),
                      index_status_HI = numeric(),
                      index_status_CI = numeric(),
                      index_status_BI = numeric())

test_that("load dataframe to get_1map_stegomyia_indices", {
  expect_s3_class(df_for_map, "data.frame")
}
)
test_that("the object is a leaflet map", {
  expected <- get_1map_stegomyia_indices(df = df_for_map)
  expect_s3_class(expected, "leaflet")
})

test_that(
  "error_get_1map_stegomyia_indices",
  {
    expect_error(
      get_1map_stegomyia_indices(df = df_indx),
      "dataframe o path is incorrect"
    )
    expect_error(
      get_1map_stegomyia_indices(df = dfempty),
      "dataframe is empty"
    )
  }
)
