dfempty <- data.frame(Sector = factor(),
                      HI = numeric(),
                      CI = numeric(),
                      BI = numeric(),
                      index_status_HI = numeric(),
                      index_status_CI = numeric(),
                      index_status_BI = numeric())

test_that("load dataframe to get_maps_stegomyia_indices", {
  expect_s3_class(df_for_map, "data.frame")
}
)
test_that("all objects in the list are ggplot", {
  expected_list <- get_maps_stegomyia_indices(df = df_for_map)
  expect_type(expected_list, "list")
  for (plot in expected_list) {
    expect_s3_class(plot, "gg")
  }
})

test_that(
  "error_get_maps_stegomyia_indices",
  {
    expect_error(
      get_maps_stegomyia_indices(df = df_indx),
      "dataframe o path is incorrect"
    )
     expect_error(
       get_maps_stegomyia_indices(df = dfempty),
       "dataframe is empty"
     )
  }
)
