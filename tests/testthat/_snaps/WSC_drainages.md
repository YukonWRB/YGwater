# drainages works with no limit_stns

    Code
      WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(
        tempdir(), "/test"), limit_stns = NULL)
    Output
      [1] "The polygons shapefile has been sav
      [1] "The points shapefile has been saved

# drainages works with limit_stns

    Code
      WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(
        tempdir(), "/test"))
    Output
      [1] "The polygons shapefile has been sav
      [1] "The points shapefile has been saved

