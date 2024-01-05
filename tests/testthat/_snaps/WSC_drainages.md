# drainages works with no limit_stns

    Code
      WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(
        tempdir(), "/test"), limit_stns = NULL)
    Message
      The polygons shapefile has been saved in
      The points shapefile has been saved in C

# drainages works with limit_stns

    Code
      WSC_drainages(inputs_folder = test_path("drainages_data"), save_path = paste0(
        tempdir(), "/test"))
    Message
      The polygons shapefile has been saved in
      The points shapefile has been saved in C

