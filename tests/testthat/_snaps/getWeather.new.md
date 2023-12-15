# weather is fetched and return looks as expected

    Code
      getWeather("53179", start = "2022-01-01", end = "2022-01-15", interval = "day")
    Condition
      Warning in `value[[3L]]()`:
      The local list of stations is outdated and automatically updating it failed. Please update it by running weathercan::stations_dl(), especially if there's an issue running this function.
    Output
      # A tibble: 15 x 37
         station_name   station_id station_operator prov    lat   lon  elev climate_id
         <chr>               <dbl> <lgl>            <chr> <dbl> <dbl> <dbl> <chr>     
       1 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       2 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       3 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       4 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       5 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       6 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       7 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       8 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
       9 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      10 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      11 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      12 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      13 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      14 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      15 BEAVER CREEK A      53179 NA               YT     62.4 -141.  650. 2100155   
      # i 29 more variables: WMO_id <chr>, TC_id <chr>, date <date>, year <chr>,
      #   month <chr>, day <chr>, qual <chr>, cool_deg_days <dbl>,
      #   cool_deg_days_flag <chr>, dir_max_gust <dbl>, dir_max_gust_flag <chr>,
      #   heat_deg_days <dbl>, heat_deg_days_flag <chr>, max_temp <dbl>,
      #   max_temp_flag <chr>, mean_temp <dbl>, mean_temp_flag <chr>, min_temp <dbl>,
      #   min_temp_flag <chr>, snow_grnd <dbl>, snow_grnd_flag <chr>,
      #   spd_max_gust <dbl>, spd_max_gust_flag <chr>, total_precip <dbl>, ...

