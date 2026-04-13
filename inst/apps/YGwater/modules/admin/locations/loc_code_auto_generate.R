# Module logic for auto-generating location codes and HYDAT integration

# UI element for auto-generate button
auto_generate_ui <- function(ns) {
  actionButton(
    ns("auto_generate"),
    "Auto-generate code (clear input)",
    width = "100%",
    style = "margin-top: 32px;"
  )
}

auto_generate_server <- function(input, session, ns, moduleData) {
  observeEvent(input$auto_generate, {
    # Check if latitude/longitude are provided
    if (
      is.na(input$lat) || is.na(input$lon) || input$lat == "" || input$lon == ""
    ) {
      showModal(modalDialog(
        title = "Error",
        "Please provide both latitude and longitude to auto-generate a location code.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }

    # Ensure that input$loc_type is already populated
    if (is.null(input$loc_type)) {
      showModal(modalDialog(
        title = "Error",
        "Please select a location type before auto-generating a location code.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    }

    # Search for the layer_name 'National Hydro Network - Basins' in spatial.vectors. If not found, prompt user to download NHN basins and load to database.
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "
      SELECT COUNT(*) AS count
      FROM spatial.vectors
      WHERE layer_name = 'National Hydro Network - Basins';
      "
    )[1, 1]

    if (check < 1338) {
      showModal(modalDialog(
        title = "NHN Basins Not Found",
        HTML(
          "The National Hydro Network - Basins layer is not found in the database or has fewer than the expected number of features.<br><br>
          Would you like to load the NHN drainage basins into the database now?<br><br>
          This may take several minutes depending on your internet connection."
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          bslib::input_task_button(
            ns("fetch_nhn"),
            "Download NHN Basins"
          )
        )
      ))
      return(NULL)
    }

    # See if there's a corresponding polygon. Do a spatial intersection with the lat/lon
    poly <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "
      WITH p AS (
        SELECT ST_Transform(ST_SetSRID(ST_MakePoint($1,$2), 4326), ST_SRID(geom)) AS pt
        FROM spatial.vectors
        WHERE layer_name = 'National Hydro Network - Basins'
        LIMIT 1
      )
      SELECT feature_name
      FROM spatial.vectors v, p
      WHERE v.layer_name = 'National Hydro Network - Basins'
        AND ST_Intersects(v.geom, p.pt)
      ORDER BY ST_Area(v.geom::geography) ASC
      LIMIT 1;
      ",
      params = list(input$lon, input$lat)
    )[1, 1]

    # Take the first 2 numbers and subsequent 2 or 3 letters from the polygon name
    code <- sub("^([0-9]{2})([A-Za-z]{2,3}).*$", "\\1\\2", poly)

    # Generate a location type suffix
    type_suffix <- moduleData$loc_types[
      moduleData$loc_types$type_id == input$loc_type,
      "type_suffix"
    ]

    if (is.na(type_suffix)) {
      type_suffix <- "OT"
      showNotification(
        paste0(
          "Location type '",
          moduleData$loc_types[
            moduleData$loc_types$type_id == input$loc_type,
            "type"
          ],
          "' can't be matched to a predefined suffix. Using 'OT' (other) as the suffix. PLEASE LET THE DATABASE ADMIN KNOW SO THIS CAN BE UPDATED!"
        ),
        type = "warning",
        duration = 10
      )
    }

    if (is.na(code) || code == "") {
      showModal(modalDialog(
        "Warning: could not extract a valid National Hydro Network code from the provided latitude and longitude. This probably means that your location is outside Canada or not within any NHN polygon. We'll use 'NA' as a code instead, but please reach out to the maintainer of this R package to discuss adding support for your area.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      code <- "NA"
    }

    code <- paste0(code, "-", type_suffix)

    # Find the database's greatest existing code with that prefix in column 'location_code' of table 'locations'
    existing_code <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "
      SELECT MAX(location_code) AS max_code
      FROM public.locations
      WHERE location_code LIKE $1
      ",
      params = list(paste0(code, "%"))
    )[1, 1]

    if (is.na(existing_code)) {
      code <- paste0(code, "-00001")
    } else {
      # Increment the numeric suffix by 1
      # Find the last numbers with no in-between characters (maay not be a hyphen)
      suffix_num <- as.integer(sub(
        "^.*?(\\d+)$",
        "\\1",
        existing_code
      ))
      suffix_num <- suffix_num + 1
      if (suffix_num <= 99999) {
        code <- paste0(code, "-", sprintf("%05d", suffix_num))
      } # else it'll just increase in number
    }

    updateTextInput(
      session,
      "loc_code",
      value = code
    )
  })

  # Make ExtendedTask to fetch NHN basins
  fetch_basins_task <- ExtendedTask$new(
    function(config) {
      promises::future_promise(expr = {
        con <- AquaConnect(
          name = config$dbName,
          host = config$dbHost,
          port = config$dbPort,
          username = config$dbUser,
          password = config$dbPass,
          silent = TRUE
        )
        on.exit(DBI::dbDisconnect(con)) # Disconnect when done

        # Fetch the file. terra::vect will auto-unzip.
        vect <- terra::vect(
          "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_decoupage.gpkg.zip"
        )
        vect <- vect[, c("validity_date", "dataset_name", "edition", "version")]
        vect$description <- paste0(
          "Edition: ",
          vect$edition,
          ", Version: ",
          vect$version
        )

        AquaCache::insertACVector(
          geom = vect,
          layer_name = "National Hydro Network - Basins",
          feature_name_col = "dataset_name",
          description_col = "description",
          con = con,
          overwrite = FALSE,
          ask = FALSE
        )
      })
    }
  ) |>
    bslib::bind_task_button("fetch_nhn")

  observeEvent(input$fetch_nhn, {
    fetch_basins_task$invoke(config = session$userData$config)
  })
}
