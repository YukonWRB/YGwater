YGwater_globals <- function(
  dbName,
  dbHost,
  dbPort,
  dbUser,
  dbPass,
  RLS_user,
  RLS_pass,
  network_check,
  accessPath1,
  accessPath2,
  logout_timer_min,
  public
) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  # Use a user-writable cache directory for sass
  cache_dir <- tools::R_user_dir("YGwater", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  options(bslib.sass.cache = cache_dir)

  # Initialize a shared cache environment available to all sessions
  if (!exists("app_cache", envir = .GlobalEnv)) {
    assign("app_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }

  # Load the cache functions (in a file so they can be used across a few modules)
  source(system.file(
    "apps/YGwater/modules/cache_functions.R",
    package = "YGwater"
  ))

  g_drive <- FALSE
  if (!isFALSE(network_check)) {
    network_check <- dir.exists(network_check)
    # YG specific code here
    if (!public) {
      # confirm G drive access for FOD reports
      g_drive <- dir.exists(
        "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/"
      )
    }
    if (g_drive) {
      # FOD module (only visible internally)
      source(system.file(
        "apps/YGwater/modules/client/FOD/FOD_main.R",
        package = "YGwater"
      ))
    }
  }

  if (!public) {
    # 'Admin' side modules #####
    # database admin modules
    source(system.file(
      "apps/YGwater/modules/admin/locations/locationMetadata.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/locations/addLocation.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/locations/addSubLocation.R",
      package = "YGwater"
    ))

    # equipment sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/equipment/calibrate.R",
      package = "YGwater"
    ))

    # continuous data sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/addContData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/continuousCorrections.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/imputeMissing.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/editContData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/grades_approvals_qualifiers.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/addTimeseries.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/syncCont.R",
      package = "YGwater"
    ))

    # discrete data sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addSamples.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/editDiscData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addSampleSeries.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addGuidelines.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/syncDisc.R",
      package = "YGwater"
    ))

    # Borehole/well modules
    source(system.file(
      "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
      package = "YGwater"
    ))

    # Field visit modules
    source(system.file(
      "apps/YGwater/modules/admin/field/field_visit.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/field/deploy_recover.R",
      package = "YGwater"
    ))

    # Files/document/image sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/documents/addDocs.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/imgupload/addImgs.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/imgupload/addImgSeries.R",
      package = "YGwater"
    ))

    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/adminLanding.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/manageNewsContent.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/manageNotifications.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/viewFeedback.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/users/manageUsers.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/users/changePassword.R",
      package = "YGwater"
    ))

    # Set up a temporary directory for storing R documentation files during app runtime
    .rd_dir <<- file.path(tempdir(), "rdocs")
    dir.create(.rd_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath("rdocs", .rd_dir)

    # define some functions for later use
    array_to_text <<- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character())
      }
      if (is.list(value)) {
        value <- value[[1]]
      }
      value <- gsub("[{}\"]", "", value)
      out <- trimws(unlist(strsplit(value, ",")))
      out[nzchar(out)]
    }

    share_with_to_array <<- function(groups) {
      if (is.null(groups) || !length(groups) || all(!nzchar(groups))) {
        groups <- "public_reader"
      }
      groups <- gsub('"', '\\"', groups, fixed = TRUE)
      paste0("{", paste(sprintf('"%s"', groups), collapse = ","), "}")
    }

    selections_to_array <<- function(groups) {
      if (is.null(groups) || !length(groups) || all(!nzchar(groups))) {
        return(NULL)
      }
      groups <- gsub('"', '\\"', groups, fixed = TRUE)
      paste0("{", paste(sprintf('"%s"', groups), collapse = ","), "}")
    }

    # Take the JSON string coming from the DB and make it into a readable text string
    parse_source_args <<- function(value) {
      if (
        is.null(value) ||
          length(value) == 0 ||
          all(is.na(value)) ||
          !nzchar(value)
      ) {
        return("")
      }
      parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) NULL)
      if (is.null(parsed)) {
        return(value)
      }
      if (length(parsed) == 0) {
        return("")
      }
      if (is.list(parsed) && !is.data.frame(parsed)) {
        parsed <- unlist(parsed)
      }
      if (is.null(names(parsed))) {
        return(paste(parsed, collapse = ", "))
      }
      entries <- paste(names(parsed), parsed, sep = ": ")
      paste(entries, collapse = ", ")
    }

    # Format source function arguments to JSON for input to database
    format_source_args <<- function(args) {
      # split into "argument1: value1" etc.
      args <- strsplit(args, ",\\s*")[[1]]

      # split only on first colon
      keys <- sub(":.*", "", args)
      vals <- sub("^[^:]+:\\s*", "", args)

      # build named list
      args <- stats::setNames(as.list(vals), keys)

      # convert to JSON
      args <- jsonlite::toJSON(args, auto_unbox = TRUE)
    }

    # Increase the maximum upload size to 100 MB, necessary for some admin modules (NOTE that a change to NGINX parameters is also necessary)
    options(shiny.maxRequestSize = 1024 * 1024^2)
  } # End of if public = FALSE

  application_notifications_ui <<- function(
    ns,
    lang,
    con,
    module_id, # Set to 'all' to show the notification across all modules
    banner_key_prefix = "notification",
    fallback_lang = "English"
  ) {
    get_active_notifications <- function(
      con,
      module_id,
      lang,
      fallback_lang = fallback_lang
    ) {
      if (is.null(con) || is.null(module_id) || !nzchar(module_id)) {
        return(list())
      }
      if (
        !DBI::dbExistsTable(
          con,
          DBI::Id(schema = "application", table = "notifications")
        )
      ) {
        return(list())
      }
      notifications <- DBI::dbGetQuery(
        con,
        "
        SELECT notification_id, target_module, message
        FROM application.notifications
        WHERE active IS TRUE
          AND ($1 = ANY(target_module) OR 'all' = ANY(target_module))
        ",
        params = list(module_id)
      )
      if (!nrow(notifications)) {
        return(list())
      }
      results <- list()

      # Helper function to parse the array column of target modules
      parse_text_array <- function(value) {
        if (is.null(value) || !length(value) || all(is.na(value))) {
          return(character())
        }
        if (is.list(value)) {
          value <- value[[1]]
        }
        if (length(value) > 1) {
          return(trimws(value))
        }
        value <- gsub("[{}\"]", "", value)
        if (!nzchar(value)) {
          return(character())
        }
        out <- trimws(unlist(strsplit(value, ",")))
        out[nzchar(out)]
      }

      # Helper function to extract and format notification message
      extract_notification_message <- function(
        message,
        lang,
        fallback_lang = fallback_lang
      ) {
        if (is.null(message) || all(is.na(message))) {
          return(NA_character_)
        }
        parsed <- message
        if (is.character(message)) {
          parsed <- tryCatch(
            jsonlite::fromJSON(message),
            error = function(e) message
          )
        }
        if (is.list(parsed)) {
          if (!is.null(lang) && lang %in% names(parsed)) {
            return(parsed[[lang]])
          }
          if (fallback_lang %in% names(parsed)) {
            return(parsed[[fallback_lang]])
          }
          if (length(parsed)) {
            return(unname(parsed[[1]]))
          }
        }
        if (is.character(parsed)) {
          return(parsed[[1]])
        }
        NA_character_
      }

      for (i in seq_len(nrow(notifications))) {
        targets <- parse_text_array(notifications$target_module[i])

        if (
          !length(targets) || !(module_id %in% targets || "all" %in% targets)
        ) {
          next
        }
        msg <- extract_notification_message(
          notifications$message[i],
          lang = lang,
          fallback_lang = fallback_lang
        )
        if (is.na(msg) || !nzchar(msg)) {
          next
        }
        results[[length(results) + 1]] <- list(
          id = notifications$notification_id[i],
          message = msg
        )
      }
      return(results)
    }

    notifications <- get_active_notifications(
      con = con,
      module_id = module_id,
      lang = lang,
      fallback_lang = fallback_lang
    )

    if (!length(notifications)) {
      return(NULL)
    }
    tagList(lapply(notifications, function(notification) {
      dismissible_banner_ui(
        ns = ns,
        msg_html = notification$message,
        banner_id = paste0("notification_", notification$id),
        banner_key_prefix = paste0(banner_key_prefix, "_", notification$id)
      )
    }))
  }

  dismissible_banner_ui <<- function(
    ns,
    msg_html,
    banner_id = "app_banner",
    banner_key_prefix = "global_notice",
    banner_version = utils::packageVersion("YGwater")
  ) {
    banner_dom_id <- ns(banner_id)
    banner_key <- paste0(banner_key_prefix, "_", banner_version)

    dismiss_fn_name <- paste0("dismiss_", banner_dom_id) # may contain "-" so call via window[...]()

    tagList(
      tags$head(
        tags$style(HTML(sprintf(
          "#%s { margin-bottom: 1rem; }",
          banner_dom_id
        ))),
        tags$script(HTML(sprintf(
          "
          (function () {
            const bannerId = '%s';
            const bannerKey = '%s';
  
            function hideIfDismissed() {
              try {
                if (localStorage.getItem(bannerKey) === '1') {
                  const el = document.getElementById(bannerId);
                  if (el) el.style.display = 'none';
                }
              } catch (e) {}
            }
  
            window['%s'] = function () {
              try { localStorage.setItem(bannerKey, '1'); } catch (e) {}
              const el = document.getElementById(bannerId);
              if (el) el.style.display = 'none';
            };
  
            document.addEventListener('DOMContentLoaded', hideIfDismissed);
          })();
        ",
          banner_dom_id,
          banner_key,
          dismiss_fn_name
        )))
      ),

      bslib::card(
        id = banner_dom_id,
        class = "mb-3",
        bslib::card_header(
          class = "d-flex align-items-start",
          style = "gap:12px;",
          tags$div(style = "flex: 1 1 auto; min-width: 0;", HTML(msg_html)),
          tags$button(
            type = "button",
            class = "btn-close ms-auto",
            `aria-label` = "Dismiss",
            onclick = sprintf("window['%s']();", dismiss_fn_name)
          )
        )
      )
    )
  }

  # Derive privilege flags for tables
  # @param tbl A table with at minimum columns called 'extra_privileges' with strings such as "SELECT, UPDATE", and column called 'qual_name' containing schema qualified table names such as 'public.table_name'.
  # @param qual_names A character vector of qualified table names (schema.table)
  # @param priv A list of character vectors of privileges to check for each table in 'qual_names'. If length(priv) == 1, the same privileges are checked for all tables.
  # @return Boolean. TRUE if the specified privileges exist for the listed tables, FALSE any privileges missing on any table.

  has_priv <<- function(
    tbl,
    qual_names,
    priv = list(c(
      "DELETE",
      "INSERT",
      "UPDATE"
    ))
  ) {
    if (length(priv) > 1) {
      # Ensure length 'priv' is same length as 'qual_names'
      if (length(priv) != length(qual_names)) {
        stop(
          "Length of 'priv' must be 1 or equal to length of 'qual_names'"
        )
      }
    } else {
      # Replicate 'priv' to match length of 'qual_names'
      priv <- rep(priv, length(qual_names))
    }
    # Ensure the user has ALL of the specified privileges on ALL of the specified tables
    for (i in seq_along(qual_names)) {
      qn <- qual_names[i]
      p <- priv[[i]]
      user_privs <- tbl$extra_privileges[
        tbl$qual_name == qn
      ]
      if (length(user_privs) == 0) {
        return(FALSE)
      }
      user_privs_vec <- unlist(strsplit(user_privs, ", "))
      if (!all(p %in% user_privs_vec)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  # 'client' side modules #####
  # Plot modules
  source(system.file(
    "apps/YGwater/modules/client/plot/discretePlot.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/plot/continuousPlot.R",
    package = "YGwater"
  ))

  # Non-public client-side modules
  if (!public) {
    # Old plot modules (kept for backward compatibility)
    source(system.file(
      "apps/YGwater/modules/client/plot/continuousPlot_old.R",
      package = "YGwater"
    ))

    # Report modules
    if (network_check) {
      source(system.file(
        "apps/YGwater/modules/client/reports/WQReport.R",
        package = "YGwater"
      ))
      source(system.file(
        "apps/YGwater/modules/client/reports/snowBulletin.R",
        package = "YGwater"
      ))
    }
    source(system.file(
      "apps/YGwater/modules/client/reports/snowInfo.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/client/reports/waterInfo.R",
      package = "YGwater"
    ))
  }

  # Map modules
  source(system.file(
    "apps/YGwater/modules/client/map/paramsMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/rasterMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/locationsMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/snowBulletinMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/WWR/registry_front_end.R",
    package = "YGwater"
  ))

  # Image and document modules
  source(system.file(
    "apps/YGwater/modules/client/images/image_table_view.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/images/image_map_view.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/documents/document_table_view.R",
    package = "YGwater"
  ))

  # Data modules
  source(system.file(
    "apps/YGwater/modules/client/data/continuousData.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/data/discreteData.R",
    package = "YGwater"
  ))

  # Info modules
  source(system.file(
    "apps/YGwater/modules/client/info/home.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/info/news.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/info/about.R",
    package = "YGwater"
  ))

  # Establish database connection parameters
  # The actual connection to AquaCache is being done at the server level and stored in session$userData$AquaCache. This allows using a login input form to connect to the database with edit privileges or to see additional elements

  ## Access database connections ###########
  # Look for .mdb files in the AccessPath directories
  if (network_check) {
    if (!is.null(accessPath1)) {
      if (dir.exists(accessPath1) & !public) {
        # List the *.mdb files in the directory
        mdb_files1 <- list.files(
          accessPath1,
          pattern = "*.mdb",
          full.names = TRUE
        )
        if (length(mdb_files1) == 0) {
          mdb_files1 <- NULL
        }
      } else {
        mdb_files1 <- NULL
      }
    } else {
      mdb_files1 <- NULL
    }
    if (!is.null(accessPath2)) {
      if (dir.exists(accessPath2) & !public) {
        # List the *.mdb files in the directory
        mdb_files2 <- list.files(
          accessPath2,
          pattern = "*.mdb",
          full.names = TRUE
        )
        if (length(mdb_files2) == 0) {
          mdb_files2 <- NULL
        }
      } else {
        mdb_files2 <- NULL
      }
    } else {
      mdb_files2 <- NULL
    }

    mdb_files <- c(mdb_files1, mdb_files2)

    if (is.null(mdb_files) & !public) {
      print("No .mdb files found in the accessPath directories.")
    }
  } else {
    mdb_files <- NULL
  }

  # Make the configuration list available globally
  # double assignment creates a global variable that can be accessed by all UI and server functions

  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    public = public,
    g_drive = g_drive, # YG specific - whether the app has access to the G drive where FOD reports are stored, which allows the FOD report module to be visible and functional
    network_check = network_check,
    mdb_files = mdb_files,
    logout_timer_min = logout_timer_min,
    admin = FALSE,
    sidebar_bg = "#FFFCF5", # Default background color for all sidebars
    main_bg = "#D9EFF2" # Default background color for all main panels
  )
}
