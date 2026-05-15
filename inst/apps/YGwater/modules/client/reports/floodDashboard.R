floodDashboardUIMod <- function(id) {
    ns <- NS(id)

    tagList(
        uiOutput(ns("content"))
    )
}

floodDashboardMod <- function(id, language, inputs = NULL) {
    moduleServer(id, function(input, output, session) {
        req(language$language)

        con <- session$userData$AquaCache
        req(con)

        standalone_path <- file.path(
            getwd(),
            "dev",
            "freshet_forecasting",
            "dashboard.r"
        )

        validate(
            need(
                file.exists(standalone_path),
                "Cannot locate dev/freshet_forecasting/dashboard.r from current working directory."
            )
        )

        standalone_env <- new.env(parent = globalenv())
        standalone_env$con <- con
        sys.source(standalone_path, envir = standalone_env)

        validate(
            need(
                exists("launch_freshet_dashboard", envir = standalone_env, inherits = FALSE),
                "launch_freshet_dashboard() was not found in dashboard.r"
            )
        )

        app <- standalone_env$launch_freshet_dashboard(con = con)

        output$content <- renderUI({
            app$ui
        })

        root_session <- session$rootScope()
        server_fun <- NULL

        if (is.function(app$serverFuncSource)) {
            server_fun <- app$serverFuncSource()
        } else if (is.function(app$server)) {
            server_fun <- app$server
        }

        validate(
            need(
                is.function(server_fun),
                "Unable to resolve server function for flood dashboard app object."
            )
        )

        session$onFlushed(
            function() {
                server_fun(
                    root_session$input,
                    root_session$output,
                    root_session
                )
            },
            once = TRUE
        )
    })
}
