floodDashboardUIMod <- function(id) {
    ns <- NS(id)

    tagList(
        uiOutput(ns("content"))
    )
}

floodDashboardMod <- function(id, language, inputs = NULL) {
    moduleServer(id, function(input, output, session) {
        output$content <- renderUI({
            req(language$language)

            is_french <- identical(language$abbrev, "fr") ||
                identical(language$language, "Français")

            title_text <- if (is_french) {
                "Tableau de bord des conditions d'inondation"
            } else {
                "Flood Conditions Dashboard"
            }

            body_text <- if (is_french) {
                "Espace réservé pour le tableau de bord des conditions d'inondation."
            } else {
                "Placeholder for the flood conditions dashboard."
            }

            detail_items <- if (is_french) {
                "Ce tableau de bord est en cours de développement et sera bientôt disponible. Revenez plus tard pour les mises à jour."
            } else {
                "This dashboard is in development and will be coming soon. Check back later for updates."
            }

            bslib::card(
                full_screen = FALSE,
                bslib::card_header(tags$strong(title_text)),
                bslib::card_body(
                    tags$p(body_text),
                    tags$ul(lapply(detail_items, tags$li))
                )
            )
        })
    })
}
