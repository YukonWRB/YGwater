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
                c(
                    "Le routage du module utilise maintenant l'identifiant floodDashboard.",
                    "Cette page peut etre remplacée par le tableau de bord réel quand il sera prêt.",
                    "Les libellés de navigation existants restent inchangés."
                )
            } else {
                c(
                    "The module is now wired through the floodDashboard identifier.",
                    "This page can be replaced with the real dashboard when ready.",
                    "Existing navigation labels remain unchanged."
                )
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
