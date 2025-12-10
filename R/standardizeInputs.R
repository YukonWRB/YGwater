# this set of functions standardizes inputs for various functions in the package

english_variants <- c("en", "english", "anglais")
french_variants <- c("fr", "french", "français", "francais", "fran\u00e7ais")

#' Standardize Language Input
#'
#' Standardizes the input language to either "en" (English) or "fr" (French).
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as
#'   "en", "english", "anglais" for English, and "fr", "french", "français", "francais",
#'   "fran\u00e7ais" for French. Case-insensitive.
#'
#' @return A character string: "en" for English or "fr" for French.
#' @examples
#' .standardizeLanguage("EN")        # returns "en"
#' .standardizeLanguage("français")  # returns "fr"
#' .standardizeLanguage(NULL)        # returns "English"
#' .standardizeLanguage("de")        # returns "en" with a warning
.shortenLanguage <- function(language) {
    # standardize language input to "English" or "French"
    if (is.null(language)) {
        return("English")
    }

    language <- tolower(language)
    if (language %in% c(english_variants)) {
        return("en")
    } else if (language %in% c(french_variants)) {
        return("fr")
    } else {
        warning("Unrecognized language input. Defaulting to English.")
        return("en")
    }
}

#' Standardize Language Input to Full Name
#' Standardizes the input language to either "English" or "Français".
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as
#'   "en", "english", "anglais" for English, and "fr", "french", "français", "francais",
#'   "fran\u00e7ais" for French. Case-insensitive.
#' @return A character string: "English" for English or "Français" for French.
#' @examples
#' .lengthenLanguage("EN")        # returns "English"
#' .lengthenLanguage("français")  # returns "Français"
#' .lengthenLanguage(NULL)        # returns "English"
.lengthenLanguage <- function(language) {
    # standardize language input to "English" or "French"
    if (is.null(language)) {
        return("English")
    }

    language <- tolower(language)
    if (language %in% c("en", "english", "anglais")) {
        return("English")
    } else if (language %in% c(french_variants)) {
        return("Français")
    } else {
        warning("Unrecognized language input. Defaulting to English.")
        return("English")
    }
}
