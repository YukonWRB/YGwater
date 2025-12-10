# this set of functions standardizes inputs for various functions in the package

#' Standardize Language Input to short form
#'
#' @description
#' Standardizes the input language to either "en" (English) or "fr" (French).
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as
#'   "en", "english", "anglais" for English, and "fr", "french", "fran\u00e7ais", "francais",
#'   "fran\u00e7ais" for French. Case-insensitive.
#'
#' @return A character string: "en" for English or "fr" for French.
#' @export
#'
#' @examples
#' shortenLanguage("EN")        # returns "en"
#' shortenLanguage("fran\u00e7ais")  # returns "fr"
#' shortenLanguage(NULL)        # returns "English"
#' shortenLanguage("de")        # returns "en" with a warning

shortenLanguage <- function(language) {
    language <- tolower(language)
    english_variants <- c("en", "english", "anglais")
    french_variants <- c(
        "fr",
        "french",
        "fran\u00e7ais",
        "francais",
        "fran\u00e7ais"
    )
    # standardize language input to "English" or "French"
    if (is.null(language)) {
        return("en")
    }

    language <- tolower(language)
    if (language %in% english_variants) {
        return("en")
    } else if (language %in% french_variants) {
        return("fr")
    } else {
        warning("Unrecognized language input. Defaulting to English.")
        return("en")
    }
}

#' Standardize Language Input to Full Name
#' @description
#' Standardizes the input language to either "English" or "Fran\u00e7ais".
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as "en", "english", "anglais" for English, and "fr", "french", "fran\u00e7ais", "francais", "fran\u00e7ais" for French. Case-insensitive.
#' @return A character string: "English" for English or "Fran\u00e7ais" for French.
#' @export
#'
#' @examples
#' lengthenLanguage("EN")        # returns "English"
#' lengthenLanguage("\u00e7")  # returns "Fran\u00e7ais"
#' lengthenLanguage(NULL)        # returns "English"

lengthenLanguage <- function(language) {
    language <- tolower(language)
    english_variants <- c("en", "english", "anglais")
    french_variants <- c(
        "fr",
        "french",
        "fran\u00e7ais",
        "francais",
        "fran\u00e7ais"
    )
    # standardize language input to "English" or "French"
    if (is.null(language)) {
        return("English")
    }

    language <- tolower(language)
    if (language %in% english_variants) {
        return("English")
    } else if (language %in% french_variants) {
        return("Fran\u00e7ais")
    } else {
        warning("Unrecognized language input. Defaulting to English.")
        return("English")
    }
}
