#' Capitalize titles in French or English
#'
#' @param text The text to capitalize.
#' @param language The language to use for capitalization, limited to 'fr' or 'en'.
#'
#' @return A string with proper capitalization for the language.
#' @export
#'

titleCase <- function(text, language = "en") {
  
  if (!language %in% c("en", "fr")) {
    stop("Language must be either 'en' or 'fr'.")
  }
  
  capitalize <- function(word) {
    return(stringr::str_to_title(word))
  }
  
  lowercase_words <- c("the", "and", "but", "or", "for", "nor", "on", "at", "to", "from", "by", "de", "d'", "et", "à", "la", "le", "les", "un", "une", "des")
  uppercase_words <- c("Yukon", "Klondike", "Alsek", "Takhini", "Alaska", "Canada", "Pelly", "Dawson", "Whitehorse", "Carmacks", "Pelly", "Stewart Crossing", "Mayo", "Keno", "Eagle", "Plains", "Watson", "Teslin", "Carcross", "Haines Junction","Ross River", "Rancheria")
  
  # Split the text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Capitalize function that respects lowercase words list
  words_capitalized <- mapply(function(word, index) {
    if (tolower(word) %in% lowercase_words & index != 1 & index != length(words)) {
      return(tolower(word))
    } else {
      if (language == "en") {
        return(capitalize(word))
      } else if (language == "fr") {
        if (capitalize(word) %in% uppercase_words) {
          return(capitalize(word))
        } else {
          return(tolower(word))}
      }
    }
  }, words, seq_along(words))
  
  # Special handling for French apostrophes
  if (language == "fr") {
    words_capitalized <- gsub(" D'", " d'", paste(words_capitalized, collapse = " "), ignore.case = FALSE)
  }
  
  # Re-join the words into a single string
  result <- paste(words_capitalized, collapse = " ")
  
  return(result)
}
