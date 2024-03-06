#' Capitalize titles in French or English
#' 
#' This function capitalizes a string according to the rules of the English or French language. It is based on the `str_to_title` function from the `stringr` package, but with additional rules to handle lowercase words and proper nouns specific to the Yukon, such as Klondike, Alsek, Takhini, and others.
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
  
  lowercase_words <- c("the", "and", "but", "or", "for", "nor", "on", "at", "to", "from", "by", "de", "d'", "et", "à", "la", "le", "les", "un", "une", "des")
  uppercase_words <- c("Yukon", "Klondike", "Alsek", "Takhini", "Alaska", "Canada", "Pelly", "Dawson", "Whitehorse", "Carmacks", "Stewart", "Crossing", "Mayo", "Keno", "Eagle", "Plains", "Watson", "Teslin", "Carcross", "Haines", "Junction", "Ross", " River", "Rancheria", "Nisutlin", "Laberge", "Old", "Crow", "C.-B.", "Colombie", "Britanique", "Nord-Ouest", "King", "Solomon", "Midnight", "North")
  
  # Split the text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Capitalize function that respects lowercase and uppercase words list
  words_capitalized <- mapply(function(word, index) {
    if (tolower(word) %in% lowercase_words & index != 1 & index != length(words)) {
      return(tolower(word))
    } else if (index == 1) {
      return(stringr::str_to_title(word))
    } else {
      if (language == "en") {
        return(stringr::str_to_title(word))
      } else if (language == "fr") {
        if (stringr::str_to_title(word) %in% uppercase_words) {
          return(stringr::str_to_title(word))
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
