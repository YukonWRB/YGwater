#' Capitalize titles in French or English
#' 
#' @description
#' `r lifecycle::badge('stable')`
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
  
  lowercase_words <- c("the", "and", "but", "or", "for", "nor", "on", "at", "to", "from", "by", "de", "d'", "et", "\u00E0", "la", "le", "les", "un", "une", "des")
  uppercase_words <- c("Yukon", "Klondike", "Alsek", "Takhini", "Alaska", "Canada", "Pelly", "Dawson", "Whitehorse", "Carmacks", "Stewart", "Johnson's", "Crossing", "Mayo", "Keno", "Eagle", "Plains", "Watson", "Teslin", "Carcross", "Haines", "Junction", "Ross", " River", "Rancheria", "Nisutlin", "Laberge", "Old", "Crow", "C.-B.", "Colombie", "Britanique", "Nord-Ouest", "King", "Solomon", "Midnight", "North", "\u00C9tats-Unis", "(\u00C9tats-Unis)", "Burwash", "Landing", "Wellgreen", "Kluane", "Quill", "Duke", "Silver", "City", "Christmas", "Summit", "Dezadeash", "Pine", "Profile", "Felsite", "Alder", "Dalton", "Post", "Million", "Dollar", "South", "Canol", "Stevens")
  all_uppercase_words <- c("SWE", "EEN", "ECCC", "d'ECCC", "(US)", "SWDF")
  
  french_keywords <- c("rivi\u00E8re", "ruisseau", "montagne", "sommet", "lac", "pont", "route", "autoroute")
  
  processText <- function(text) {
    words <- unlist(strsplit(text, "\\s+"))
    
    # Capitalize function that respects lowercase and uppercase words list
    words_capitalized <- mapply(function(word, index) {
      if (word %in% all_uppercase_words) {
        return(toupper(word))
      } else if (tolower(word) %in% lowercase_words & index != 1 & index != length(words)) {
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
    
    # French capitalization rule for keywords
    if (language == "fr") {
      if (length(words) > 1) {
        for (i in 1:(length(words) - 1)) {
          if (tolower(words[i]) %in% french_keywords) {
            if (words[i + 1] != "de") {
              words_capitalized[[i + 1]] <- stringr::str_to_title(words_capitalized[[i + 1]])
            } else {
              if (i < (length(words) - 1)) {
                words_capitalized[[i + 2]] <- stringr::str_to_title(words_capitalized[[i + 2]])
              }
            }
            
          }
        }
      }
      
    }
    
    # Special handling for French apostrophes
    if (language == "fr") {
      words_capitalized <- gsub(" D'", " d'", paste(words_capitalized, collapse = " "), ignore.case = FALSE)
      words_capitalized <- gsub(" L'", " l'", paste(words_capitalized, collapse = " "), ignore.case = FALSE)
    }
    
    result <- paste(words_capitalized, collapse = " ")
    return(result)
  }
  
  # Use vapply to apply processText to each element of text, ensuring character output
  result <- vapply(text, processText, character(1))
  
  return(unname(result))
}
