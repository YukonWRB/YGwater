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
  uppercase_words <- c("Yukon", "Klondike", "Alsek", "Takhini", "Alaska", "Canada", "Pelly", "Dawson", "Whitehorse", "Carmacks", "Stewart", "Johnson's", "Crossing", "Mayo", "Keno", "Eagle", "Plains", "Watson", "Teslin", "Carcross", "Haines", "Junction", "Ross", " River", "Rancheria", "Nisutlin", "Laberge", "Old", "Crow", "Colombie", "Britanique", "Nord-Ouest", "King", "Solomon", "Midnight", "North", "\u00C9tats-Unis", "(\u00C9tats-Unis)", "Burwash", "Landing", "Wellgreen", "Kluane", "Quill", "Duke", "Silver", "City", "Christmas", "Summit", "Dezadeash", "Pine", "Profile", "Felsite", "Alder", "Dalton", "Post", "Million", "Dollar", "South", "Canol", "Stevens", "Plume", "Atlin", "Log", "Cabin", "McIntyre")
  all_uppercase_words <- c("SWE", "EEN", "ECCC", "d'ECCC", "(US)", "SWDF", "(B.C.)", "C.-B.", "(C.-B.)")
  
  french_keywords <- c("rivi\u00E8re", "ruisseau", "montagne", "sommet", "mont", "lac", "pont", "route", "autoroute", "plage")
  
  processText <- function(text) {
    words <- unlist(strsplit(text, "\\s+"))
    
    # Capitalize function that respects lowercase and uppercase words list
    words_capitalized <- mapply(function(word, index) {
      if (language == "fr") {
        if (grepl("'", word)) {
          apostrophe <- TRUE
          word.sub <- substr(word, 3, nchar(word))
        } else {
          apostrophe <- FALSE
          word.sub <- word
        }
      } else {
        apostrophe <- FALSE
        word.sub <- word
      }
      if (word.sub %in% all_uppercase_words) {
        return(toupper(word))
      } else if (tolower(word.sub) %in% lowercase_words & index != 1) {
        return(tolower(word))
      } else if (index == 1) { #The first word should always be capitalized
        if (apostrophe) { #The first letter and the letter after the apostrophe should be capitalized
          before_apostrophe <- toupper(substr(word, 1, 2))
          char_after_apostrophe <- toupper(substr(word, 3, 3))
          after_apostrophe <- substr(word, 4, nchar(word))
          return(paste0(before_apostrophe, char_after_apostrophe, after_apostrophe))
        } else {
          return(stringr::str_to_title(word))
        }
      } else { #The rest of the words should be capitalized if in English. In French, only if they are specified as uppercase word
        if (language == "en") {
          return(stringr::str_to_title(word))
        } else if (language == "fr") {
          if (tolower(word.sub) %in% tolower(uppercase_words)) {
            if (apostrophe) {
              before_apostrophe <- tolower(substr(word, 1, 2))
              char_after_apostrophe <- toupper(substr(word, 3, 3))
              after_apostrophe <- substr(word, 4, nchar(word))
              return(paste0(before_apostrophe, char_after_apostrophe, after_apostrophe))
            } else {
              return(stringr::str_to_title(word))
            }
          } else {
            return(tolower(word))}
        }
      } 
    }, words, seq_along(words))
    
    # French capitalization rule for keywords
    if (language == "fr") {
      if (length(words_capitalized) > 1) {
        for (i in 1:(length(words_capitalized) - 1)) {
          stripped <- tolower(sub("^.+?'", "", words_capitalized[i]))
          if (stripped %in% french_keywords) {
            if (!(words_capitalized[[i + 1]] %in% c("de", "du", "des"))) {
              words_capitalized[[i + 1]] <- stringr::str_to_title(words_capitalized[[i + 1]])
            } else {
              if (i < (length(words_capitalized) - 1)) {
                if (grepl("'", words_capitalized[[i + 2]])) {
                  before_apostrophe <- substr(words_capitalized[[i + 2]], 1, 2)
                  char_after_apostrophe <- toupper(substr(words_capitalized[[i + 2]], 3, 3))
                  after_apostrophe <- substr(words_capitalized[[i + 2]], 4, nchar(words_capitalized[[i + 2]]))
                  words_capitalized[[i + 2]] <- paste0(before_apostrophe, char_after_apostrophe, after_apostrophe)
                } else {
                  words_capitalized[[i + 2]] <- stringr::str_to_title(words_capitalized[[i + 2]])
                }
              }
            }
          }
        }
      }
    }
    
    result <- paste(words_capitalized, collapse = " ")
    return(result)
  }
  
  # Use vapply to apply processText to each element of text, ensuring character output
  result <- vapply(text, processText, character(1))
  
  return(unname(result))
}
