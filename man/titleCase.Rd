% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/titleCase.R
\name{titleCase}
\alias{titleCase}
\title{Capitalize titles in French or English}
\usage{
titleCase(text, language = "en")
}
\arguments{
\item{text}{The text to capitalize as a character vector of length n.}

\item{language}{The language to use for capitalization, limited to 'fr' or 'en'.}
}
\value{
A string with proper capitalization for the language.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function capitalizes a string according to the rules of the English or French language. Compared to sringr::str_to_title, has additional rules to handle lowercase words and proper nouns specific to the Yukon, such as Klondike, Alsek, Takhini, and others. Handles apostrophes in French words and all uppercase 'words' such as 'SWE' or 'ECCC'.
}
