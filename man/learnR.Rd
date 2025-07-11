% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learnR.R
\name{learnR}
\alias{learnR}
\title{learnR: learn R with a Water Resources Branch twist}
\usage{
learnR(lesson = NULL)
}
\arguments{
\item{lesson}{The lesson number to open. Leave NULL (default) and the function will attempt to find the last lesson you had open, or, if you completed the lesson and ran the last line, the next lesson.}
}
\value{
A file with lessons to learn R.
}
\description{
This function takes no arguments and opens an R script where you can learn R in a Water Resources Branch context.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

# Open the last lesson you worked on, or lesson 1 is you haven't opened a lesson yet:
learnR()

# Open the first lesson:
learnR(1)
\dontshow{\}) # examplesIf}
}
