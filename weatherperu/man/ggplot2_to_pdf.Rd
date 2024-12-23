% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ggplot2_to_pdf}
\alias{ggplot2_to_pdf}
\title{Save a ggplot2 plot as PDF, using LaTeX tikz}
\usage{
ggplot2_to_pdf(
  plot,
  path_to_latex = "/Library/TeX/texbin/",
  interpreter = "pdflatex",
  path = "./",
  filename,
  keep_tex = FALSE,
  width = 15,
  height = 15,
  verbose = FALSE,
  ignore.stdout = TRUE
)
}
\arguments{
\item{plot}{ggplot2 object}

\item{path_to_latex}{path to LaTeX engine (Defaults to
`/Library/TeX/texbin/`)}

\item{interpreter}{by default, use pdflatex (`pdflatex`)}

\item{path}{path to the destination folder}

\item{filename}{file name (without the extension)}

\item{keep_tex}{should the tex file be kept after compilation? Defaults to
`FALSE`}

\item{width}{width in inches (default to 15)}

\item{height}{height in inches (default to 15)}

\item{verbose}{A logical value indicating whether diagnostic messages are
printed when measuring dimensions of strings. Defaults to `FALSE`}

\item{ignore.stdout}{a logical (not NA) indicating whether messages written
to ‘stdout’  should be ignored. Defaults to `TRUE`}
}
\description{
Save a ggplot2 plot as PDF, using LaTeX tikz
}
