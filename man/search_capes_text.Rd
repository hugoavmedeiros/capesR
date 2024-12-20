% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capes.R
\name{search_capes_text}
\alias{search_capes_text}
\alias{buscar_texto_capes}
\title{Search for terms in text fields of the CAPES Catalog of Theses and Dissertations data}
\usage{
search_capes_text(data, term, field)

buscar_texto_capes(data, term, field)
}
\arguments{
\item{data}{A `data.frame` containing the CAPES Catalog of Theses and Dissertations data.}

\item{term}{A string, the term to search for.}

\item{field}{A string, the name of the field to search in (e.g., "resumo", "titulo").}
}
\value{
A `data.frame` with rows matching the search or a message indicating no results were found.
}
\description{
This function allows searching for specific terms in the text fields of a previously loaded `data.frame`.
}
\examples{
\donttest{
# Download data for the years 1987 and 1990
capes_files <- download_capes_data(c(1987, 1990))
# Combine all selected data
combined_data <- read_capes_data(capes_files)
# Search data
results <- search_capes_text(
data = combined_data,
term = "Educação",
  field = "titulo"
)
}
}
