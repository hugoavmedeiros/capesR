% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capes.R
\name{read_capes_data}
\alias{read_capes_data}
\alias{ler_dados_capes}
\title{Read and filter data from the CAPES Catalog of Theses and Dissertations}
\usage{
read_capes_data(files, filters = list())

ler_dados_capes(files, filters = list())
}
\arguments{
\item{files}{A vector or list of paths to Parquet files.}

\item{filters}{A list of filters to apply (e.g., list(base_year = 1987, state = "SP", title = "education")).}
}
\value{
A `data.frame` containing the combined and filtered data.
}
\description{
This function combines data from multiple Parquet files and applies optional filters, including text-based searches.
}
\examples{
\donttest{
# Download data for the years 1987 and 1990
capes_files <- download_capes_data(c(1987, 1990))
# Combine all selected data
combined_data <- read_capes_data(capes_files)
}

}
