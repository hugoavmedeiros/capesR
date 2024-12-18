utils::globalVariables(".data")


#' Identifiers (IDs) on OSF for the annual data of the Catalog of Theses and Dissertations from the Brazilian Coordination for the Improvement of Higher Education Personnel (CAPES)
#'
#' A data frame containing the years and the corresponding IDs for downloading the files.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{year}{Year of the data (1987-2022).}
#'   \item{osf_id}{OSF ID corresponding to the year.}
#' }
#' @source \url{https://osf.io/}
#' @examples
#' data(years_osf)
#' head(years_osf)
#'
"years_osf"


#' Synthetic CAPES Data
#'
#' Aggregated data from the CAPES Catalog of Theses and Dissertations, 
#' containing summarized information by year, institution, area, program, type, region, and state (UF).
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{base_year}{Reference year of the data.}
#'   \item{institution}{Higher Education Institution.}
#'   \item{area}{Area of Concentration.}
#'   \item{program_name}{Name of the Graduate Program.}
#'   \item{type}{Type of work (e.g., Master's, Doctorate).}
#'   \item{region}{Region of Brazil.}
#'   \item{state}{Federative Unit (state).}
#'   \item{n}{Total number of works.}
#' }
#' @source Synthetic data created from the CAPES Catalog of Theses and Dissertations.
#' @examples
#' data(capes_synthetic_df)
#' head(capes_synthetic_df)
#'
"capes_synthetic_df"


#' Read and filter data from the CAPES Catalog of Theses and Dissertations
#'
#' This function combines data from multiple Parquet files and applies optional filters, including text-based searches.
#'
#' @param files A vector or list of paths to Parquet files.
#' @param filters A list of filters to apply (e.g., list(base_year = 1987, state = "SP", title = "education")).
#' @return A `data.frame` containing the combined and filtered data.
#' @importFrom arrow open_dataset
#' @importFrom dplyr filter
#' @importFrom stringr str_detect fixed
#' @importFrom rlang sym
#' @examples
#' \donttest{
#' # Download data for the years 1987 and 1990
#' capes_files <- download_capes_data(c(1987, 1990))
#' # Combine all selected data
#' combined_data <- read_capes_data(capes_files)
#' }
#' 
#' @export
read_capes_data <- function(files, filters = list()) {
  # Convert a named list into a vector of paths, if necessary
  if (is.list(files)) {
    files <- unlist(files, use.names = FALSE)
  }
  
  # Check if the files exist
  if (any(!file.exists(files))) {
    stop("One or more specified files do not exist.")
  }
  
  # Open the Parquet files as a single dataset
  dataset <- arrow::open_dataset(files)
  
  # Get the column names of the dataset
  dataset_columns <- names(as.data.frame(dataset))
  
  # Check if all filter fields exist in the dataset
  invalid_columns <- setdiff(names(filters), dataset_columns)
  if (length(invalid_columns) > 0) {
    stop("The following filter columns do not exist in the dataset: ", paste(invalid_columns, collapse = ", "))
  }
  
  # Apply exact filters
  if (length(filters) > 0) {
    for (field in names(filters)) {
      value <- filters[[field]]
      
      if (field == "title" && is.character(value)) {
        # Text filter will be applied after reading
        next
      } else {
        # Filter for exact values
        dataset <- dataset %>%
          dplyr::filter(!!sym(field) %in% value)
      }
    }
  }
  
  # Load the filtered data into a `data.frame`
  data <- as.data.frame(dataset)
  
  # Apply text-based filter in memory
  if ("title" %in% names(filters) && !is.null(filters$title)) {
    term <- filters$title
    data <- data %>%
      dplyr::filter(stringr::str_detect(.data[["title"]], stringr::fixed(term, ignore_case = TRUE)))
  }
  
  # Return the final data.frame
  return(data)
}

#' @rdname read_capes_data
#' @export
ler_dados_capes <- read_capes_data

#' Search for terms in text fields of the CAPES Catalog of Theses and Dissertations data
#'
#' This function allows searching for specific terms in the text fields of a previously loaded `data.frame`.
#'
#' @param data A `data.frame` containing the CAPES Catalog of Theses and Dissertations data.
#' @param term A string, the term to search for.
#' @param field A string, the name of the field to search in (e.g., "resumo", "titulo").
#' @return A `data.frame` with rows matching the search or a message indicating no results were found.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect fixed
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' # Download data for the years 1987 and 1990
#' capes_files <- download_capes_data(c(1987, 1990))
#' # Combine all selected data
#' combined_data <- read_capes_data(capes_files)
#' # Search data
#' results <- search_capes_text(
#' data = combined_data,
#' term = "Educação",
#'   field = "titulo"
#' )
#' }
#' @export
search_capes_text <- function(data, term, field) {
  # Validate input
  if (missing(data) || missing(term) || missing(field)) {
    stop("The parameters `data`, `term`, and `field` are required.")
  }
  
  if (!is.data.frame(data)) {
    stop("The `data` parameter must be a `data.frame`.")
  }
  
  if (!field %in% colnames(data)) {
    stop(paste("The specified field ('", field, "') does not exist in the provided `data.frame`.", sep = ""))
  }
  
  # Filter for the term in the specified field
  results <- data %>%
    dplyr::filter(stringr::str_detect(.data[[field]], stringr::fixed(term, ignore_case = TRUE)))
  
  # Check if there are results
  if (nrow(results) == 0) {
    message("No results found for the search.")
    return(data.frame())
  }
  
  # Return the results
  return(results)
}


#' @rdname search_capes_text
#' @export
buscar_texto_capes <- search_capes_text

#' Download CAPES Data
#'
#' Downloads CAPES theses and dissertations data files from OSF for selected years.
#'
#' @param years A vector with the desired years.
#' @param destination The directory where the files will be saved (default: temporary directory).
#' @param timeout The timeout in seconds for the download process (default: 120 seconds).
#' @return A list of file paths for the downloaded or already existing files.
#' @importFrom utils download.file
#' @importFrom utils data 
#' @examples
#' \donttest{
#' # Download data for the years 1987 and 1990
#' capes_files <- download_capes_data(c(1987, 1990))
#' }
#' @export
download_capes_data <- function(years, destination = tempdir(), timeout = 120) {
  
  # Save the current timeout and restore it on exit
  original_timeout <- getOption("timeout")
  on.exit(options(timeout = original_timeout), add = TRUE)
  
  # Set the new timeout
  options(timeout = timeout)
  
  # Check if destination directory exists, if not, try to create it
  if (!dir.exists(destination)) {
    message("The directory does not exist. Attempting to create: ", destination)
    success <- dir.create(destination, recursive = TRUE)
    
    if (!success) {
      stop("Failed to create the directory: ", destination)
    } else {
      message("Directory successfully created: ", destination)
    }
  }
  
  # Load the dataset with OSF IDs
  data("years_osf", package = "capesR", envir = environment())
  
  # Filter IDs for the selected years
  filtered_ids <- capesR::years_osf[capesR::years_osf$year %in% years, ]
  
  # Check for invalid years
  if (nrow(filtered_ids) == 0) {
    stop("None of the selected years are available.")
  }
  
  # Loop to download files
  downloaded_files <- list()
  for (i in seq_len(nrow(filtered_ids))) {
    # Name of the destination file
    file_destination <- file.path(destination, paste0("capes_", filtered_ids$year[i], ".parquet"))
    
    # Check if the file already exists
    if (!file.exists(file_destination)) {
      url <- paste0("https://osf.io/download/", filtered_ids$osf_id[i], "/")
      message("Downloading: ", file_destination)
      
      # Try to download the file
      result <- tryCatch(
        {
          download.file(url, file_destination, mode = "wb")
          TRUE
        },
        error = function(e) {
          message("Failed to download file for year: ", filtered_ids$year[i])
          FALSE
        }
      )
      
      # Add to the list if successful
      if (result) {
        downloaded_files[[as.character(filtered_ids$year[i])]] <- file_destination
      }
    } else {
      message("File already exists: ", file_destination)
      downloaded_files[[as.character(filtered_ids$year[i])]] <- file_destination
    }
  }
  
  # Return the list of file paths
  return(downloaded_files)
}

#' @rdname download_capes_data
#' @export
baixar_dados_capes <- download_capes_data
