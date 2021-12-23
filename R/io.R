#' Read an input file source
#'
#' @export
#' @param filename character, the file to read. The default is to read a dummy example data file.
#' @param drop_vars character or NULL, variables (columns) to drop from input
#' @param na.rm logical, if TRUE drop any cases (rows) with one or more missing values
#' @return tibble or simple feature
read_dataset <- function(filename = system.file("exdata/fake_data.csv.gz",
                                              package = "calanusthreshold"),
                         drop_vars = c("station", 
                                  "latitude",
                                  "longitude", 
                                  "siconc",
                                  "sithick",
                                  "geometry"),
                         na.rm = TRUE){

  x <- readr::read_csv(filename, show_col_types = FALSE)
  if (length(drop_vars > 0))x <- dplyr::select(x, -dplyr::any_of(drop_vars))
  if (na.rm)  x <- tidyr::drop_na(x)
  x
}

#' Save a model to a file
#' 
#' @export
#' @param x model structure
#' @param filename character, the name of the file to save.
#' @param overwrite logical, overwrite existing files? It is an error to try to without
#'   setting this argument to TRUE
#' @return the input model
save_model <- function(x, filename = "calanusthreshold_model.Rdata",
                       overwrite = FALSE){
  
  if (file.exists(filename[1]) & overwrite == FALSE){
    stop("File exists, please set overwrite to TRUE to replace existing")
  }
  save(x, file = filename[1])
  invisible(x)
}

#' Read a model from a file
#' 
#' @export
#' @param filename character, the name of the file to read
#' @return the model
read_model <- function(filename = "calanusthreshold_model.Rdata"){
  name <- load(filename[1])
  get(name, inherits = FALSE)
}
