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
read_model <- function(filename = system.file("exdata/model.Rdata",
                                              package = "calanusthreshold")){
  name <- load(filename[1])
  get(name, inherits = FALSE)
}

#' Read configuration
#' 
#' @export
#' @param filename the file to read
#' @return a list of configuration values
read_config <- function(filename = system.file("exdata/model.yaml",
                                               package = "calanusthreshold")){
  yaml::read_yaml(filename)
}


#' Write configuration
#' 
#' @export
#' @param x configuration (a list)
#' @param filename the file to write
#' @return a list of configuration values
write_config <- function(x, filename = "config.yaml"){
  yaml::write_yaml(x, filename)
}



#' Read one or more prediction files
#' 
#' If multiple files are provided then a "version" column is prepended
#'
#' @export
#' @param filename charcater, one or more filenames
#' @return tibble (possibly with a 'version' column)
read_pred <- function(filename = system.file("exdata/prediction.csv.gz",
                                             package = "calanusthreshold")){
  if(length(filename) > 1){
    x <- readr::read_csv(filename, show_col_types = FALSE, id = 'version') |>
      dplyr::relocate(version, .before = 1) |>
      dplyr::mutate(version = gsub(".csv.gz", "", basename(version), fixed = TRUE))
  } else {
    x <- readr::read_csv(filename, show_col_types = FALSE)
  }
  
  dplyr::mutate(x,
                patch = factor(as.integer(.data$patch), levels = c(0L, 1L)),
                .pred_class = factor(as.integer(.data$.pred_class), levels = c(0L, 1L)))
}


#' Write a prediction file
#'
#' @export
#' @param x table of predictions
#' @param filename character, the filename to write to
#' @return the inout, \code{x}
write_pred <- function(x, filename){
  readr::write_csv(x, filename)
}


#' Read a raster
#' 
#' @export
#' @param filename character, the name of the file to read
#' @param form characater, one of 'raster', 'sf' or 'tibble'.  If the latter then
#'   coordinate columns (x, y) followed by one column per variable.
#' @return raster or tibble object
read_raster <- function(filename = system.file("exdata/predictors.tif",
                                               package = "calanusthreshold"),
                        form = c("raster", "tibble", "sf")[1]){
  x <- stars::read_stars(filename)
  if (length(dim(x)) > 2) x <- split(x, "band")
  switch(tolower(form[1]),
         "sf" = sf::st_as_sf(x),
         "tibble" =  dplyr::as_tibble(x),
         x)
}

#' Write a raster
#' 
#' @export
#' @param x raster object
#' @param filename character, the name of the file to write
#' @param overwrite logical, if TRUE allow overwrite of an existing file
#' @return stars raster object
write_raster <- function(x, filename = "raster.tif", overwrite = TRUE){
  if (!overwrite && file.exists(filename)){
    warning("file already exists:", filename)
    return(x)
  }
  stars::write_stars(x, filename)
}