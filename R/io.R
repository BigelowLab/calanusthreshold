#' Read an input file source
#'
#' @export
#' @param filename character, the file to read. The default is to read a dummy example data file.
#' @param form character, either 'tibble' (default) or 'sf'
#' @param crs character or numeric, if \code{form} is 'sf' then pass this to
#'   \code{\link[sf]{st_as_sf}}
#' @return tibble or simple feature
read_dataset <- function(filename = system.file("exdata/example_input.csv.gz",
                                              package = "calanusthreshold"),
                       form = c("tibble", "sf")[1],
                       crs = 4326){
  if (FALSE){
    filename = "/mnt/ecocast/projects/calanus/calanus-threshold/data/GSTS_Calanus_consolidated.csv"
  }
  x <- suppressMessages(readr::read_csv(filename))
  if (tolower(form[1]) == 'sf') {
    x <- sf::st_as_sf(x, wkt = "geometry", crs = crs) 
  } else {
    x <- dplyr::select(x, -dplyr::any_of("geometry"))
  }
  x
}

#' Write a dataset
#'
#' @param x sf or tibble input-style dataset
#' @param filename character, the file to write to
#' @return the provided sf or tibble
write_dataset <- function(x, filename = "dataset.csv.gz"){
  if (!inherits(x, 'sf')){
    isTibble <- TRUE
    x <- sf::st_as_sf(x, wkt = "geometry", crs = 4326)
  } else {
    isTibble <- FALSE
  }
  crs <- sf::st_crs(x)
  g <- sf::st_as_text(sf::st_geometry(x))
  y <- x |>
     sf::st_drop_geometry() |>
     dplyr::mutate(geometry = g) |>
     readr::write_csv(filename)
  if (!isTibble){
    return(invisible(x))
  } else {
    return(invisible(y))
  }
}
