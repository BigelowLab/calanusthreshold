#' Read an input file source
#'
#' @export
#' @param filename character, the file to read. The default is to read a dummy example data file.
#' @param form character, either 'tibble' or 'sf' (default, simple feature)
#' @param crs character or numeric, if \code{form} is 'sf' then pass this to
#'   \code{\link[sf]{st_as_sf}}
#' @return tibble or simple feature
read_input <- function(filename = system.file("exdata/example_input.csv.gz",
                                              package = "calanusthreshold"),
                       form = c("tibble", "sf")[2],
                       crs = 4326){
  if (FALSE){
    filename = "/mnt/ecocast/projects/calanus/calanus-threshold/GSTS_Calanus_consolidated.csv"
  }
  x <- suppressMessages(readr::read_csv(filename))
  if (tolower(form[1]) == 'sf')  x <- sf::st_as_sf(x, wkt = "geometry", crs = crs)
}
