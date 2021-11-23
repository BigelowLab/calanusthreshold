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


#' Given an input dataset, create a subsample and possibly anonymize ID and location
#'
#' @param x sf or tibble input dataset
#' @param n the number of subsamples, or NA to use all
#' @param anonymize logical, if TRUE anonymize the data
#' @param noise numeric, fraction of noise to add to each numeric variable (column)
#' @return sf or tibble subset, possibly anonymized
create_subsample <- function(x,
                             n = c(NA, 500)[2],
                             anonymize = TRUE,
                             noise = 0.2){
  if (!inherits(x, 'sf')){
    isTibble <- TRUE
    x <- sf::st_as_sf(x, wkt = "geometry", crs = 4326)
  }

  if (!is.na(n[1])) x <- dplyr::slice_sample(x, n = n[1])
  if (anonymize){
    id <- unique(x$station)
    uid <- as.vector(outer(LETTERS, sprintf("%.02i", seq_along(LETTERS)), paste, sep = "-"))[seq_along(id)] |>
      rlang::set_names(id)
    x <- x |> dplyr::mutate(station = uid[.data$station])

    klass <- sapply(x, function(x) paste(class(x), collapse = ","))
    N <- nrow(x)
    for (nm in names(klass)){
      if (klass[[nm]] == "numeric"){
        r <- range(x[[nm]], na.rm = TRUE)
        amount <- noise * (r[2] - r[1])
        x[[nm]] <- jitter(x[[nm]], amount = amount)

      }
    }
    xy <- as.matrix(x |> sf::st_drop_geometry() |> dplyr::select("longitude", "latitude"))
    sfc <- lapply(seq_len(nrow(xy)),
                 function(i) sf::st_point(xy[i,])) |>
      sf::st_sfc(crs = sf::st_crs(x))
    x <- x |>
      sf::st_set_geometry(sfc)
  } # anonymize
  if (isTibble){
    g <- sf::st_as_text(sf::st_geometry(x))
    x <- x |>
      sf::st_drop_geometry() |>
      dplyr::mutate(geometry = g)
  }
  x
}
