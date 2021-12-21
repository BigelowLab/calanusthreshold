#' Lump variables in a table together by summing
#' 
#' @export
#' @param x tibble
#' @param vars character, one or more variables to lump
#' @param selector function the function used to select the variables,
#'   see \code{tidyselect} helpers.
#' @param newname character, the name of the new (lumped) columns
#' @param na.rm logical, if TRUE lump after removing NA values
#' @param drop logical, if TRUE drop the \code{vars}
#' @return tibble
lump_vars <- function(x = read_dataset(),
                      vars = c("Calanus finmarchicus IV", 
                               "Calanus finmarchicus V", 
                               "Calanus finmarchicus VI", 
                               "Calanus hyperboreus IV", 
                               "Calanus hyperboreus V", 
                               "Calanus hyperboreus VI", 
                               "Calanus glacialis IV", 
                               "Calanus glacialis V", 
                               "Calanus glacialis VI"),
                      selector = dplyr::all_of,
                      newname = "lumped",
                      na.rm = TRUE, 
                      drop = TRUE){

  if (inherits(x, "sf")){
    lumped <- x |>
      sf::st_drop_geometry() |>
      dplyr::select(selector(vars)) |>
      rowSums(na.rm = na.rm)
  } else {
    lumped <- x |>
      dplyr::select(selector(vars)) |>
      rowSums(na.rm = na.rm)
  }
  
  x <- x |>
    dplyr::mutate(!!newname := lumped)
  
  if (drop){
    x <- x |> 
    dplyr::select(-selector(vars))
  }
  x
}


#' Convert abundance to patch density
#' 
#' @export
#' @param x a vector of abundances
#' @param threshold numeric boundaries defining levels of patchiness from low to high.
#'  If zero is not the first boundary it is added so that the first patch index
#'  is always 0 (implying no patch of meaningful density)
#' @param form character, one of 'index' of 'factor' to control the output type
#'   in either case NAs are propagated.
#' @return numeric indices into threshold, possibly as factors
as_patch <- function(x, 
                     threshold = list(10000, c(2500, 5000, 7500, 10000))[[1]],
                     form = c("index", "factor")[2]){
  if (threshold[1] > 0) threshold = c(0, threshold)
  ix <- findInterval(x, threshold) - 1
  if (tolower(form[1]) == "factor") {
    ix <- factor(ix, levels = seq(from = 0L, by = 1L, length = length(threshold)))
  }
  ix
}


#' Given a dataset, create a subsample and possibly anonymize ID and location
#'
#' @export
#' @param x sf or tibble input dataset
#' @param n the number of subsamples, or NA to use all
#' @param anonymize logical, if TRUE anonymize the data
#' @param complete_cases_only logical, if TRUE 
#' @param skip_for_complete character or NULL, if present then drop these columns before
#'   determing complete cases.  Note that these variables are still included in the 
#'   output.
#' @param noise numeric, fraction of noise to add to each numeric variable (column)
#' @return sf or tibble subset, possibly anonymized
subsample_dataset <- function(x,
                              n = c(NA, 500)[2],
                              anonymize = TRUE,
                              complete_cases_only = TRUE,
                              skip_for_complete = c("siconc", "sithick"),
                              noise = 0.2){
  if (inherits(x, 'sf')){
    isSF <- TRUE
    crs <- sf::st_crs(x)
    x <- sf::st_drop_geometry(x)
  } else {
    isSF <- FALSE
  }
  
  if (complete_cases_only){
    if (length(skip_for_complete) > 0) {
      ix <- dplyr::select(x, -dplyr::all_of(skip_for_complete)) |>
        complete.cases()
    } else {
      ix <- complete.cases(x)
    }
    x <- dplyr::filter(x, ix)
  }
  
  if (!is.na(n[1])) x <- dplyr::slice_sample(x, n = n[1])
  if (anonymize){
    id <- unique(x$station)
    uid <- as.vector(outer(LETTERS, sprintf("%.02i", seq_along(LETTERS)), paste, sep = "-"))[seq_along(id)] |>
      rlang::set_names(id)
    x <- x |> dplyr::mutate(station = uid[.data$station])
    
    klass <- sapply(x, function(x) paste(class(x), collapse = ","))
    N <- nrow(x)
    
    # given a vector and min/max values truncate the vector such that mn <= x <= mx
    trunc_range <- function(x, mn, mx){
      x[x < mn] <- mn
      x[x > mx] <- mx
      x
    }
    for (nm in names(klass)){
      if (klass[[nm]] == "numeric"){
        r <- range(x[[nm]], na.rm = TRUE)
        amount <- noise * (r[2] - r[1])
        # we allow lon/lat to range more freely
        if (nm == "latitude"){
          r <- c(-90, 90)
        }
        if (nm == "longitude"){
          r <- c(-180, 180)
        }
        
        if (nm %in% c("year", "month")){
          n <- nrow(x)
          ix <- sample(seq_len(n), n, replace = FALSE)
          x[[nm]] <- x[[nm]][ix]
        } else {
          x[[nm]] <- trunc_range(jitter(x[[nm]], amount = amount), r[1], r[2])
        }      
      }
    }
  } # anonymize
  if (isSF){
    xy <- as.matrix(x |> dplyr::select("longitude", "latitude"))
    sfc <- lapply(seq_len(nrow(xy)),
                  function(i) sf::st_point(xy[i,])) |>
      sf::st_sfc(crs = crs)
    x <- sf::st_set_geometry(x, sfc)
  }
  x
}
