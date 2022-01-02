#' Prep a dataset by removing variables, remove incomplete cases,
#'  aggregating specified columns, and applying threshold/labeling to the aggregated column
#'  
#' @export
#' @param x tibble of input data
#' @param drop_var character, the variables to deselect
#' @param drop_fun function, such as \code{\link[dplyr]{starts_with}}
#' @param complete_cases_only logical, if TRUE drop incomplete records (after deselection)
#' @param lump_var character character, vector of variables to sum into a new column
#' @param lump_fun function, such as \code{\link[dplyr]{starts_with}}
#' @param log_var character, variables to log scale (base 10), match with \code{\link[dplyr]{any_of}}
#' @param newname character the name of the new aggregated colum
#' @param threshold numeric, the threshold(s) used to define patches
#' @return a tibble cleaned and transformed for downstream processing
prep_dataset <- function(x = read_dataset(),
                         complete_cases_only = TRUE,
                         lump_var = "Calanus finmarchicus",
                         lump_fun = dplyr::starts_with,
                         drop_var = c("geometry",
                                      "longitude",
                                      "latitude",
                                      "station",
                                      "year",
                                      "siconc",
                                      "sithick",
                                      complement_species(lump_var)),
                         drop_fun = dplyr::starts_with,
                         log_var = c("bathymetry", "chlor_a"),
                         newname = "patch",
                         threshold = 10000){
  
  if (length(drop_var) > 0) x <- dplyr::select(x, -drop_fun(drop_var)) 
  
  if(complete_cases_only) x <- na.omit(x)

  # log scale as requested
  if (length(log_var) > 0) {
    x <- dplyr::mutate(x, dplyr::across(dplyr::any_of(log_var),  ~ log10(abs(.x) + 0.00001) ))
  }
  
  # aggregate the species
  x <- calanusthreshold::lump_vars(x, 
                                  vars = lump_var,
                                  selector = lump_fun,
                                  newname = newname)
  # now apply threshold
  x[[newname]] <- calanusthreshold::as_patch(x[[newname]], threshold) 
  x
}

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
#'   Values in \code{x} below the lowest threshold are considered "no-patch"
#' @param form character, one of 'index' of 'factor' to control the output type
#'   in either case NAs are propagated.
#' @return numeric indices into threshold, possibly as factors.  Zero means no patch.
as_patch <- function(x, 
                     threshold = list(10000, c(2500, 5000, 7500, 10000))[[1]],
                     form = c("index", "factor")[2]){

  ix <- findInterval(x, threshold)
  if (tolower(form[1]) == "factor") {
    ix <- factor(ix, levels = seq(from = 0L, by = 1L, length = length(threshold)+1))
  }
  ix
}
