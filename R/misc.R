#' Lump variables in a table together
#' 
#' @export
#' @param x tibble
#' @param vars character, one or more variables to lump
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
                      newname = "lumped",
                      na.rm = TRUE, 
                      drop = TRUE){

  if (inherits(x, 'sf')){
    lumped <- x |>
      sf::st_drop_geometry() |>
      dplyr::select(dplyr::all_of(vars)) |>
      rowSums(na.rm = na.rm)
  } else {
    lumped <- x |>
      dplyr::select(dplyr::all_of(vars)) |>
      rowSums(na.rm = na.rm)
  }
  x <- x |>
    dplyr::mutate(!!newname := lumped)
  
  if (drop){
    x <- x |> 
    dplyr::select(-dplyr::all_of(vars))
  }
  
  x
}