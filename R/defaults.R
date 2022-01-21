#' The known predictor variables.
#' 
#' Year and station ID and location are disallowed.
#' Sea ice data (siconc, sithick) are too incomplete to train on. 
#' 
#' @export
#' @return character vector of allowed predictor variables
known_predictors <- function(){
  c("month", 
    "bathymetry", "slope", "aspect", "roughness", "proximity", "mlotst", 
    "thetao", "usi", "bottomT", "vsi", "vo", 
    "uo", "so", "zos", "chlor_a")
}

#' The known species groups
#' 
#' @export
#' @param stage logical if FALSE drop the stage information
#' @return character vector of known species names
known_species <- function(stage = FALSE){
  x <- c("Calanus finmarchicus IV", "Calanus finmarchicus V", "Calanus finmarchicus VI", 
         "Calanus hyperboreus IV", "Calanus hyperboreus V", "Calanus hyperboreus VI", 
         "Calanus glacialis IV", "Calanus glacialis V", "Calanus glacialis VI")
  if (!stage){
    ix <- sapply(gregexpr(" ", x, fixed = TRUE), function(s) {s[length(s)]})
    x <- substring(x, 1, ix-1) |>
      unique()
  }
  x
} 

#' Given a set of species, determine the other species
#' 
#' @export
#' @param species character, one or more species
#' @param known character, vector of known species
#' @return character vector, possibly empty, of the other species not identifed
complement_species <- function(species = "Calanus finmarchicus",
                               known = known_species()){
  known[!(known %in% species)]
}


#' Retrieve a configuration path
#'
#' @export
#' @param ... character, file path segments passed to \code{file.path}
#' @param root character, the root configuration path
#' @return file path
config_path <- function(..., 
                        root = rappdirs::user_config_dir(appname = "calanusthreshold")){
  if (!dir.exists(root[1])){
    ok <- dir.create(root[1], recursive = TRUE)
    if(!dir.exists(root[1])) stop("unable to create directory:", root[1])
  }
  file.path(root, ...)
}

#' Set the data path as a configuration
#' 
#' @export
#' @param path character, the path to the project data
#' @param ... other arguments for \code{config_path}
#' @return logical with TRUE for success
set_project_path <- function(path = ".", ...){
  cpath <- config_path("project_path.txt", ...)
  cat(path, sep = "\n", file = cpath)
  invisible(file.exists(cpath))
}

#' Retrieve the project path
#' 
#' @export
#' @param filename character, the name of the filename with the config path
#' @param ... other arguments for \code{config_path}
#' @return the filepath to the project
get_project_path <- function(filename = "project_path.txt", ...){
  cpath <- config_path("project_path.txt", ...)
  if (!file.exists(cpath[1])) stop("config path not found:", cpath[1])
  readLines(cpath[1])
}


#' Retrieve project data path
#' 
#' @export
#' @param ... character, file path segments for file/directories relative to \code{root}
#'   See \code{\link[base]{file.path}} 
#' @param root character, the root project path 
#' @return a fully qualified path
project_path <- function(..., root = get_project_path()){
  file.path(root[1], ...)
}