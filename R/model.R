#' Model the data
#'
#' @export
#' @param x input
#' @param path character output path, if it doesn't exists the it is created
#' @return model
model <- function(x, path = "."){
  if (!dir.exists(path[1])) {
    ok <- dir.create(path[1], recursive = TRUE)
    if (!ok) stop("unable to create output path")
  }
  NULL
}
