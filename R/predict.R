#' Predict using a model
#' 
#' @export
#' @param model either a 'model_fit' object or a file description to one
#' @param predictors a table (data.frame or tibble) or raster (terra or stars)
#' @param include_prob logical, if TRUE include probabilities of occurrence
#' @param ... arguments for \code{predict}
#' @return prediction as table (tibble) or stars raster
predict_calanus <- function(model,
                            predictors,
                            include_prob = TRUE,
                            ...){
  
  if (inherits(model, "character")) {
    model <- tryCatch(read_model(model), 
                      error = function(e){
                        print(e)
                        stop("error reading model:", model)
                      })
  }
  
  if (inherits(predictors, "data.frame")){
    r <- predict_table(model, predictors, include_prob = include_prob, ...)
  } else {
    r <- predict_raster(model, predictors, include_prob = include_prob, ...)
  }
  
  r
}

#' Predict using tibble class object
#' 
#' @export
#' @param model either a 'model_fit' object or a file description to one
#' @param predictors a table (data.frame or tibble) or raster (terra or stars)
#' @param include_prob logical, if TRUE include probabilities of occurrence
#' @param ... arguments passed to \code{predict}
#' @return tibble of predictions, possibly with prob
predict_table <- function(model, predictors, include_prob = TRUE, ...){
  
  as_numeric <- function(f) as.numeric(as.character(f))
  
  vm <- model_vars(model)
  vp <- names(predictors)

  if (!setequal(vp,vm$predictors)) stop(sprintf("models vars (%s) not the same as predictor vars (%s)",
                                 paste(vm, collapse = ","), 
                                 paste(vp, collapse = ","))  )
    
  event_var <- vm$predicted
  ix <- complete.cases(predictors)
  r <- dplyr::tibble(!!event_var := rep(NA_real_, nrow(predictors)))
  if (any(ix)){  # be careful!  What if they are all missing?
    event <- stats::predict(model, predictors |> dplyr::filter(ix))
    r[[event_var]][which(ix)] <-  as_numeric(dplyr::pull(event, .data$.pred_class))
  } 
  
  if (include_prob){
    r <- dplyr::mutate(r, prob = rep(NA_real_, nrow(predictors)))
    if (any(ix)){
      prob <- stats::predict(model, predictors |> dplyr::filter(ix), type = 'prob')
      r$prob[which(ix)] <- dplyr::pull(prob, .data$.pred_1)
    } 
  }
  r
}

#' Predict using stars class raster
#' 
#' @param model either a 'model_fit' object or a file description to one
#' @param predictors a table (data.frame or tibble) or raster (terra or stars)
#' @param include_prob logical, if TRUE include probabilities of occurrence
#' @param month numeric or NA, if not NA then add a layer with value equal to this month number
#' @param drop logical, if TRUE drop unused layers in the predictors
#' @param ... arguments passed to \code{predict}
#' @return stars object with one or two layers (see \code{include_prob})
predict_raster <- function(model, predictors = read_raster(), include_prob = TRUE, 
                           month = NA, drop = TRUE, ...){
  
  if (!is.na(month)) predictors <- dplyr::mutate(predictors, month = month)
  
  if (length(dim(predictors)) > 2) {
    p <- split(predictors, "band") |>
      dplyr::as_tibble() 
  } else {
    p <- dplyr::as_tibble(predictors)
  }
  p <- dplyr::select(p, -dplyr::any_of(c("x", "y", "X", "Y",
                                         "lon", "longitude", "lng",
                                         "lat", "latitude")))
  
  mv <- model_vars(model)
  if (drop){
    p <- dplyr::select(p,  dplyr::all_of(mv$predictors))
  }
  
  r <- predict_table(model, p, include_prob = include_prob, ...)
  
  R <- predictors 
  for (nm in colnames(r)) R <- R |> dplyr::mutate(!!nm := r[[nm]])
  dplyr::select(R, -dplyr::any_of(names(predictors)))
}