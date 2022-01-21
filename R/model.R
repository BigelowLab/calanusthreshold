#' Extract the names of the predictor variables
#' 
#' @export
#' @param x model inheriting from \code{model_fit}
#' @return charcater vector of predictor names
model_predvars <- function(x){
  stopifnot(inherits(x, "model_fit"))
  a <- attr(x$preproc$terms, "factors")
  if (length(a > 0)) a <- colnames(a)
  a
}

#' Extract the names of the predictor and predicted variables
#' 
#' @export
#' @param x model inheriting from \code{model_fit}
#' @return list of \code{predicted} and \code{predictors} names
model_vars <- function(x){
  stopifnot(inherits(x, "model_fit"))
  a <- attr(x$preproc$terms, "factors")
  if (length(a > 0)) {
    predictors <- colnames(a)
    a <- list(
      predicted = setdiff(rownames(a), predictors),
      predictors = predictors
      )
  }
  a
}

#' Model the data for a given version configuration
#'
#' @export
#' @param cfg list, as per \code{\link{read_config}}
#' @param x input data set (as data.frame or tibble).  See \code{\link{read_dataset}}
#' @param save_model logical, if TRUE save the model, see \code{path}
#' @param save_pred logical, if TRUE save the prediction (made with testing data split), see \code{path}
#' @param save_summary logical, if TRUE save the summary (made with testing data split), see \code{path}
#' @param path character, output path, if it doesn't exists the it is created. Subdirectories
#'   are created for saving model, prediction, and summary
#' @param verbose logical, if TRUE output helpful messages
#' @return \code{\link[parsnip]{model_fit}} object or NULL
model_by_version <- function(cfg = read_config(system.file("exdata/cf.std.000.yaml",
                                                           package = "calanusthreshold")),
                             x = read_dataset(),
                             save_model = FALSE,
                             save_pred = FALSE,
                             save_summary = FALSE, 
                             path = ".",
                             verbose = interactive()){
  
  # development stuff
  if (FALSE){
    cfg = read_config(system.file("exdata/cf.std.000.yaml",
                                  package = "calanusthreshold"))
    x = read_dataset(project_path("orig", "GSTS_Calanus_consolidated.csv"))
    save_model = TRUE
    save_pred = TRUE
    path = "/Users/ben/Dropbox/code/projects/calanusthreshold/versions/categorical/cf"
    verbose = TRUE
  }
  
  #' This gets returned when there is an issue
  dummy_return <- function(version,
                           .metrics = c("accuracy", "kap", "roc_auc")){
    n <- length(.metrics)
    dplyr::tibble(
      version = rep(version, n),
      .metric = .metrics,
      .estimator = rep("binary", n),
      .estimate = rep(NA_real_, n)
    )
  }
  
  if ((save_model || save_pred || save_summary) && !dir.exists(path[1])) {
    ok <- dir.create(path[1], recursive = TRUE)
    if (!ok) stop("unable to create output path: ", path)
  }
  
  x <- dplyr::select(x, dplyr::contains(cfg$species), dplyr::all_of(cfg$predictors)) |>
    prep_dataset(lump_var = cfg$species,
                 drop_var = NULL,
                 threshold = cfg$threshold)
  
  set.seed(cfg$seed)
  x_split <- rsample::initial_split(x, prop = cfg$prop)
  set.seed(NULL)
  
  x_training_n <- dplyr::count(rsample::training(x_split), dplyr::all_of("patch"))
  if (nrow(x_training_n) < 2){
    # hack!
    # things fall apart when a class is absent in the training set 
    return(dummy_return(version = cfg$version))
  }
  
  x_recipe <- rsample::training(x_split) |>
    recipes::recipe(patch ~.) |>
    recipes::step_corr(recipes::all_predictors()) |>
    recipes::step_center(recipes::all_predictors(), -recipes::all_outcomes()) |>
    recipes::step_scale(recipes::all_predictors(), -recipes::all_outcomes()) |>
    recipes::prep()
  
  x_testing <- x_recipe |>
    recipes::bake(rsample::testing(x_split)) 
  
  x_training <- x_recipe |>
    recipes::bake(rsample::training(x_split))
  
  
  x_model <- parsnip::rand_forest(trees = cfg$trees, 
                                  mode = "classification",
                                  mtry = cfg$mtry) |>
      parsnip::set_engine(cfg$engine) |>
      parsnip::fit(patch ~ ., data = x_training)
    
  if (save_model){
    opath <- file.path(path, "model")
    if (!dir.exists(opath)) ok <- dir.create(opath, recursive = TRUE)
    ofile <- file.path(opath, paste0(cfg$version, ".Rdata"))
    save_model(x_model, filename = ofile, overwrite = TRUE)
  }
  
  x_pred <-  stats::predict(x_model, x_testing) |>
    dplyr::bind_cols(x_testing) |>
    dplyr::relocate(dplyr::all_of("patch"), .before = 1)
  
  x_metrics <- yardstick::metrics(x_pred , truth = .data$patch, estimate = .data$.pred_class)
  
  x_prob <-  stats::predict(x_model, x_testing, type = "prob") |>
    dplyr::bind_cols(x_testing) |>
    dplyr::relocate(dplyr::all_of("patch"), .before = 1)
  
  if (save_pred){
    opath <- file.path(path, "prediction")
    if (!dir.exists(opath)) ok <- dir.create(opath, recursive = TRUE)
    ofile <- file.path(opath, paste0(cfg$version, ".csv.gz"))
    write_pred(dplyr::mutate(x_prob, 
                             .pred_class = x_pred$.pred_class, 
                             .after = 1) , 
               ofile)
  }
  
  if (save_summary){
    opath <- file.path(path, "summary")
    if (!dir.exists(opath)) ok <- dir.create(opath, recursive = TRUE)
    ofile <- file.path(opath, paste0(cfg$version, ".csv"))
    
    x_auc <- yardstick::roc_auc(x_prob, .data$patch, .data$.pred_1, event_level = 'second')
    
    x_sens <- yardstick::sens(x_pred, truth = .data$patch, estimate = .data$.pred_class,
                              estimator = 'binary', event_level = 'second') |>
      dplyr::pull(dplyr::all_of(".estimate"))
    x_spec <- yardstick::spec(x_pred, truth = .data$patch, estimate = .data$.pred_class,
                              estimator = 'binary', event_level = 'second') |>
      dplyr::pull(dplyr::all_of(".estimate"))
    # TSS <- Sensitivity + Specificity - 1
    x_tss <- x_sens + x_spec - 1
    x_rmse <- yardstick::rmse(x_pred |> 
                                dplyr::mutate(truth = as.numeric(.data$patch), 
                                              estimate = as.numeric(.data$.pred_class)), 
                              truth = .data$truth, 
                              estimate = .data$estimate) |>
      dplyr::pull(dplyr::all_of(".estimate"))
    
    x_summa <- dplyr::tibble(version = rep(cfg$version, 3),
                  tss = rep(x_tss, 3),
                  rmse = rep(x_rmse, 3)) |>
      dplyr::bind_cols(dplyr::bind_rows(x_metrics, x_auc)) |>
      tidyr::pivot_wider(values_from = .data$.estimate, names_from = .data$.metric) |>
      readr::write_csv(ofile)
  }
  return(x_model)
}
