


#' Plot a confusion matrix or set of matrices
#' 
#' @export
#' @param x \code{\link[yardstick]{conf_mat}} confusion matrix OR a tibble
#'  with a conf_mat list column
#' @param title character or NULL, for the plot title
#' @param title_col character or integer, the column from which to draw the title when
#'   providing a data frame of confusion matrices
#' @param proportions logical if TRUE annotate with percentages
#' @return ggplot object ala \code{\link[yardstick]{conf_mat}}
#' @examples
#' \dontrun{
#'  library(dplyr)
#'  data("hpc_cv")
#'  cm <- dplyr::as_tibble(hpc_cv) |>
#'    dplyr::filter(Resample %in% sprintf("Fold%0.2i", 1:4)) |>
#'    dplyr::group_by(Resample) |>
#'    yardstick::conf_mat(obs, pred)
#'  gg <- heat_map(cm)
#'  patchwork::wrap_plots(gg, ncol = 2)
#' }
heat_map <- function(x, 
                     title = NULL,
                     title_col = 1,
                     proportions = TRUE) {
  
  if (inherits(x, "data.frame") && ("conf_mat" %in% colnames(x))){
    gg <- lapply(seq_len(nrow(x)),
                 function(i){
                   heat_map(x$conf_mat[[i]],,
                            title = x[[title_col]][i],
                            proportions = proportions)
                 }) |>
      rlang::set_names(x[[title_col]])
  } else {
    df <- as.data.frame.table(x$table) |>
      dplyr::mutate(Frac = .data$Freq/sum(.data$Freq) * 100)
    
    gg <- autoplot(x, type = 'heatmap')
    # Prediction Truth  Freq
    if (!is.null(title)) gg <- gg +  ggplot2::labs(title = title)
    
    if (proportions) gg <- gg +  
      ggplot2::geom_text(data = df, 
                         ggplot2::aes(label = sprintf("%1.0f%%", .data$Frac)), 
                                      vjust = 2, size=3)
  }
  return(gg)
}