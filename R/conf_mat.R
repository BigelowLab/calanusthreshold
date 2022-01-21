#' Create graphics for one or more confusion matrices
#' 
#' @export
#' @param x \code{\link[yardstick]{conf_mat}} confusion matrix OR a tibble
#'  with a conf_mat list column
#' @param title character or NULL, for the plot title
#' @param title_col character or integer, the column from which to draw the title when
#'   providing a data frame of confusion matrices
#' @param proportions logical if TRUE annotate with proportions (as percentages)
#' @param annotate logical if TRUE annotate with a caption of stats (acc, sens, spec)
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
                     proportions = TRUE,
                     annotate = TRUE) {
  
  if (inherits(x, "data.frame") && ("conf_mat" %in% colnames(x))){
    gg <- lapply(seq_len(nrow(x)),
                 function(i){
                   heat_map(x$conf_mat[[i]],
                            title = x[[title_col]][i],
                            annotate = annotate,
                            proportions = proportions)
                 }) |>
      rlang::set_names(x[[title_col]])
  } else {
    df <- as.data.frame.table(x$table) |>
      dplyr::mutate(Frac = .data$Freq/sum(.data$Freq) * 100)
    
    gg <- ggplot2::autoplot(x, type = 'heatmap')
    
    if (annotate){
      st <- summary(x, event_level = "second")
      s <- st$.estimate |>
        rlang::set_names(st$.metric)
      ann <- sprintf("acc: %0.3f  sens: %0.3f  spec: %0.3f",
                     s[["accuracy"]], s[['sens']], s[['spec']])
    } else {
      ann <- ggplot2::waiver()
    }
    
    if (is.null(title)) title <- ggplot2::waiver()
    
    gg <- gg + 
      ggplot2::labs( title = title, caption = ann) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
            plot.caption = ggplot2::element_text(hjust = 0.5))
  
    if (proportions) gg <- gg +  
      ggplot2::geom_text(data = df, 
                         ggplot2::aes(label = sprintf("%1.0f%%", .data$Frac)), 
                                      vjust = 2, size = 3)
  }
  return(gg)
}