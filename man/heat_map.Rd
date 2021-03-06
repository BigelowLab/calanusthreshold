% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf_mat.R
\name{heat_map}
\alias{heat_map}
\title{Create graphics for one or more confusion matrices}
\usage{
heat_map(x, title = NULL, title_col = 1, proportions = TRUE, annotate = TRUE)
}
\arguments{
\item{x}{\code{\link[yardstick]{conf_mat}} confusion matrix OR a tibble
with a conf_mat list column}

\item{title}{character or NULL, for the plot title}

\item{title_col}{character or integer, the column from which to draw the title when
providing a data frame of confusion matrices}

\item{proportions}{logical if TRUE annotate with proportions (as percentages)}

\item{annotate}{logical if TRUE annotate with a caption of stats (acc, sens, spec)}
}
\value{
ggplot object ala \code{\link[yardstick]{conf_mat}}
}
\description{
Create graphics for one or more confusion matrices
}
\examples{
\dontrun{
 library(dplyr)
 data("hpc_cv")
 cm <- dplyr::as_tibble(hpc_cv) |>
   dplyr::filter(Resample \%in\% sprintf("Fold\%0.2i", 1:4)) |>
   dplyr::group_by(Resample) |>
   yardstick::conf_mat(obs, pred)
 gg <- heat_map(cm)
 patchwork::wrap_plots(gg, ncol = 2)
}
}
