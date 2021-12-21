library(dplyr)
calanusthreshold <- "/Users/ben/Dropbox/code/R/packages/calanusthreshold"
devtools::load_all(calanusthreshold)

x <- read_dataset(filename = "~/Downloads/GSTS_Calanus_consolidated.csv",
                  form = "tibble")

N <- nrow(x)
na <- sapply(x, function(x) sum(is.na(x))/N)
