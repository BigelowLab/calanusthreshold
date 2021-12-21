library(dplyr)
calanusthreshold <- "/Users/ben/Dropbox/code/R/packages/calanusthreshold"
devtools::load_all(calanusthreshold)
#library(calanusthreshold)
library(tidymodels)

if (FALSE){
  calanusthreshold <- "/Users/ben/Dropbox/code/R/packages/calanusthreshold"
  devtools::document(calanusthreshold)
  devtools::install(calanusthreshold)
}


# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# https://www.tidymodels.org/

THRESHOLD<- c(2500, 5000, 7500, 10000)

# load the raw dataset
# remove variables we don't need
# aggregate the species as a new var called 'patch' 
# threshold patch (0/1)
# drop incomplete cases
x <- calanusthreshold::read_dataset("~/Downloads/GSTS_Calanus_consolidated.csv",
                                    form = 'tibble') |>
  dplyr::select(-dplyr::starts_with(c("Calanus glacialis", 
                                      "Calanus hyperboreus",
                                      "geom",
                                      "longitude",
                                      "latitude",
                                      "station",
                                      "year",
                                      "siconc",
                                      "sithick"))) |>
  na.omit() |>
  lump_vars(vars = c("Calanus finmarchicus"),
            selector = dplyr::starts_with,
            newname = "patch") |>
  dplyr::mutate(patch = calanusthreshold::as_patch(patch))

# divide into test/train
x_split <- rsample::initial_split(x, prop = 0.6)

# normalize and cenetr the data
x_recipe <- rsample::training(x_split) %>%
  recipes::recipe(patch ~.) %>%
  recipes::step_corr(recipes::all_predictors()) %>%
  recipes::step_center(recipes::all_predictors(), -recipes::all_outcomes()) %>%
  recipes::step_scale(recipes::all_predictors(), -recipes::all_outcomes()) %>%
  recipes::prep()

# no clue
x_testing <- x_recipe %>%
  recipes::bake(rsample::testing(x_split)) 


x_training <- recipes::juice(x_recipe)


x_ranger <- parsnip::rand_forest(trees = 100, mode = "classification") %>%
  parsnip::set_engine("ranger") %>%
  parsnip::fit(patch ~ ., data = x_training)

x_rf <-  parsnip::rand_forest(trees = 100, mode = "classification") %>%
  parsnip::set_engine("randomForest") %>%
  parsnip::fit(patch ~ ., data = x_training)


x_ranger_p <- predict(x_ranger, x_testing)

x_ranger %>%
  predict(x_testing) %>%
  bind_cols(x_testing) %>%
  glimpse()


x_ranger %>%
  predict(x_testing) %>%
  bind_cols(x_testing) %>%
  metrics(truth = patch, estimate = .pred_class)


x_rf %>%
  predict(x_testing) %>%
  bind_cols(x_testing) %>%
  metrics(truth = patch, estimate = .pred_class)

x_ranger %>%
  predict(x_testing, type = "prob") %>%
  glimpse()


x_probs <- x_ranger %>%
  predict(x_testing, type = "prob") %>%
  bind_cols(x_testing)


x_probs %>%
  gain_curve(patch, .pred_1) %>%
  glimpse()

x_probs %>%
  gain_curve(patch, .pred_1) %>%
  autoplot()

x_probs %>%
  roc_curve(patch, .pred_1) %>%
  autoplot()
