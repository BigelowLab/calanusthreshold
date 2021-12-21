library(calanusthreshold)
#library(biomod2)

x <- calanusthreshold::read_dataset(form = 'sf') %>%
  lump_vars(newname = "abund")


lonlat <- x |>
  sf::st_drop_geometry() |>
  dplyr::select(dplyr::all_of(c("longitude", "latitude"))) %>%
  as.matrix()

qs <- quantile(x$abund, 0.5)
patch_den <- as.numeric(x$abund > qs[1])

  
expl.var <- x |> 
  sf::st_drop_geometry() |>
  dplyr::select(-dplyr::all_of(c("station", "abund", "longitude","latitude")))


  
bd <- biomod2::BIOMOD_FormatingData(
  resp.var = patch_den,
  expl.var = expl.var,
  resp.xy = lonlat,
  resp.name = "pd"
)

rf <- biomod2::BIOMOD_Modeling(bd,
                               models = "RF")
