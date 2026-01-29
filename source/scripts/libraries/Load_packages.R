###Load/install all packages-------------------------------------------------------------------------------------------
#Add new packages here if necessary. The code will check and download new ones and load everything.
# Install the packages 
allPackages <- c(
  # General
  "DBI", "conflicted", "units",
  # INBO-packages
  "watina", "inbodb", "inbolims", "INBOtheme", 
  # Data wrangling
  "tidyverse", "glue", "lubridate", "santoku",
  # Spatial data
  "sf", "terra", 
  # Visualisation
  "ggforce", "patchwork", "tidyterra", "ggspatial", "ggnewscale", "ggsflabel", "maptiles", "scales",
  # external
  "writexl", "readxl", "officer", "flextable")


for (package in allPackages){
  if (!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::lag)
conflicts_prefer(readxl::read_xlsx)
conflicts_prefer(glue::trim)
conflicts_prefer(ggplot2::geom_sf_label)
conflicts_prefer(readxl::read_xlsx)

RoundAny = function(x, accuracy, f = round){
  # Rond een getal af op een in te geven cijfer.
  # x is het getal, accuracy de cijfer waarnaar het dient af te ronden
  # f de formule voor ceiling (dichtbijzijnde naar boven afgerond) of floor (dichtsbijzijnde naar onder afgerond).
  
  f(x/accuracy) * accuracy
}

NullVariable <- function(variable) {
  # Make NA variable NULL
  if (is.na(variable)) {
    variable <- NULL
  }
  
  return(variable)
}

CreatePath <- function(path, subfolder) {
  
  new_path <- str_c(path, subfolder, sep = "/")
  if (!file.exists(new_path)) {
    dir.create(new_path)
  }
  return(new_path)
}

