###Load/install all packages -------------------
# Add new packages here if necessary.
# The code will download new ones and load everything.
all_packages <- c(
                  # General
                  "DBI", "conflicted", "units", "checklist",
                  # INBO-packages
                  "watina", "inbodb", "inbolims", "INBOtheme",
                  # Data wrangling
                  "tidyverse", "glue", "lubridate", "santoku",
                  # Spatial data
                  "sf", "terra",
                  # Visualization
                  "ggforce", "patchwork", "tidyterra", "ggspatial",
                  "ggnewscale",
                  "ggsflabel", "maptiles", "scales",
                  # external
                  "writexl", "readxl", "officer", "flextable")

for (package in all_packages) {
  if (!require(package, character.only = TRUE)) {
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
