####
####
# Aim: Install and load packages required for analysis --------------------
####
####

# list CRAN packages used
pkgs = c("od", # generate OD lines
         "tidyverse", # for data cleaning
         "stringr", # for string manipulation
         "tidygeocoder", # OSM api used to calculate OD between corridors
         "hrbrthemes", # ggplot themes
         "sf",
         "tmap",
         "mapview") # simple feature handling

# Install pkgs not yet installed
installed_packages = pkgs %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}

# Load pkgs
invisible(lapply(pkgs, library, character.only = TRUE))
