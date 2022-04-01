if (!require("pacman")) install.packages("pacman")
if (!require("BiocManager")) install.packages("BiocManager")
library(pacman)
library(BiocManager)

workshop_packages <- c("here", "yaml",  "tidyverse", "scales", "devtools",
                       "RSocrata", "tidygeocoder",
                       "AirSensor", "MazamaSpatialUtils", "PWFSLSmoke")
pacman::p_install(workshop_packages)

library(devtools)
devtools::install_github('mazamascience/AirMonitorPlots', build_vignettes=TRUE)
