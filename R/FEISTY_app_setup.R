
## Script name: FEISTY app setup
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: April 2022
## Last update:  July 2022
##
## ---------------------------
##
## Readme:
##
## This script calls on basic functions and packages to run FEISTY in a shiny app
##
## ---------------------------


#####################################################################
# Load libraries:
require(deSolve)
require(tidyverse)
require(ggthemes)
require(scales)
require(patchwork)
library(shiny)

# Load functions:
source("shiny_parameters.R")  
source("feisty.R") 
source("plot_weight.R") 
source("plot_network.R") 
source("plotdiet.R")

# # Define Palette colors: 
# my_palette <- c("Res" = "#F57C0D",
#                 "SZoo" = "#FFEE58",
#                 "BZoo" = "#F9A825",
#                 "Bent" = "#795548",
#                 "Spel" = "#BBDEFB",
#                 "Meso" = "#9E9E9E",
#                 "Lpel" = "#2196F3",
#                 "Dem" = "#000000", 
#                 "Squid" = "#AD1457", 
#                 "Bpel" =  "#0D47A1")


#                                          END OF SCRIPT
#############################################################################################################