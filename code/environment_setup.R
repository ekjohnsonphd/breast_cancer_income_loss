# Purpose: Set up the packages and set some objects for the analysis
Sys.setenv(lang = "en_US")

dkk_to_eur <- 0.134212 # exchange rate for 2023, from Danmkarks Statistics https://www.dst.dk/en/Statistik/emner/oekonomi/finansielle-markeder/valutakurser

pacman::p_load(data.table, tidyverse, arrow, danstat, forcats, parallel, cowplot)

load("data/supplemental_inputs/edu.RData")