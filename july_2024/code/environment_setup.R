setwd("Emily/")

Sys.setenv(lang = "en_US")

dkk_to_eur <- 0.134212

pacman::p_load(data.table, tidyverse, arrow, danstat, forcats, parallel, cowplot)

load("data/supplemental_inputs/edu.RData")