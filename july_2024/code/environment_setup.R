setwd("Emily/")

Sys.setenv(lang = "en_US")

dkk_to_eur <- 0.134212

pacman::p_load(data.table, tidyverse, arrow, danstat, forcats, parallel, cowplot)

load("data/supplemental_inputs/edu.RData")
# # Functions
archive_path <- function(path) {
  if (file.exists(path)) {
    dir_path <- dirname(path)
    base_name <- basename(path)

    archive_path <- str_replace(dir_path, "data", paste0("data/_archive/", Sys.Date()))
    dir.create(archive_path, showWarnings = FALSE)

    if (file.info(path)$isdir) {
      dir.create(paste0(archive_path, base_name))
      lapply(Sys.glob(paste0(path, "/*")), function(file) {
        file_base_name <- basename(file)
        file.copy(file, file.path(archive_path, base_name))
      })
    } else {
      file.copy(path, file.path(archive_path, base_name))
    }
    return(paste0("Content archived in ", archive_path))
  } else {
    return("File does not exist")
  }
}


theme_ej <- function() {
  theme_half_open() +
    panel_border() +
    background_grid() +
    theme( # panel.grid.major = element_line(colour = "gray90"),
      axis.title = element_text(hjust = 1),
      panel.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      legend.position = "bottom", legend.direction = "horizontal"
    )
}

ej_colors <- c(
  "#1985A1", "#A5B452", "#C1292E", "#191716", "#BFB6BB", "#D65108",
  "#EF476F", "#2A2A72", "#758E4F"
)
ej_4colors <- c("#1985A1", "#A5B452", "#C1292E", "#D65108")
