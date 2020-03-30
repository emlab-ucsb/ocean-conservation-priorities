# Set data directories

emLab_project_dir <- "~/Google Drive File Stream/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

emLab_shrd_data_dir <- "~/Google Drive File Stream/Shared drives/emlab/data"

# Load ocean and land mask 

ocean_low_res_moll <- raster::raster(file.path(emLab_project_dir, "data", "02_processed", "masks", "ocean_low_res_moll.tif"), overwrite = T)

land_50_moll <- sf::st_read(file.path(emLab_project_dir, "data", "02_processed", "masks", "land_50_moll.shp"))

land_50 <- sf::st_read(file.path(emLab_project_dir, "data", "02_processed", "masks", "land_50.shp"))

# Basic mapping theme

my_theme_map <- function (base_size = 9, base_family = "") {
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.background = element_blank(), panel.border = element_blank(),
          panel.spacing = unit(0, "lines"), plot.background = element_blank(),
          panel.grid = element_line(color = "transparent"),
          legend.position = 'right', legend.margin = margin(t = 0, r = 0, b = 0, l = -.8, unit='cm'))
}
