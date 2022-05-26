# Set data directories

emLab_project_dir <- "~/Google Drive/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

emLab_project_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/ocean-conservation-priorities/"

emLab_shrd_data_dir <- "~/Google Drive/Shared drives/emlab/data"

emLab_shrd_data_dir <- "/Volumes/GoogleDrive/Shared\ drives/emlab/data"

# Load ocean and land mask 

ocean_low_res_moll <- raster::raster(file.path(emLab_project_dir, "data", "02_processed", "masks", "ocean_low_res_moll.tif"), overwrite = T)

ocean_low_res_wgs84 <- raster::raster(file.path(emLab_project_dir, "data", "02_processed", "masks", "ocean_low_res_wgs84.tif"), overwrite = T)

land_50_moll <- sf::st_read(file.path(emLab_project_dir, "data", "02_processed", "masks", "land_50_moll.shp"))

land_50_moll_raster <- raster::raster(file.path(emLab_project_dir, "data", "02_processed", "masks", "land_low_res_moll.tif"))

land_50 <- sf::st_read(file.path(emLab_project_dir, "data", "02_processed", "masks", "land_50.shp"))

# Ocean matrix

ocean_matrix <- ocean_low_res_moll %>% 
  raster::as.data.frame(xy = T) %>% 
  filter(ocean_low_res_moll == 1) %>% 
  mutate(cell_id = as.character(group_indices(., x, y))) %>% 
  select(-ocean_low_res_moll) %>% 
  as_tibble()

# Basic mapping theme

my_theme_map <- function(base_size = 12, base_family = "") {
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          panel.spacing = unit(0, "lines"), 
          panel.grid = element_line(color = "transparent"),
          plot.background = element_blank(),
          legend.position = 'bottom', 
          legend.text = element_text(size = 10),
          legend.text.align = 0,
          legend.key.height = unit(0.01, 'npc'),
          legend.key.width = unit(0.05, 'npc'),
          legend.background =  element_blank(),
          legend.title = element_text(hjust = 0.5),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ='cm'))
}

# Priorities mapping palette

priorities_map_pal <- list(breaks = c(0, 0.2, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 1),
                           labels = c("0-20", "20-50", "50-70", "70-80", "80-85", "85-90", "90-95", "95-100"),
                           colors = c("#4575b4", "#74add1", "#abd9e9", "#e0f3f8" ,"#fee090" ,"#fdae61" ,"#f46d43", "#d73027"))
