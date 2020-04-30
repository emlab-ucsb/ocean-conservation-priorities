map_priorities <- function(ranking_raster, mpas_raster, ocean_df, caption = "", legend_title = "Top % of the Ocean", discrete,
                           rescale = F,
                           rank_breaks = c(0, 5, 10, 15, 20, 30,  40, 50, 100),
                           legend_key_widths = c(0.05, 0.05, rep(.1, 7))/1.5
                           ){
  
  ranking_df <- ranking_raster %>% 
    as.data.frame(xy = TRUE, centroids = T) %>% 
    set_names(c("lon", "lat", "rank")) %>% 
    mutate(rank = 100 - rank*100) %>% 
    filter(!is.na(rank))
  
  if(is.null(mpas_raster)){
    
    fraction_in_mpa <- 0
    
    } else{
      
    mpas_df <- mpas_raster %>% 
      as.data.frame(xy = TRUE, centroids = T) %>%
      set_names(c("lon", "lat", "mpa")) %>% 
      inner_join(ocean_df, by = c("lon" = "x", "lat" = "y")) %>% 
      as_tibble() %>% 
      filter(!is.na(mpa))
    
    fraction_in_mpa <- nrow(mpas_df)/nrow(ranking_df)
    
    ranking_df <- ranking_df %>% 
      left_join(mpas_df)
    
  }
  
  if(discrete == FALSE){
    
    priorities_map <- ranking_df %>% 
      ggplot()+
      geom_raster(aes(x = lon, y = lat, fill = rank))+
      scale_fill_viridis_c(direction = -1, breaks = c(0,10,20,30,40,50,100))+
      guides(fill = guide_colorbar(title = legend_title,
                                   frame.colour = "black", ticks.colour = "black", title.position =  "top"))
    
    if(!is.null(mpas_raster)){
      
      priorities_map <- priorities_map +
        geom_raster(data = mpas_df, aes(x = lon, y = lat), fill = "lightblue")
      
    }else if(is.null(mpas_raster)){
      
      priorities_map <- priorities_map 
      
    }
    
  } else if(discrete == TRUE){
    
    if(fraction_in_mpa > 0){
      
      max_mpa_rank <- max(ranking_df$rank[!is.na(ranking_df$mpa)])
      
      rank_breaks <- sort(unique(c(rank_breaks, max_mpa_rank)))
      
      rank_labels <- head(rank_breaks, -1)
      
      rank_palette <- rev(viridis::viridis(n = 8))
      
      rank_palette[rank_labels < max_mpa_rank] <- "lightblue"
      
      rank_palette[rank_labels >= max_mpa_rank] <- rev(viridis::viridis(n = 8))
      
      rank_labels[rank_labels ==  max_mpa_rank] <- " "

      if(rescale == TRUE){
        rescaled_rank_breaks <- as.numeric(raster::quantile(100 - 100*ranking_raster, probs = rank_breaks/100))
        
        priorities_map <- ranking_df %>% 
          mutate(rank = if_else(rank < 0, 0, rank)) %>% 
          mutate(rank_cut = cut(rank, 
                                breaks = rescaled_rank_breaks,
                                labels = rank_labels,
                                include.lowest = T)) %>%
          ggplot()+
          geom_raster(aes(x = lon, y = lat, fill = rank_cut))+
          geom_raster(data = mpas_df, aes(x = lon, y = lat), fill = "lightblue")+
          scale_fill_manual(values = rank_palette)+
          guides(fill = guide_legend(title = legend_title,
                                     direction = "horizontal",
                                     title.position = 'top',
                                     title.hjust = 0.5,
                                     keywidth = unit(legend_key_widths, "npc"),
                                     label.hjust = -.02,
                                     nrow = 1,
                                     byrow = T,
                                     label.position = "bottom"))
      }else if (rescale == FALSE){
        
        priorities_map <- ranking_df %>% 
          mutate(rank = if_else(rank < 0, 0, rank)) %>% 
          mutate(rank_cut = cut(rank, 
                                breaks = rank_breaks,
                                labels = rank_labels,
                                include.lowest = T)) %>%
          ggplot()+
          geom_raster(aes(x = lon, y = lat, fill = rank_cut))+
          geom_raster(data = mpas_df, aes(x = lon, y = lat), fill = "lightblue")+
          scale_fill_manual(values = rank_palette)+
          guides(fill = guide_legend(title = legend_title,
                                     direction = "horizontal",
                                     title.position = 'top',
                                     title.hjust = 0.5,
                                     keywidth = unit(legend_key_widths, "npc"),
                                     label.hjust = -.02,
                                     nrow = 1,
                                     byrow = T,
                                     label.position = "bottom"))
      }
      
      
    }else if(fraction_in_mpa == 0){
    
      priorities_map <- ranking_df %>% 
        mutate(rank = if_else(rank < 0, 0, rank)) %>% 
        mutate(rank_cut = cut(rank, 
                              breaks = rank_breaks,
                              labels = head(rank_breaks, -1),
                              include.lowest = T)) %>%
        ggplot()+
        geom_raster(aes(x = lon, y = lat, fill = rank_cut))+
        scale_fill_viridis_d(direction = -1)+
        guides(fill = guide_legend(title = legend_title,
                                   direction = "horizontal",
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = -.02,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))}
    }
  
  priorities_map <- priorities_map+
    geom_sf(data = land_50_moll, col = "transparent", fill = "grey", inherit.aes = FALSE)+
    my_theme_map()+
    labs(caption = caption)
  
  return(priorities_map)
}





