#Function to create map of a continuous spatial raster layer overlayed onto the BC Map
RC_Map <- function(RC_Layer, Legend_Label, layername, BC){
  RC_Plot <- ggplot(BC) + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + 
    geom_raster(data = RC_Layer, mapping = aes(x = x, y=y, fill = .data[[layername]])) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000))  + annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    scale_fill_continuous(type = "viridis", na.value = NA, name = Legend_Label , limits = c((round(minmax(RC_Layer)[1], digits = 2)-0.01),(round(minmax(RC_Layer)[2], digits = 2)+0.01)), breaks = c(round(minmax(RC_Layer)[1], digits = 2), round((minmax(RC_Layer)[1]+0.25*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1])), digits = 2),round((minmax(RC_Layer)[1]+0.5*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1])), digits = 2),round((minmax(RC_Layer)[1]+0.75*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1])), digits = 2), round(minmax(RC_Layer)[2], digits = 2)), labels = c(as.character(round(minmax(RC_Layer)[1], digits = 2)), as.character(round(minmax(RC_Layer)[1]+0.25*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1]), digits = 2)),as.character(round(minmax(RC_Layer)[1]+0.5*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1]), digits = 2)),as.character(round(minmax(RC_Layer)[1]+0.75*(minmax(RC_Layer)[2]-minmax(RC_Layer)[1]), digits = 2)), as.character(round(minmax(RC_Layer)[2], digits = 2)))) + 
    xlab(element_blank()) + ylab(element_blank()) + 
    theme(legend.position = c(1,1), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, legend.justification = c("right","top"), legend.box.just = "right") + 
    theme(panel.background = element_rect(fill = "white"))
  return(RC_Plot)
}

#Function to create a map of a discrete (2-value) raster layer overlayed onto the BC Map
RD_Map_GUR <- function(RD_Layer, Legend_Label, BC){
  Kelp_Points <- terra::as.points(RD_Layer)
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry() %>% dplyr::filter(meanw == 1)
  RD_Plot <- ggplot(BC) + geom_sf() + geom_raster(data = RD_Layer, mapping = aes(x = x, y=y, fill = meanw)) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000))  + annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    scale_fill_manual(values = c(NA,"#55C667FF"), na.value = NA, name = element_blank(), labels = c(Legend_Label,""), na.translate = F) + 
    xlab(element_blank()) + ylab(element_blank()) + 
    theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0) + 
    theme(panel.background = element_rect(fill = "white"))+ 
    geom_point(data = Kelp_df, aes(x=x, y=y), color = "#55C667FF", size = 0.1, alpha = 0)
  RD_Plot_Hist <- ggExtra::ggMarginal(RD_Plot, type = "histogram", fill = "#55C667FF", bins = 50)
  return(RD_Plot_Hist)
}

RD_Map_PUR <- function(RD_Layer, Legend_Label, BC){
  Kelp_Points <- terra::as.points(RD_Layer)
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry() %>% dplyr::filter(meanw == 1)
  RD_Plot <- ggplot(BC) + geom_sf() + geom_raster(data = RD_Layer, mapping = aes(x = x, y=y, fill = meanw)) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000))  + annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    scale_fill_manual(values = c(NA,"#a503fc"), na.value = NA, name = element_blank(), labels = c(Legend_Label,""), na.translate = F) + 
    xlab(element_blank()) + ylab(element_blank()) + 
    theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0) + 
    theme(panel.background = element_rect(fill = "white"))+ 
    geom_point(data = Kelp_df, aes(x=x, y=y), color = "#a503fc", size = 0.1, alpha = 0)
  RD_Plot_Hist <- ggExtra::ggMarginal(RD_Plot, type = "histogram", fill = "#a503fc", bins = 50)
  return(RD_Plot_Hist)
}

RD_Map_RUR <- function(RD_Layer, Legend_Label, BC){
  Kelp_Points <- terra::as.points(RD_Layer)
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry() %>% dplyr::filter(meanw == 1)
  RD_Plot <- ggplot(BC) + geom_sf() + geom_raster(data = RD_Layer, mapping = aes(x = x, y=y, fill = meanw)) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000))  + annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    scale_fill_manual(values = c(NA,"#e6360F"), na.value = NA, name = element_blank(), labels = c(Legend_Label,""), na.translate = F) + 
    xlab(element_blank()) + ylab(element_blank()) + 
    theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0) + 
    theme(panel.background = element_rect(fill = "white"))+ 
    geom_point(data = Kelp_df, aes(x=x, y=y), color = "#e6360F", size = 0.1, alpha = 0)
  RD_Plot_Hist <- ggExtra::ggMarginal(RD_Plot, type = "histogram", fill = "#e6360F", bins = 50)
  return(RD_Plot_Hist)
}

