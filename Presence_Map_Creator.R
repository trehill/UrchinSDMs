#This function maps out point locations for species presence data given the input
#the points and a land shapefile

Pres_Map_GUR <- function(BC, Kelp_Points){
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
  #Create the map
  Map_Plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y), color = "#55C667FF", size = 0.8) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
    annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    theme(legend.position = c(0.4,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size =10)) + 
    theme(panel.background = element_rect(fill = "white")) ##878c8c used to be fill 
  return(Map_Plot)
}

Pres_Map_PUR <- function(BC, Kelp_Points){
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
  #Create the map
  Map_Plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y), color = "#a503fc", size = 0.8) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
    annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    theme(legend.position = c(0.4,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size =10)) + 
    theme(panel.background = element_rect(fill = "white")) ##878c8c used to be fill 
  return(Map_Plot)
}

Pres_Map_RUR <- function(BC, Kelp_Points){
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
  #Create the map
  Map_Plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y), color = "#e6360F", size = 0.8) + 
    coord_sf(crs = st_crs(3005)) + xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
    annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    theme(legend.position = c(0.4,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size =10)) + 
    theme(panel.background = element_rect(fill = "white")) ##878c8c used to be fill 
  return(Map_Plot)
}