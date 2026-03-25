#Function to plot the PA datasets geographically
Geog_Plot_GUR <- function(BC, Kelp_Points){
    #Separate kelp points into presence and absence so I can overlay them on top of one another in the order I want on the plot
    Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
    Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
    Kelp_df$Occurrence <- factor(Kelp_df$Occurrence, levels = c("1", "0"))
    levels(Kelp_df$Occurrence) <- c("Presence", "Pseudo-absence")
    Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
      PA_plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y, color = Occurrence), size =0.8) + 
        scale_color_manual(values = c("#55C667FF", "#404788FF"), name = "Data Type") + 
        coord_sf(crs = st_crs(3005))+ xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
        theme_cowplot() + 
        theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size = 10)) + 
        annotation_scale(location = "bl", width_hint = 0.5) 
}

Geog_Plot_PUR <- function(BC, Kelp_Points){
  #Separate kelp points into presence and absence so I can overlay them on top of one another in the order I want on the plot
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
  Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
  Kelp_df$Occurrence <- factor(Kelp_df$Occurrence, levels = c("1", "0"))
  levels(Kelp_df$Occurrence) <- c("Presence", "Pseudo-absence")
  Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
  PA_plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y, color = Occurrence), size = 0.8) + 
    scale_color_manual(values = c("#a503fc", "#404788FF"), name = "Data Type") + 
    coord_sf(crs = st_crs(3005))+ xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size = 10)) + 
    annotation_scale(location = "bl", width_hint = 0.5) 
}


Geog_Plot_RUR <- function(BC, Kelp_Points){
  #Separate kelp points into presence and absence so I can overlay them on top of one another in the order I want on the plot
  Kelp_df <- st_as_sf(Kelp_Points) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
  Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
  Kelp_df$Occurrence <- factor(Kelp_df$Occurrence, levels = c("1", "0"))
  levels(Kelp_df$Occurrence) <- c("Presence", "Pseudo-absence")
  Kelp_df <- dplyr::arrange(Kelp_df, desc(Occurrence))
  PA_plot <- ggplot() + geom_sf(data = BC, fill = "#d9d9d9", color = NA) + geom_point(data = Kelp_df, aes(x=x, y=y, color = Occurrence), size =0.8) + 
    scale_color_manual(values = c("#e6360F", "#404788FF"), name = "Data Type") + 
    coord_sf(crs = st_crs(3005))+ xlim(c(515000, 1255000)) + ylim(c(340000, 1100000)) + 
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.01, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering) + 
    theme_cowplot() + 
    theme(legend.position = c(0.6,0.9), legend.title=element_text(size=11), legend.text = element_text(size=10), legend.text.align = 0, legend.title.align = 0, axis.title = element_blank(), axis.text = element_text(size = 10)) + 
    annotation_scale(location = "bl", width_hint = 0.5) 
}