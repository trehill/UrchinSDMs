
#This function creates a final dataset of count points 

Point_Creator <- function(All_Transects, Species_Code, DEM){
  # Filter out all transects for the species of interest
  Transects <- All_Transects %>% dplyr::filter(species == Species_Code)
  
  # Create a new raster filled with zeros using bathymetry
  Urchin_Raster <- DEM * 0
  
  # Set values in the raster equal to 1 where the species occurred based on unique cell numbers
  Urchin_Raster[unique(Transects$CellNum)] <- 1
  
  # Create a dataset of points for these occurrences
  Urchin_Points <- terra::as.points(x = Urchin_Raster, values = TRUE, na.rm = TRUE)
  Urchin_df <- sf::st_as_sf(Urchin_Points) %>% 
    filter(WEST_COAST_DEM2 == 1) %>%
    dplyr::mutate(species = Species_Code)  # Add species column
  
  Urchin_Presence_Points <- terra::vect(Urchin_df)
  
  # Export the new raster and vector of presence locations
  Urchin_Return <- list(Urchin_Raster, Urchin_Presence_Points)
  return(Urchin_Return)
}

#This function maps out point locations for species presence data given the input
#the points and a land shapefile

Pres_Map <- function(BC, Urchin_Points) {
  # Convert SpatVector objects to sf
  BC_sf <- sf::st_as_sf(BC)  
  Urchin_sf <- sf::st_as_sf(Urchin_Points)  
  
  # Extract coordinates and drop geometry
  Urchin_df <- Urchin_sf %>% 
    dplyr::mutate(
      x = sf::st_coordinates(.)[,1], 
      y = sf::st_coordinates(.)[,2]
    ) %>% 
    st_drop_geometry()
  
  # Create the map with species-specific colors
  Map_Plot <- ggplot() + 
    geom_sf(data = BC_sf) +  # Use the converted BC_sf
    geom_point(
      data = Urchin_df, 
      aes(x = x, y = y, color = species)
    ) +
    scale_color_manual(
      values = c("GUR" = "green", "RUR" = "red", "PUR" = "purple")
    ) +
    coord_sf(crs = st_crs(3005)) + 
    xlim(c(515000, 1255000)) + 
    ylim(c(340000, 1100000)) + 
    annotation_scale(location = "bl", width_hint = 0.5) + 
    annotation_north_arrow(
      location = "bl", 
      which_north = "true", 
      pad_x = unit(0.01, "in"), 
      pad_y = unit(0.25, "in"), 
      style = north_arrow_fancy_orienteering
    ) + 
    theme_cowplot() + 
    theme(
      legend.position = c(0.4, 0.9), 
      legend.title = element_text(size = 11), 
      legend.text = element_text(size = 10), 
      legend.text.align = 0, 
      legend.title.align = 0, 
      axis.title = element_blank(), 
      axis.text = element_text(size = 10)
    ) + 
    theme(panel.background = element_rect(fill = "white"))
  
  return(Map_Plot)
}


Shapefile_Creator <- function(filtered_data, bathymetry, selected_species) {
  # Filter the data for the specified year
  species_transects <- filtered_data 
  
  # Convert LINESTRING to POINT geometries safely
  species_points <- species_transects %>% st_cast("POINT", warn = FALSE)
  
  # Get coordinates as a matrix
  coordinates <- st_coordinates(species_points)[, 1:2]  # Ensure only lon/lat columns
  
  # Extract bathymetry values (handling single-layer raster)
  bathymetry_values <- terra::extract(bathymetry, coordinates)[,1]
  
  # Handle missing values
  bathymetry_values[is.na(bathymetry_values)] <- -999  # Replace NA with a placeholder
  
  # Construct output dataframe
  output_df <- data.frame(
    species = selected_species,
    density = species_points$density,
    lon = coordinates[,1],
    lat = coordinates[,2],
    bathymetry_value = bathymetry_values
  )
  
  # Convert to sf object
  output_sf <- st_as_sf(output_df, coords = c("lon", "lat"), crs = st_crs(species_transects))
  
  # Convert sf to SpatVector for terra compatibility
  output_vect <- terra::vect(output_sf)
  
  return(output_vect)
}

#write a function so plot overall density of each species on a map 
Dens_Map_RUR <- function(BC, Kelp_Points){   
  Kelp_df <- st_as_sf(Kelp_Points) %>%     
    dplyr::filter(density > 0) %>%  # filter out zero density         
    dplyr::mutate(
      x = sf::st_coordinates(.)[,1],       
      y = sf::st_coordinates(.)[,2],
      density_group = ifelse(density > 10, "high", "low")  # new column for coloring
    ) %>%     
    st_drop_geometry()
  
  # Separate color aesthetic so we can do manual coloring
  Map_Plot <- ggplot() +
    geom_sf(data = BC) +
    geom_point(data = Kelp_df %>% filter(density <= 10), 
               aes(x = x, y = y, color = density), size = 1) +
    geom_point(data = Kelp_df %>% filter(density > 10), 
               aes(x = x, y = y), color = "#aa1e1e", size = 1) +  # fixed red for high density
    scale_color_gradient(
      low = "#ffb8b8",   # light red
      high = "red",  # dark red
      name = "Density (1–10)",
      limits = c(1,10),
      na.value = "transparent"
    ) +
    coord_sf(crs = st_crs(3005)) +
    xlim(c(515000, 1255000)) +
    ylim(c(340000, 1100000)) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(
      location = "bl", 
      which_north = "true", 
      pad_x = unit(0.01, "in"), 
      pad_y = unit(0.25, "in"), 
      style = north_arrow_fancy_orienteering
    ) +
    theme_cowplot() + 
    theme(
      legend.position = c(0.4, 0.9),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.text.align = 0,
      legend.title.align = 0,
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white")
    )
  
  return(Map_Plot)
}

#write a function so plot overall density of each species on a map 
Dens_Map_PUR <- function(BC, Kelp_Points){   
  Kelp_df <- st_as_sf(Kelp_Points) %>%     
    dplyr::filter(density > 0) %>%  # filter out zero density         
    dplyr::mutate(
      x = sf::st_coordinates(.)[,1],       
      y = sf::st_coordinates(.)[,2],
      density_group = ifelse(density > 10, "high", "low")  # new column for coloring
    ) %>%     
    st_drop_geometry()
  
  # Separate color aesthetic so we can do manual coloring
  Map_Plot <- ggplot() +
    geom_sf(data = BC) +
    geom_point(data = Kelp_df %>% filter(density <= 10), 
               aes(x = x, y = y, color = density), size = 1) +
    geom_point(data = Kelp_df %>% filter(density > 10), 
               aes(x = x, y = y), color = "#49057d", size = 1) +  # fixed red for high density
    scale_color_gradient(
      low = "#e2c5f9",   # light red
      high = "purple",  # dark red
      name = "Density (1–10)",
      limits = c(1,10),
      na.value = "transparent"
    ) +
    coord_sf(crs = st_crs(3005)) +
    xlim(c(515000, 1255000)) +
    ylim(c(340000, 1100000)) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(
      location = "bl", 
      which_north = "true", 
      pad_x = unit(0.01, "in"), 
      pad_y = unit(0.25, "in"), 
      style = north_arrow_fancy_orienteering
    ) +
    theme_cowplot() + 
    theme(
      legend.position = c(0.4, 0.9),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.text.align = 0,
      legend.title.align = 0,
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white")
    )
  
  return(Map_Plot)
}


#write a function so plot overall density of each species on a map 
Dens_Map_GUR <- function(BC, Kelp_Points){   
  Kelp_df <- st_as_sf(Kelp_Points) %>%     
    dplyr::filter(density > 0) %>%  # filter out zero density         
    dplyr::mutate(
      x = sf::st_coordinates(.)[,1],       
      y = sf::st_coordinates(.)[,2],
      density_group = ifelse(density > 10, "high", "low")  # new column for coloring
    ) %>%     
    st_drop_geometry()
  
  # Separate color aesthetic so we can do manual coloring
  Map_Plot <- ggplot() +
    geom_sf(data = BC) +
    geom_point(data = Kelp_df %>% filter(density <= 10), 
               aes(x = x, y = y, color = density), size = 1) +
    geom_point(data = Kelp_df %>% filter(density > 10), 
               aes(x = x, y = y), color = "#0a5d04", size = 1) +  # fixed red for high density
    scale_color_gradient(
      low = "#d7fdd4",   # light red
      high = "green",  # dark red
      name = "Density (1–10)",
      limits = c(1,10),
      na.value = "transparent"
    ) +
    coord_sf(crs = st_crs(3005)) +
    xlim(c(515000, 1255000)) +
    ylim(c(340000, 1100000)) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(
      location = "bl", 
      which_north = "true", 
      pad_x = unit(0.01, "in"), 
      pad_y = unit(0.25, "in"), 
      style = north_arrow_fancy_orienteering
    ) +
    theme_cowplot() + 
    theme(
      legend.position = c(0.4, 0.9),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.text.align = 0,
      legend.title.align = 0,
      axis.title = element_blank(),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white")
    )
  
  return(Map_Plot)
}



