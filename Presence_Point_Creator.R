#This function creates a final dataset of presence points from DFO transect data 
#using presence records associated with "study area" grid cells and the grid itself

Point_Creator <- function(All_Transects, Species_Code, DEM){
  #Filter out all transects for the species of interest
  Transects <- All_Transects %>% dplyr::filter(Species == Species_Code)
  #Create a new raster filled with zeros using bathymetry
  Kelp_Raster <- DEM*0
  #Set values in the raster equal to 1 where the species of interest occurred based on unique cell numbers
  Kelp_Raster[unique(Transects$CellNum)] <- 1
  #Create a dataset of points for these kelp occurrences represented as the centroid of each raster cell where a presence occurred
  Kelp_Points <- terra::as.points(x = Kelp_Raster, values = TRUE, na.rm = TRUE)
  Kelp_df <- sf::st_as_sf(Kelp_Points) %>% filter(WEST_COAST_DEM2 == 1)
  Kelp_Presence_Points <- terra::vect(Kelp_df)
  #Export the new raster and vector of presence locations
  Kelp_Return <- list(Kelp_Raster, Kelp_Presence_Points)
  return(Kelp_Return)
}