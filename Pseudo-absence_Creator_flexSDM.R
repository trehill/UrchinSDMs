
#This function function generates PA_num sets of pseudo-absence points throughout the study region 
#These sets of pseudo-absences will be generated randomly, but in an amount 20x what is needed.
#Therefore they can be environmentally clustered and an even amount selected from each cluster 
#to create the final data set of random/environmentally stratified pseudoabsences.

#Write the function for generating the pseudo-absences
PA_Creation <- function(Kelp_Raster, Kelp_Presence_Points, Overall_Kelp_Raster, Enviro_df, Gen_num){
  #Turn the kelp presence points into a dataframe with x and y coordinates
  
  
  Kelp_df <- terra::as.data.frame(Overall_Kelp_Raster, xy = TRUE) %>% dplyr::filter(WEST_COAST_DEM2 == 1)
  #Remove all of the presence locations from the overall environmental dataframe
  Background_points <- dplyr::anti_join(Enviro_df, Kelp_df, by = c("x","y"))
  
  #Set seed and randomly select Gen_num worth of background points to use as pseudoabsences
  
  set.seed(2325)
  PA_index <- sample(seq_len(nrow(Background_points)), size = Gen_num*20)
  PA_points <- Background_points[PA_index,]
  
  #Perform k-means clustering on pseudo absences based on environmental conditions
  PA_kmeans <- kmeans(PA_points[,3:ncol(PA_points)], centers = Gen_num, iter.max = 100000)
  #Add a column for cluster to the psedoabsences
  PA_points$cluster <- PA_kmeans$cluster
  #Group the PA dataframe by cluster and select a singular point from each
  PA_points_final <- PA_points[1:Gen_num,]
  for (j in 1:Gen_num){
    PA_temp <- PA_points %>% dplyr::filter(cluster == j)
    set.seed(23252323+j)
    PA_index <- sample(seq_len(nrow(PA_temp)), size = 1)
    PA_points_final[j,] <- PA_temp[PA_index,] 
  }
  #Turn the presence points into a sf dataframe and rename the column of ones to occurrence
  Kelp_Presence <- sf::st_as_sf(Kelp_Presence_Points)
  colnames(Kelp_Presence)[1] <- "Occurrence"
  
  #Create an occurence field in the PA data and select relevant layers
  PA_points_final$Occurrence <- 0
  PA_points_final <- PA_points_final %>% dplyr::select(Occurrence, x, y)
  #Turn the PA data into an sf dataframe
  Kelp_PA <- sf::st_as_sf(PA_points_final, coords = c("x","y"), crs = crs(Kelp_Presence_Points))
  
  #Combine the PA and Presence sf dataframes
  Kelp_Points <- rbind(Kelp_Presence, Kelp_PA)
  #Turn this into a Spatvector
  Kelp_Vector <- terra::vect(Kelp_Points)
  
  #return the kelp vector file
  return(Kelp_Vector)
}

