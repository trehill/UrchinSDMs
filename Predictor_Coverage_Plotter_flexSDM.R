#Write a function to create plots comparing the range of the predictor data for each kelp species
#presence and presence/pseudo-absence dataset to that of the environmental data
Enviro_plot <- function(Enviro_df_long, Kelp_Data, Predictor_num){
  #Take the columns specifically for predictor variables from the data 
  Kelp_Predictors <- Kelp_Data[,2:(Predictor_num+1)]
  #Create a long-form predictor dataframe
  Predictor_df_long <- Kelp_Predictors %>% pivot_longer(cols = everything(), names_to = "Type", values_to = "Value")
  Predictor_df_long$DataType <- "Presence/Pseudo-absence"
  #Create the presence only dataframe to pair with the above environmental dataframe
  Presence_df <- Kelp_Data %>% dplyr::filter(Occurrence == 1)
  Presence_df <- Presence_df[,2:(Predictor_num+1)]
  Presence_df_long <- Presence_df %>% pivot_longer(cols = everything(), names_to = "Type", values_to = "Value")
  Presence_df_long$DataType <- "Presence-only"
  
  #Create a dataframe of all the environmental conditions
  Enviro_df_long_final <- Enviro_df_long[[1]]
  for (i in 2:length(Enviro_df_long)){
    Enviro_df_long_final <- rbind(Enviro_df_long_final, Enviro_df_long[[i]])
  }
  
  #Combine the dataframes
  Plot_df <- rbind(Presence_df_long, Predictor_df_long, Enviro_df_long_final)
  
  
  #Turn type into a factor and rename all the levels to what I want the plot titles to be
  Plot_df$Type <- factor(Plot_df$Type, levels = c("temp_s_max", "temp_s_mean", "temp_s_min", "temp_10m_max", "temp_10m_mean", "temp_10m_min", "salt_10m_mean", "salt_10m_min",  "do_10m_mean", "do_10m_max", "TAlk_10m_mean",  "Current", "REI", "Rock", "Slope",  "Depth"))
  levels(Plot_df$Type) <- c("Tsmax (\u00B0C)", "Tsmean (\u00B0C)", "Tsmin (\u00B0C)", "Tbmax (\u00B0C)", "Tbmean (\u00B0C)", "Tbmin (\u00B0C)",  "Sbmean (PSU)", "Sbmin (PSU)", "DObmean(mmol/m^3)", "DObmax (mmol/m^3)", "TAlkbmean (mmol/m^3)", "Current (m/s)", "REI", "Percent Rock", "Slope (deg)", "Depth (m)")
  
  #Create a gg boxplot to look at the spread of the data
  Enviro_kelp_plot <- ggplot(Plot_df, aes(x = DataType, y = Value, fill = DataType)) + geom_violin(alpha = 0.8) + 
    theme_cowplot() + facet_wrap(Type~., scales = "free") + 
    theme(legend.position = c(0.43,0.12), axis.text.x = element_blank(), legend.title = element_text(size = 32), legend.text = element_text(size =28)) + 
    ylab(NULL) + xlab(NULL) + theme(strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size=14)) +
    scale_fill_manual(values = c("#404788FF", "#55C667FF", "#238A8DFF"))
  return(Enviro_kelp_plot)
}