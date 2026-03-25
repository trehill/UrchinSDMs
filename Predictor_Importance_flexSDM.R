#Create a function to determine the relative importance of different predictors to model outcomes as well as aggregated predictor classes
Predictor_Importance_GUR <- function(ENS, training_df, Predictor_num){
  set.seed(76843)
  #Use the flex SDM package to calculate variable importance by model permutation
  Variable_Importance <- sdm_varimp(ENS, data = training_df, response = "Occurrence", predictors = colnames(training_df[2:(Predictor_num+1)]), n_sim = 100, n_cores = 10)
  #Replace all of the NAs with 0's 
  Variable_Importance[is.na(Variable_Importance)] <- 0
  #See what each column sums up to
  colSums(Variable_Importance[,4:13])
  #Summarize a mean value across all threshold metric for each importance metric
  Threshold_Summary <- Variable_Importance %>% dplyr::group_by(predictors) %>% dplyr::summarise(across(TPR:IMAE, list(mean)))
  colSums(Threshold_Summary[,2:11])
  #Scale each column from TPR to IMAE so all of their values are rescaled between 0 and 100
  Threshold_Summary[,2:11] <- data.frame(lapply(Threshold_Summary[,2:11], function(x){scale(x, center = FALSE, scale = sum(x, na.rm = TRUE)/100)}))
  colSums(Threshold_Summary[,2:11])
  #Now pivot the dataframe longer and summarize a variable importance mean, stdev and stderr for each predictor
  Summarized_Importance <- Threshold_Summary %>% pivot_longer(cols = TPR_1:IMAE_1, names_to = "metric", values_to = "value") %>% dplyr::group_by(predictors) %>% dplyr::summarise(Mean_Importance = mean(value, na.rm = TRUE), StDev_Importance = sd(value, na.rm = TRUE), StErr_Importance = stats::sd(value, na.rm = TRUE)/sqrt(n()))
  #Calculate min and max values for error bars using stdev or sterr (I use stdev here)
  Summarized_Importance$min <- Summarized_Importance$Mean_Importance - Summarized_Importance$StDev_Importance
  Summarized_Importance$max <- Summarized_Importance$Mean_Importance + Summarized_Importance$StDev_Importance
  
  #Create a plot for predictor importance of all predictors
  Predictor_Plot <- ggplot() + 
    geom_col(data = Summarized_Importance, mapping = aes(x = Mean_Importance, y = reorder(predictors, Mean_Importance)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#55C667FF") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    geom_errorbar(data = Summarized_Importance, mapping = aes(y = predictors, xmin = min, xmax = max), position = position_dodge2(preserve = "total"), width = 0.5, size = 0.7) + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))

  #Create a version of the plot that is summarized up to the scale of large scale predictor groupings
  
  #Create a placeholder variable for predictor class
  Summarized_Importance$Predictor_Class <- "None"
  #Create groups of predictors
  for (i in 1:nrow(Summarized_Importance)){
    if(grepl("temp", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Temperature"
    }
    if(grepl("salt", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Salinity"
    }
    if(grepl("PAR", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Light"
    }
    if(grepl("Rock", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Substrate"
    }
    if(grepl("NH4", Summarized_Importance$predictors[i]) | grepl("NO3", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Nutrient"
    }
    if(grepl("Current", Summarized_Importance$predictors[i]) | grepl("REI", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Current/Exposure"
    }
    if(grepl("do", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Oxygen"
    }
    if(grepl("TAlk", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Alkalinity"
    }
    if(grepl("Slope", Summarized_Importance$predictors[i]) | grepl("Depth", Summarized_Importance$predictors[i]) | grepl("Aspect", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Bathymetric"
    }
  }
  
  #Sum mean predictor importance within predictor classes
  Class_Importance <- Summarized_Importance %>% dplyr::group_by(Predictor_Class) %>% summarise(Predictor_Imp = sum(Mean_Importance))
  
  #Create a plot for predictor class importance
  Class_Plot <- ggplot() + 
    geom_col(data = Class_Importance, mapping = aes(x = Predictor_Imp, y = reorder(Predictor_Class, Predictor_Imp)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#55C667FF") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))
  
  Class_Plot
  
  return(list(Variable_Importance, Predictor_Plot, Class_Plot))
}

######
#Create a function to determine the relative importance of different predictors to model outcomes as well as aggregated predictor classes
Predictor_Importance_RUR <- function(ENS, training_df, Predictor_num){
  set.seed(76843)
  #Use the flex SDM package to calculate variable importance by model permutation
  Variable_Importance <- sdm_varimp(ENS, data = training_df, response = "Occurrence", predictors = colnames(training_df[2:(Predictor_num+1)]), n_sim = 100, n_cores = 10)
  #Replace all of the NAs with 0's 
  Variable_Importance[is.na(Variable_Importance)] <- 0
  #See what each column sums up to
  colSums(Variable_Importance[,4:13])
  #Summarize a mean value across all threshold metric for each importance metric
  Threshold_Summary <- Variable_Importance %>% dplyr::group_by(predictors) %>% dplyr::summarise(across(TPR:IMAE, list(mean)))
  colSums(Threshold_Summary[,2:11])
  #Scale each column from TPR to IMAE so all of their values are rescaled between 0 and 100
  Threshold_Summary[,2:11] <- data.frame(lapply(Threshold_Summary[,2:11], function(x){scale(x, center = FALSE, scale = sum(x, na.rm = TRUE)/100)}))
  colSums(Threshold_Summary[,2:11])
  #Now pivot the dataframe longer and summarize a variable importance mean, stdev and stderr for each predictor
  Summarized_Importance <- Threshold_Summary %>% pivot_longer(cols = TPR_1:IMAE_1, names_to = "metric", values_to = "value") %>% dplyr::group_by(predictors) %>% dplyr::summarise(Mean_Importance = mean(value, na.rm = TRUE), StDev_Importance = sd(value, na.rm = TRUE), StErr_Importance = stats::sd(value, na.rm = TRUE)/sqrt(n()))
  #Calculate min and max values for error bars using stdev or sterr (I use stdev here)
  Summarized_Importance$min <- Summarized_Importance$Mean_Importance - Summarized_Importance$StDev_Importance
  Summarized_Importance$max <- Summarized_Importance$Mean_Importance + Summarized_Importance$StDev_Importance
  
  #Create a plot for predictor importance of all predictors
  Predictor_Plot <- ggplot() + 
    geom_col(data = Summarized_Importance, mapping = aes(x = Mean_Importance, y = reorder(predictors, Mean_Importance)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#e6360F") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    geom_errorbar(data = Summarized_Importance, mapping = aes(y = predictors, xmin = min, xmax = max), position = position_dodge2(preserve = "total"), width = 0.5, size = 0.7) + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))
  
  #Create a version of the plot that is summarized up to the scale of large scale predictor groupings
  
  #Create a placeholder variable for predictor class
  Summarized_Importance$Predictor_Class <- "None"
  #Create groups of predictors
  for (i in 1:nrow(Summarized_Importance)){
    if(grepl("temp", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Temperature"
    }
    if(grepl("salt", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Salinity"
    }
    if(grepl("PAR", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Light"
    }
    if(grepl("Rock", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Substrate"
    }
    if(grepl("NH4", Summarized_Importance$predictors[i]) | grepl("NO3", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Nutrient"
    }
    if(grepl("Current", Summarized_Importance$predictors[i]) | grepl("REI", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Current/Exposure"
    }
    if(grepl("do", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Oxygen"
    }
    if(grepl("TAlk", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Alkalinity"
    }
    if(grepl("Slope", Summarized_Importance$predictors[i]) | grepl("Depth", Summarized_Importance$predictors[i]) | grepl("Aspect", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Bathymetric"
    }
  }
  
  #Sum mean predictor importance within predictor classes
  Class_Importance <- Summarized_Importance %>% dplyr::group_by(Predictor_Class) %>% summarise(Predictor_Imp = sum(Mean_Importance))
  
  #Create a plot for predictor class importance
  Class_Plot <- ggplot() + 
    geom_col(data = Class_Importance, mapping = aes(x = Predictor_Imp, y = reorder(Predictor_Class, Predictor_Imp)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#e6360F") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))
  
  Class_Plot
  
  return(list(Variable_Importance, Predictor_Plot, Class_Plot))
}


####
#Create a function to determine the relative importance of different predictors to model outcomes as well as aggregated predictor classes
Predictor_Importance_PUR <- function(ENS, training_df, Predictor_num){
  set.seed(76843)
  #Use the flex SDM package to calculate variable importance by model permutation
  Variable_Importance <- sdm_varimp(ENS, data = training_df, response = "Occurrence", predictors = colnames(training_df[2:(Predictor_num+1)]), n_sim = 100, n_cores = 10)
  #Replace all of the NAs with 0's 
  Variable_Importance[is.na(Variable_Importance)] <- 0
  #See what each column sums up to
  colSums(Variable_Importance[,4:13])
  #Summarize a mean value across all threshold metric for each importance metric
  Threshold_Summary <- Variable_Importance %>% dplyr::group_by(predictors) %>% dplyr::summarise(across(TPR:IMAE, list(mean)))
  colSums(Threshold_Summary[,2:11])
  #Scale each column from TPR to IMAE so all of their values are rescaled between 0 and 100
  Threshold_Summary[,2:11] <- data.frame(lapply(Threshold_Summary[,2:11], function(x){scale(x, center = FALSE, scale = sum(x, na.rm = TRUE)/100)}))
  colSums(Threshold_Summary[,2:11])
  #Now pivot the dataframe longer and summarize a variable importance mean, stdev and stderr for each predictor
  Summarized_Importance <- Threshold_Summary %>% pivot_longer(cols = TPR_1:IMAE_1, names_to = "metric", values_to = "value") %>% dplyr::group_by(predictors) %>% dplyr::summarise(Mean_Importance = mean(value, na.rm = TRUE), StDev_Importance = sd(value, na.rm = TRUE), StErr_Importance = stats::sd(value, na.rm = TRUE)/sqrt(n()))
  #Calculate min and max values for error bars using stdev or sterr (I use stdev here)
  Summarized_Importance$min <- Summarized_Importance$Mean_Importance - Summarized_Importance$StDev_Importance
  Summarized_Importance$max <- Summarized_Importance$Mean_Importance + Summarized_Importance$StDev_Importance
  
  #Create a plot for predictor importance of all predictors
  Predictor_Plot <- ggplot() + 
    geom_col(data = Summarized_Importance, mapping = aes(x = Mean_Importance, y = reorder(predictors, Mean_Importance)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#a503fc") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    geom_errorbar(data = Summarized_Importance, mapping = aes(y = predictors, xmin = min, xmax = max), position = position_dodge2(preserve = "total"), width = 0.5, size = 0.7) + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))
  
  #Create a version of the plot that is summarized up to the scale of large scale predictor groupings
  
  #Create a placeholder variable for predictor class
  Summarized_Importance$Predictor_Class <- "None"
  #Create groups of predictors
  for (i in 1:nrow(Summarized_Importance)){
    if(grepl("temp", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Temperature"
    }
    if(grepl("salt", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Salinity"
    }
    if(grepl("PAR", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Light"
    }
    if(grepl("Rock", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Substrate"
    }
    if(grepl("NH4", Summarized_Importance$predictors[i]) | grepl("NO3", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Nutrient"
    }
    if(grepl("Current", Summarized_Importance$predictors[i]) | grepl("REI", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Current/Exposure"
    }
    if(grepl("do", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Oxygen"
    }
    if(grepl("TAlk", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Alkalinity"
    }
    if(grepl("Slope", Summarized_Importance$predictors[i]) | grepl("Depth", Summarized_Importance$predictors[i]) | grepl("Aspect", Summarized_Importance$predictors[i])){
      Summarized_Importance$Predictor_Class[i] <- "Bathymetric"
    }
  }
  
  #Sum mean predictor importance within predictor classes
  Class_Importance <- Summarized_Importance %>% dplyr::group_by(Predictor_Class) %>% summarise(Predictor_Imp = sum(Mean_Importance))
  
  #Create a plot for predictor class importance
  Class_Plot <- ggplot() + 
    geom_col(data = Class_Importance, mapping = aes(x = Predictor_Imp, y = reorder(Predictor_Class, Predictor_Imp)), position = position_dodge2(padding = 0), width = 0.7, preserve = "total", fill = "#a503fc") + 
    theme_cowplot() +
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    xlab("Predictor Importance") + ylab("Predictor Variable") + 
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 9))
  
  Class_Plot
  
  return(list(Variable_Importance, Predictor_Plot, Class_Plot))
}

