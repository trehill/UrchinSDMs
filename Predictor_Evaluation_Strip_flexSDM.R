## Functions for performing evaluation strip method

#Write a function to create evaluation strip predictions for each variable
ES_Predictions <- function(Enviro_df, Evaluation_Strip_Base, Evaluation_Strip_Base_Max, Evaluation_Strip_Base_Min, Evaluation_Strip_Range, BRT, Max, GLM, GAM){
  #set the seed 
  set.seed(8546)
  #define the rowMin and rowMax functions
  rowMax <- function(data) as.data.frame(sapply(as.data.frame(t(data)), max, na.rm = TRUE))
  rowMin <- function(data) as.data.frame(sapply(as.data.frame(t(data)), min, na.rm = TRUE))
  
  #Create a dataframe to hold the mean evaluation strip predictions for each variable for each model
  #BRT
  BRT_ES_Mean <- data.frame()
  BRT_ES_Mean[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(BRT_ES_Mean) <- colnames(Enviro_df)
  #Max
  Max_ES_Mean <- data.frame()
  Max_ES_Mean[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(Max_ES_Mean) <- colnames(Enviro_df)
  #GLM
  GLM_ES_Mean <- data.frame()
  GLM_ES_Mean[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GLM_ES_Mean) <- colnames(Enviro_df)
  #GAM
  GAM_ES_Mean <- data.frame()
  GAM_ES_Mean[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GAM_ES_Mean) <- colnames(Enviro_df)
  #Create a dataframe to hold the max evaluation strip predictions for each variable for each model
  #BRT
  BRT_ES_Max <- data.frame()
  BRT_ES_Max[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(BRT_ES_Max) <- colnames(Enviro_df)
  #Max
  Max_ES_Max <- data.frame()
  Max_ES_Max[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(Max_ES_Max) <- colnames(Enviro_df)
  #GLM
  GLM_ES_Max <- data.frame()
  GLM_ES_Max[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GLM_ES_Max) <- colnames(Enviro_df)
  #GAM
  GAM_ES_Max <- data.frame()
  GAM_ES_Max[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GAM_ES_Max) <- colnames(Enviro_df)
  #Create a dataframe to hold the mean evaluation strip predictions for each variable for each model
  #BRT
  BRT_ES_Min <- data.frame()
  BRT_ES_Min[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(BRT_ES_Min) <- colnames(Enviro_df)
  #Max
  Max_ES_Min <- data.frame()
  Max_ES_Min[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(Max_ES_Min) <- colnames(Enviro_df)
  #GLM
  GLM_ES_Min <- data.frame()
  GLM_ES_Min[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GLM_ES_Min) <- colnames(Enviro_df)
  #GAM
  GAM_ES_Min <- data.frame()
  GAM_ES_Min[1:1000,1:ncol(Enviro_df)] <- 0
  colnames(GAM_ES_Min) <- colnames(Enviro_df)
  
  #Loop through all of the variables and predict to their full range keeping all other predictors at their mean, max, or min
  for (i in 1:ncol(BRT_ES_Mean)){
    #set up the dataframes for each variable iteration
    #Mean
    ES_Predictors_Mean <- Evaluation_Strip_Base
    ES_Predictors_Mean[,i] <- Evaluation_Strip_Range[,i]
    #Max
    ES_Predictors_Max <- Evaluation_Strip_Base_Max
    ES_Predictors_Max[,i] <- Evaluation_Strip_Range[,i]
    #Min
    ES_Predictors_Min <- Evaluation_Strip_Base_Min
    ES_Predictors_Min[,i] <- Evaluation_Strip_Range[,i]
    
    #Predict to each using each ES df using all models
    #Mean predictions
    BRT_ES_Mean[,i] <- predict.gbm(BRT$model, ES_Predictors_Mean, type = "response")
    Max_ES_Mean[,i] <- predictMaxNet(Max$model, ES_Predictors_Mean, TRUE, "cloglog")
    GLM_ES_Mean[,i] <- predict.glm(GLM$model, ES_Predictors_Mean, type = "response")
    GAM_ES_Mean[,i] <- predict.gam(GAM$model, ES_Predictors_Mean, type = "response")
    #Max predictions
    BRT_ES_Max[,i] <- predict.gbm(BRT$model, ES_Predictors_Max, type = "response")
    Max_ES_Max[,i] <- predictMaxNet(Max$model, ES_Predictors_Max, TRUE, "cloglog")
    GLM_ES_Max[,i] <- predict.glm(GLM$model, ES_Predictors_Max, type = "response")
    GAM_ES_Max[,i] <- predict.gam(GAM$model, ES_Predictors_Max, type = "response")
    #Min predictions
    BRT_ES_Min[,i] <- predict.gbm(BRT$model, ES_Predictors_Min, type = "response")
    Max_ES_Min[,i] <- predictMaxNet(Max$model, ES_Predictors_Min, TRUE, "cloglog")
    GLM_ES_Min[,i] <- predict.glm(GLM$model, ES_Predictors_Min, type = "response")
    GAM_ES_Min[,i] <- predict.gam(GAM$model, ES_Predictors_Min, type = "response")
  }
  
  #Add a column to each ES df for row ID, model type and hold type and bind them together
  BRT_ES_Mean$ID <- seq(from = 1, to = 1000, by = 1)
  BRT_ES_Mean$Model <- "BRT"
  BRT_ES_Mean$Hold <- "Mean"
  BRT_ES_Max$ID <- seq(from = 1, to = 1000, by = 1)
  BRT_ES_Max$Model <- "BRT"
  BRT_ES_Max$Hold <- "Max"
  BRT_ES_Min$ID <- seq(from = 1, to = 1000, by = 1)
  BRT_ES_Min$Model <- "BRT"
  BRT_ES_Min$Hold <- "Min"
  Max_ES_Mean$ID <- seq(from = 1, to = 1000, by = 1)
  Max_ES_Mean$Model <- "MaxEnt"
  Max_ES_Mean$Hold <- "Mean"
  Max_ES_Max$ID <- seq(from = 1, to = 1000, by = 1)
  Max_ES_Max$Model <- "MaxEnt"
  Max_ES_Max$Hold <- "Max"
  Max_ES_Min$ID <- seq(from = 1, to = 1000, by = 1)
  Max_ES_Min$Model <- "MaxEnt"
  Max_ES_Min$Hold <- "Min"
  GLM_ES_Mean$ID <- seq(from = 1, to = 1000, by = 1)
  GLM_ES_Mean$Model <- "GLM"
  GLM_ES_Mean$Hold <- "Mean"
  GLM_ES_Max$ID <- seq(from = 1, to = 1000, by = 1)
  GLM_ES_Max$Model <- "GLM"
  GLM_ES_Max$Hold <- "Max"
  GLM_ES_Min$ID <- seq(from = 1, to = 1000, by = 1)
  GLM_ES_Min$Model <- "GLM"
  GLM_ES_Min$Hold <- "Min"
  GAM_ES_Mean$ID <- seq(from = 1, to = 1000, by = 1)
  GAM_ES_Mean$Model <- "GAM"
  GAM_ES_Mean$Hold <- "Mean"
  GAM_ES_Max$ID <- seq(from = 1, to = 1000, by = 1)
  GAM_ES_Max$Model <- "GAM"
  GAM_ES_Max$Hold <- "Max"
  GAM_ES_Min$ID <- seq(from = 1, to = 1000, by = 1)
  GAM_ES_Min$Model <- "GAM"
  GAM_ES_Min$Hold <- "Min"
  
  ES_Preds <- rbind(BRT_ES_Mean, BRT_ES_Max, BRT_ES_Min, Max_ES_Mean, Max_ES_Max, Max_ES_Min, GLM_ES_Mean, GLM_ES_Max, GLM_ES_Min, GAM_ES_Mean, GAM_ES_Max, GAM_ES_Min)
  
  return(ES_Preds)
}

ES_Plotter <- function(ES_Preds, var_id, var_name){
  
  #Summarize the ES data to find the mean and mean +/- standard error for the variable between all models
  ES_summary <- ES_Preds %>% dplyr::select(all_of(var_id), ID, Hold, Model) %>% group_by(ID, Model) %>% 
    dplyr::summarise(Mean_Response = mean(.data[[var_id]])) %>% ungroup() %>% group_by(ID) %>%
    dplyr::summarise(Mean = mean(Mean_Response), Max = mean(Mean_Response) + sd(Mean_Response), Min = mean(Mean_Response) - sd(Mean_Response))
  ES_summary_enviro <- cbind(ES_summary, Evaluation_Strip_Range)
  #Summarize the ES response for each individual model type
  ES_Model_Summary <- ES_Preds %>% dplyr::select(all_of(var_id), ID, Hold, Model) %>% group_by(ID, Model) %>% 
    dplyr::summarise(Mean_Response = mean(.data[[var_id]])) %>% ungroup() %>% group_by(ID) %>% pivot_wider(names_from = Model, values_from = Mean_Response, values_fill = 0)
  #Add these columns to the entire dataset and pivot longer
  ES_final_data <- merge(ES_summary_enviro, ES_Model_Summary, by = "ID")
  ES_Final_data_long <- ES_final_data %>% pivot_longer(cols = c("Mean", "BRT", "MaxEnt", "GLM", "GAM"), values_to = "Value", names_to = "Model")
  ES_Final_data_long$Model <- factor(ES_Final_data_long$Model, levels = c("Mean", "BRT", "MaxEnt", "GLM", "GAM"))
  #Generate a dataframe to create a smooth ribbon representing standard error in model ES predictions around the ensemble line
  ES_ribbon_frame <- ES_Final_data_long %>% dplyr::filter(Model != "Mean")
  
  #Create a plot with all response curves
  ES_Plot_All <- ggplot(ES_Final_data_long) +
    geom_smooth(aes(x = .data[[var_id]], y = Value, color = Model), linewidth = 1, se = FALSE, na.rm = TRUE, method = "loess") +
    scale_color_manual(values = c("#301934", "#FDE725FF", "#55C667FF","#238A8DFF", "#EAD7d1"), name = "Model", labels = c("Ensemble", "BRT", "MaxEnt", "GLM", "GAM")) +
    theme_cowplot()+ylab("LO")+xlab(var_name)+theme(legend.position = "right")+
    guides(x.sec = "axis", y.sec = "axis")+theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16), axis.text.x = element_text(angle = 360))
  #Create a plot of just the ensemble response curve
  ES_Plot <- ggplot(ES_ribbon_frame) +
    geom_smooth(aes(x = .data[[var_id]], y = Value), linewidth = 1, color = "#301934", se = TRUE, method = "loess") +
    theme_cowplot()+ylab("LO")+xlab(var_name)+theme(legend.position = "none")+
    guides(x.sec = "axis", y.sec = "axis")+theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16), axis.text.x = element_text(angle = 360))
  #return both plots
  return(list(ES_Plot, ES_Plot_All))
}