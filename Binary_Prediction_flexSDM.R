#Function for determining the optimal TSS and Omission Error cutoff
Optimize_Threshold <- function(Kelp_Data, BRT, Max, GLM, GAM){
  set.seed(6764)
  #create predictions for the entire dataset
  #Predict to the data with each model
  BRT_Pred <- predict.gbm(BRT$model, Kelp_Data[,2:(Predictor_num+1)], type = "response")
  Max_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Pred <- predict.glm(GLM$model, Kelp_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Pred <- predict.gam(GAM$model, Kelp_Data[,2:(Predictor_num+1)], type = "response")
  #Calculate weights for each model based on AUC
  BRT_weight <- (BRT$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  Max_weight <- (Max$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GLM_weight <- (GLM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GAM_weight <- (GAM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  #Ensemble the predictions by AUC weighting
  ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
  #Add a column to the kelp data
  Kelp_Data$pred <- ENS_Pred
  
  #Set up a dataframe to house TSS and Omission Error Values 
  TSS <- data.frame(seq(from = 0, to = 1, length.out = 1000),(0:0),(0:0))
  colnames(TSS) <- c("Threshold", "TSS", "OErr")
  
  #Now loop through a bunch of different potential thresholds and calculate TSS and omission error
  for (i in 1:length(TSS$Threshold)){
    Kelp_Data$Binary <- (Kelp_Data$pred > TSS$Threshold[i])
    Kelp_Data$Binary <- as.numeric(Kelp_Data$Binary)
    TP <- nrow(dplyr::filter(Kelp_Data,as.factor(Kelp_Data$Occurrence) == 1 & as.factor(Kelp_Data$Binary) == 1))
    TN <- nrow(dplyr::filter(Kelp_Data,as.factor(Kelp_Data$Occurrence) == 0 & as.factor(Kelp_Data$Binary) == 0))
    FP <- nrow(dplyr::filter(Kelp_Data,as.factor(Kelp_Data$Occurrence) == 0 & as.factor(Kelp_Data$Binary) == 1))
    FN <- nrow(dplyr::filter(Kelp_Data,as.factor(Kelp_Data$Occurrence) == 1 & as.factor(Kelp_Data$Binary) == 0))
    TPR <- TP/(TP+FN)
    FPR <- FP/(TN+FP)
    TSS$TSS[i] <- TPR - FPR
    TSS$OErr[i] <- nrow(dplyr::filter(Kelp_Data, Occurrence == 1 & pred < TSS$Threshold[i]))/nrow(dplyr::filter(Kelp_Data, Occurrence == 1))
  }
  
  #Create a plot of TSS and Omission Error vs threshold 
  Optimal_TSS_Threshold <- TSS$Threshold[which.max(TSS$TSS)]
  Max_TSS <- max(TSS$TSS)
  TSS_OErr <- TSS$OErr[which.max(TSS$TSS)]
  Optimal_OErr_Threshold <- TSS$Threshold[(length(TSS$OErr)-which.min(rev(TSS$OErr)))]
  OErr_TSS <- TSS$TSS[(length(TSS$OErr)-which.min(rev(TSS$OErr)))]
  Min_OErr <- min(TSS$OErr)
  TSS_Plot <- ggplot(TSS) + geom_line(aes(x = Threshold, y = TSS), size = 1, color = "#55C667FF")+ geom_line(aes(x = Threshold, y = OErr), size = 1, color = "#404788FF") + theme_cowplot()+ylab("TSS or Omission Error")+xlab("Threshold")+theme(legend.position = "none")+guides(x.sec = "axis", y.sec = "axis")+theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + geom_vline(xintercept=Optimal_TSS_Threshold, size = 0.75, color = "#55C667FF", linetype = "longdash") + geom_vline(xintercept=Optimal_OErr_Threshold, size = 0.75, color = "#404788FF", linetype = "longdash") + ylim(0,1)
  
  #Generate and return output
  Output <- list(TSS_Plot, Optimal_TSS_Threshold, Optimal_OErr_Threshold, Kelp_Data, Max_TSS, OErr_TSS, Min_OErr, TSS_OErr, TSS)
  return(Output)
}

#Function to create a plot of the density of true and false negative predictions
Density_Plot <- function(Threshold_Data, Threshold){
  #Create a data frame for the plot
  Density_Plot_Frame <- Threshold_Data %>% dplyr::select(Occurrence, pred)
  Density_Plot_Frame$Occurrence <- as.factor(Density_Plot_Frame$Occurrence)
  
  #Create a column that states whether something is a true or false positive or negative
  Density_Plot_Frame$Nature <- 0
  Density_Plot_Frame$Nature <- ifelse(Density_Plot_Frame$Occurrence == 0 & Density_Plot_Frame$pred <= Threshold, "True Negative", ifelse(Density_Plot_Frame$Occurrence == 0 & Density_Plot_Frame$pred > Threshold, "False Positive", ifelse(Density_Plot_Frame$Occurrence == 1 & Density_Plot_Frame$pred > Threshold, "True Positive", ifelse(Density_Plot_Frame$Occurrence == 1 & Density_Plot_Frame$pred <= Threshold, "False Negative", Density_Plot_Frame$Nature))))
  Density_Plot_Frame$Nature <- as.factor(Density_Plot_Frame$Nature)
  
  #Create the plot
  Density_Plot <- ggplot(Density_Plot_Frame) + geom_hline(yintercept = Threshold, color = "#5A5A5A", linetype = "longdash", size = 0.75) + geom_point(aes(x = Occurrence, y = pred, colour = Nature), position = "jitter", pch = 19, size = 3, alpha = 0.5) + scale_color_manual(values = c("#FDE725FF", "#55C667FF", "#404788FF", "#238A8DFF")) + theme_cowplot() + xlab("Observed Presence (1) or Absence (0)") + ylab("Predicted Likelihood of Occurrence") + guides(x.sec = "axis", y.sec = "axis")+theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + theme(legend.position = "bottom", legend.justification = "center", legend.title = element_blank())+ geom_violin(aes(x = Occurrence, y = pred), color = "black", alpha = 0, size = 0.75) + theme(axis.text = element_text(size=14), axis.title = element_text(size = 16), legend.text = element_text(size = 16))
  
  return(Density_Plot)
}