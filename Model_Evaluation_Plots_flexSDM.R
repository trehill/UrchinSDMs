## Functions for creating plots summarizing model evaluation metrics for real and null models for each PA dataset
Eval_Plot <- function(Eval){
  #Create long-form dataframes for the model and null model metrics for plotting
  Model_metrics_long <- Eval[[1]] %>% pivot_longer(cols = c("AUC_Train", "AUC_Test", "AUCdiff", "OErr"), names_to = "Metric", values_to = "Value")
  Model_metrics_long$Metric <- factor(Model_metrics_long$Metric, levels = c("AUC_Train", "AUC_Test", "AUCdiff", "OErr"))
  Null_metrics_long <- Eval[[2]] %>% pivot_longer(cols = c("AUC_Train", "AUC_Test", "AUCdiff", "OErr"), names_to = "Metric", values_to = "Value")
  Null_metrics_long$Metric <- factor(Null_metrics_long$Metric, levels = c("AUC_Train", "AUC_Test", "AUCdiff", "OErr"))
  
  #Plot the model and null model evaluation metrics
  Model_Eval_Plot <- ggplot() + geom_boxplot(data = Null_metrics_long, aes(x = Metric, y = Value, fill = Metric)) + 
    scale_fill_manual(values = c("#613f74", "#91B3B6", "#F4E76E", "#EDAF97"), labels = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error")) + 
    stat_summary(data = Model_metrics_long, aes(x = Metric, y = Value), fun.y = mean, geom = "point", colour = "#e6360F", size = 3) + 
    xlab("Evaluation Metric") + ylab("Value") + theme_classic() + 
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.x.bottom = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    theme(legend.position = "none", legend.justification = "center") + 
    theme(axis.text = element_text(size=8), axis.title = element_text(size = 12))
  return(Model_Eval_Plot)
}

Effect_Size_Plot <- function(Eval){
  Null <- Eval[[2]]
  Model <- Eval[[1]]
  #Calculate the mean and standard deviation of all evaluation metrics for the null models
  AUCtrain_avg <- mean(Null$AUC_Train)
  AUCtrain_stdev <- stats::sd(Null$AUC_Train)
  AUCtest_avg <- mean(Null$AUC_Test)
  AUCtest_stdev <- stats::sd(Null$AUC_Test)
  AUCdiff_avg <- mean(Null$AUCdiff)
  AUCdiff_stdev <- stats::sd(Null$AUCdiff)
  OErr_avg <- mean(Null$OErr)
  OErr_stdev <- stats::sd(Null$OErr)
    
  #Calculate the standardized effect sizes for each null model
  for (i in 1:nrow(Null)){
    Null$SES_AUC_Train[i] <- (Null$AUC_Train[i] - AUCtrain_avg)/AUCtrain_stdev
    Null$SES_AUC_Test[i] <- (Null$AUC_Test[i] - AUCtest_avg)/AUCtest_stdev
    Null$SES_AUCdiff[i] <- (Null$AUCdiff[i] - AUCdiff_avg)/AUCdiff_stdev
    Null$SES_OErr[i] <- (Null$OErr[i] - OErr_avg)/OErr_stdev
  }
    
  #Calculate the standardized effect sizes for each real model
  Model$SES_AUC_Train <- (Model$AUC_Train - AUCtrain_avg)/AUCtrain_stdev
  Model$SES_AUC_Test <- (Model$AUC_Test - AUCtest_avg)/AUCtest_stdev
  Model$SES_AUCdiff <- (Model$AUCdiff - AUCdiff_avg)/AUCdiff_stdev
  Model$SES_OErr <- (Model$OErr - OErr_avg)/OErr_stdev
    
  #Calculate the 95 or 5% quantiles in the null metrics effect sizes and add these to the model dataframe
  Model$AUC_Train95 <- quantile(Null$SES_AUC_Train, 0.95)
  Model$AUC_Test95 <- quantile(Null$SES_AUC_Test, 0.95)
  Model$AUCdiff5 <- quantile(Null$SES_AUCdiff, 0.05)
  Model$OErr5 <- quantile(Null$SES_OErr, 0.05)
  
  #pull out the SES values and pivot them longer
  Model_SES <- Model %>% dplyr::select(SES_AUC_Train, SES_AUC_Test, SES_AUCdiff, SES_OErr)
  colnames(Model_SES) <- c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error")
  Model_SES_long <- Model_SES %>% pivot_longer(cols = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error"), names_to = "Metric", values_to = "SES")
  #pull out the threshold values and pivot them longer
  Model_Thresholds <- Model %>% dplyr::select(AUC_Train95, AUC_Test95, AUCdiff5, OErr5)
  colnames(Model_Thresholds) <- c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error")
  Model_Thresholds_long <- Model_Thresholds %>% pivot_longer(cols = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error"), names_to = "Metric", values_to = "Threshold")
  #Merge the dataframes
  Model_data <- merge(Model_SES_long, Model_Thresholds_long, by = "Metric")
  Model_data$Metric <- factor(Model_data$Metric, levels = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error"))
  
  #Create a plot to show how each metric compares to its threshold for each of the 10 PA datasets
  SES_Plot <- ggplot(Model_data) + geom_col(aes(x = Metric, y = SES, fill = Metric)) + 
    geom_hline(aes(col = Metric, yintercept = Threshold), linetype = "longdash") + theme_classic() + 
    scale_fill_manual(values = c("#613f74", "#91B3B6", "#F4E76E", "#EDAF97"), labels = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error"))+ 
    scale_colour_manual(values = c("#613f74", "#91B3B6", "#F4E76E", "#EDAF97"), labels = c("Training AUC", "Testing AUC", "AUC Difference", "Omission Error")) + 
    xlab("Evaluation Metric") + ylab("Standardized Effect Size") + 
    guides(x.sec = "axis", y.sec = "axis")+ theme(axis.line.y.left = element_line(size = 0.75, color = "black"), axis.line.y.right = element_line(size = 0.75, color = "black"), axis.line.x.top = element_line(size = 0.75, color = "black"), axis.line.x.bottom = element_line(size = 0.75, color = "black"),axis.ticks.x.top = element_blank(), axis.ticks.x.bottom = element_blank(), axis.ticks.y.right = element_blank(), axis.text.x.top = element_blank(), axis.text.y.right = element_blank()) + 
    theme(axis.text = element_text(size=8), axis.title = element_text(size = 12)) + 
    theme(legend.position = "none", legend.justification = "center")
    
  return(SES_Plot)
}