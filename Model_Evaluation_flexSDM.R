## Create final ensembles of BRTs and Glms, calculate metrics of model overfitting 
#(Omission Error with a 10% omission threshold, and AUCdiff) and discriminatory ability (AUC), 
#compare to null models.
Model_Eval <- function(ENS, Kelp_Training_Data, Kelp_Testing_Data, Survey_Enviro_df, BRT, Max, GLM, GAM, Predictor_num){
  
  
  set.seed(2303)
  #Predict to the testing dataframe with each individual model included in the ensemble model
  BRT_Pred <- predict.gbm(BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  Max_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Pred <- predict.glm(GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Pred <- predict.gam(GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  #Predict to the training data as well
  BRT_Train_Pred <- predict.gbm(BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  Max_Train_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Train_Pred <- predict.glm(GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Train_Pred <- predict.gam(GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  #Calculate weights for each model based on AUC
  BRT_weight <- (BRT$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  Max_weight <- (Max$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GLM_weight <- (GLM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GAM_weight <- (GAM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  #Ensemble the predictions by AUC weighting
  ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
  ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
  
  #Add a column to the testing and training data for the predictions
  Kelp_Testing_Data$pred <- ENS_Pred
  Kelp_Training_Data$pred <- ENS_Train_Pred
  
  #Calculate the testing AUC
  roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
  auc_Test <- pROC::auc(roc_Test)
  #Calculate the training AUC
  roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
  auc_Train <- pROC::auc(roc_Train)
  #Calculate the AUC difference between training data and testing fold
  AUCdiff <- auc_Train - auc_Test
  #Calculate the 10% omission error threshold for the training data
  Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
  Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
  
  
  #Calculate testing AUC for individual models
  BRT_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, BRT_Pred))
  Max_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, Max_Pred))
  GLM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GLM_Pred))
  GAM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GAM_Pred))
  
  
  
  #Create a dataframe containing AUC, AUCdiff, and Omission Error and return this
  Metrics <- data.frame(as.numeric(auc_Train), as.numeric(auc_Test), as.numeric(AUCdiff), as.numeric(Omission_Error))
  colnames(Metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")

  
  #Create null models using the same parameters and randomly drawn presence points for the enviro_buffer dataframe (same background points)
  
  #Set up a dataframe to hold all of the evaluation metrics for the null models
  Null_metrics <- data.frame(0,0,0,0)
  colnames(Null_metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")
  
  #Run 1000 null models
  for (j in 1:10){

    #Perform a random pull from the environmental dataframe for each presence point in the training dataset
    for (i in 1:nrow(Kelp_Training_Data)){
      if (Kelp_Training_Data$Occurrence[i] == 1){
        set.seed(313*j+round(46*i))
        randnum <- sample(seq_len(nrow(Survey_Enviro_df)), size = 1)
        Kelp_Training_Data[i, 2:(Predictor_num+1)] <- Survey_Enviro_df[randnum,]
      }
    }
    
    #Fit an ensemble null model to the new dataset using all the same parameters as for the actual models
    #BRT
    Null_BRT <- flexsdm::fit_gbm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), n_trees = BRT$model$n.trees, n_minobsinnode = BRT$model$n.minobsinnode, shrinkage = BRT$model$shrinkage)
    #MaxENT
    Null_Max <- flexsdm::fit_max(data = Kelp_Training_Data %>% dplyr::filter(Occurrence == 1), response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", background = Kelp_Training_Data %>% dplyr::filter(Occurrence == 0), thr = c("equal_sens_spec"), clamp = FALSE, regmult = Max$performance$regmult, classes = Max$performance$classes)
    #GLM
    Null_GLM <- flexsdm::fit_glm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GLM$model$formula, poly = 1, inter_order = 0)
    #GAM
    Null_GAM <- flexsdm::fit_gam(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GAM$model$formula, k = 3)
    
    #Fit an ensemble of all null methods
    Null_ENS <- flexsdm::fit_ensemble(models = list(Null_BRT, Null_Max, Null_GLM, Null_GAM), ens_method = "meanw", thr = c("equal_sens_spec"), thr_model = "equal_sens_spec", metric = "AUC")
    
    #Calculate null evaluation metrics
    
    #Predict to the testing dataframe with each individual model included in the ensemble model
    BRT_Pred <- predict.gbm(Null_BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    Max_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
    GLM_Pred <- predict.glm(Null_GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    GAM_Pred <- predict.gam(Null_GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    #Predict to the training data as well
    BRT_Train_Pred <- predict.gbm(Null_BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    Max_Train_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
    GLM_Train_Pred <- predict.glm(Null_GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    GAM_Train_Pred <- predict.gam(Null_GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    #Ensemble the predictions by AUC weighting
    ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
    ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
    
    #Add a column to the testing and training data for the predictions
    Kelp_Testing_Data$pred <- ENS_Pred
    Kelp_Training_Data$pred <- ENS_Train_Pred
    
    #Calculate the testing AUC
    roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
    auc_Test <- pROC::auc(roc_Test)
    #Calculate the training AUC
    roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
    auc_Train <- pROC::auc(roc_Train)
    #Calculate the AUC difference between training data and testing fold
    AUCdiff <- auc_Train - auc_Test
    #Calculate the 10% omission error threshold for the training data
    Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
    Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
    
    #Save the null metrics to the dataframe
    Null_metrics[j,1] <- as.numeric(auc_Train)
    Null_metrics[j,2] <- as.numeric(auc_Test)
    Null_metrics[j,3] <- as.numeric(AUCdiff)
    Null_metrics[j,4] <- as.numeric(Omission_Error)
  }
  return(list(Metrics, Null_metrics))
}


#########
#new model eval 
Model_Eval_NEW <- function(ENS, Kelp_Training_Data, Kelp_Testing_Data, Survey_Enviro_df, BRT, Max, GLM, GAM, Predictor_num){
  
  
  set.seed(2303)
  #Predict to the testing dataframe with each individual model included in the ensemble model
  BRT_Pred <- predict.gbm(BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  Max_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Pred <- predict.glm(GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Pred <- predict.gam(GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  #Predict to the training data as well
  BRT_Train_Pred <- predict.gbm(BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  Max_Train_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Train_Pred <- predict.glm(GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Train_Pred <- predict.gam(GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  #Calculate weights for each model based on AUC
  BRT_weight <- (BRT$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  Max_weight <- (Max$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GLM_weight <- (GLM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GAM_weight <- (GAM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  #Ensemble the predictions by AUC weighting
  ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
  ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
  
  #Add a column to the testing and training data for the predictions
  Kelp_Testing_Data$pred <- ENS_Pred
  Kelp_Training_Data$pred <- ENS_Train_Pred
  
  #Calculate the testing AUC
  roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
  auc_Test <- pROC::auc(roc_Test)
  #Calculate the training AUC
  roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
  auc_Train <- pROC::auc(roc_Train)
  #Calculate the AUC difference between training data and testing fold
  AUCdiff <- auc_Train - auc_Test
  #Calculate the 10% omission error threshold for the training data
  Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
  Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
  
  
  #Calculate testing AUC for individual models
  BRT_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, BRT_Pred))
  Max_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, Max_Pred))
  GLM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GLM_Pred))
  GAM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GAM_Pred))
  
  
  
  #Create a dataframe containing AUC, AUCdiff, and Omission Error and return this
  Metrics <- data.frame(as.numeric(auc_Train), as.numeric(auc_Test), as.numeric(AUCdiff), as.numeric(Omission_Error))
  colnames(Metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")
  
  
  #Create null models using the same parameters and randomly drawn presence points for the enviro_buffer dataframe (same background points)
  
  #Set up a dataframe to hold all of the evaluation metrics for the null models
  Null_metrics <- data.frame(0,0,0,0)
  colnames(Null_metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")
  
  #Run 1000 null models
  for (j in 1:10){
    
    #Perform a random pull from the environmental dataframe for each presence point in the training dataset
    for (i in 1:nrow(Kelp_Training_Data)){
      if (Kelp_Training_Data$Occurrence[i] == 1){
        set.seed(313*j+round(46*i))
        randnum <- sample(seq_len(nrow(Survey_Enviro_df)), size = 1)
        Kelp_Training_Data[i, 2:(Predictor_num+1)] <- Survey_Enviro_df[randnum,]
      }
    }
    
    #Fit an ensemble null model to the new dataset using all the same parameters as for the actual models
    #BRT
    Null_BRT <- flexsdm::fit_gbm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), n_trees = BRT$model$n.trees, n_minobsinnode = BRT$model$n.minobsinnode, shrinkage = BRT$model$shrinkage)
    #MaxENT
    Null_Max <- flexsdm::fit_max(data = Kelp_Training_Data %>% dplyr::filter(Occurrence == 1), response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", background = Kelp_Training_Data %>% dplyr::filter(Occurrence == 0), thr = c("equal_sens_spec"), clamp = FALSE, regmult = Max$performance$regmult, classes = Max$performance$classes)
    #GLM
    Null_GLM <- flexsdm::fit_glm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GLM$model$formula, poly = 1, inter_order = 0)
    #GAM
    Null_GAM <- flexsdm::fit_gam(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GAM$model$formula, k = 3)
    
    #Fit an ensemble of all null methods
    Null_ENS <- flexsdm::fit_ensemble(models = list(Null_BRT, Null_Max, Null_GLM, Null_GAM), ens_method = "meanw", thr = c("equal_sens_spec"), thr_model = "equal_sens_spec", metric = "AUC")
    
    #Calculate null evaluation metrics
    
    #Predict to the testing dataframe with each individual model included in the ensemble model
    BRT_Pred <- predict.gbm(Null_BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    Max_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
    GLM_Pred <- predict.glm(Null_GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    GAM_Pred <- predict.gam(Null_GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
    #Predict to the training data as well
    BRT_Train_Pred <- predict.gbm(Null_BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    Max_Train_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
    GLM_Train_Pred <- predict.glm(Null_GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    GAM_Train_Pred <- predict.gam(Null_GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
    #Ensemble the predictions by AUC weighting
    ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
    ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
    
    #Add a column to the testing and training data for the predictions
    Kelp_Testing_Data$pred <- ENS_Pred
    Kelp_Training_Data$pred <- ENS_Train_Pred
    
    #Calculate the testing AUC
    roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
    auc_Test <- pROC::auc(roc_Test)
    #Calculate the training AUC
    roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
    auc_Train <- pROC::auc(roc_Train)
    #Calculate the AUC difference between training data and testing fold
    AUCdiff <- auc_Train - auc_Test
    #Calculate the 10% omission error threshold for the training data
    #Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
    Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
    
    #Save the null metrics to the dataframe
    Null_metrics[j,1] <- as.numeric(auc_Train)
    Null_metrics[j,2] <- as.numeric(auc_Test)
    Null_metrics[j,3] <- as.numeric(AUCdiff)
    Null_metrics[j,4] <- as.numeric(Omission_Error)
  }
  return(list(Metrics, Null_metrics))
}

Model_Eval_catch <- function(ENS, Kelp_Training_Data, Kelp_Testing_Data, Survey_Enviro_df, BRT, Max, GLM, GAM, Predictor_num){
  
  set.seed(2303)
  
  # Predict to the testing dataframe with each individual model included in the ensemble model
  BRT_Pred <- predict.gbm(BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  Max_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Pred <- predict.glm(GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Pred <- predict.gam(GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
  
  # Predict to the training data as well
  BRT_Train_Pred <- predict.gbm(BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  Max_Train_Pred <- predictMaxNet(Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
  GLM_Train_Pred <- predict.glm(GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  GAM_Train_Pred <- predict.gam(GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
  
  # Calculate weights for each model based on AUC
  BRT_weight <- (BRT$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  Max_weight <- (Max$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GLM_weight <- (GLM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  GAM_weight <- (GAM$performance$AUC_mean)/ (BRT$performance$AUC_mean + Max$performance$AUC_mean + GLM$performance$AUC_mean + GAM$performance$AUC_mean)
  
  # Ensemble the predictions by AUC weighting
  ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
  ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
  
  # Add a column to the testing and training data for the predictions
  Kelp_Testing_Data$pred <- ENS_Pred
  Kelp_Training_Data$pred <- ENS_Train_Pred
  
  # Calculate the testing AUC
  roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
  auc_Test <- pROC::auc(roc_Test)
  
  # Calculate the training AUC
  roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
  auc_Train <- pROC::auc(roc_Train)
  
  # Calculate the AUC difference between training data and testing fold
  AUCdiff <- auc_Train - auc_Test
  
  # Calculate the 10% omission error threshold for the training data
  Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
  Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
  
  # Calculate testing AUC for individual models
  BRT_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, BRT_Pred))
  Max_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, Max_Pred))
  GLM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GLM_Pred))
  GAM_AUCtest <- pROC::auc(pROC::roc(Kelp_Testing_Data$Occurrence, GAM_Pred))
  
  # Create a dataframe containing AUC, AUCdiff, and Omission Error and return this
  Metrics <- data.frame(as.numeric(auc_Train), as.numeric(auc_Test), as.numeric(AUCdiff), as.numeric(Omission_Error))
  colnames(Metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")
  
  # Create null models using the same parameters and randomly drawn presence points for the enviro_buffer dataframe (same background points)
  
  # Set up a dataframe to hold all of the evaluation metrics for the null models
  Null_metrics <- data.frame(0,0,0,0)
  colnames(Null_metrics) <- c("AUC_Train", "AUC_Test", "AUCdiff", "OErr")
  
  # Run 1000 null models
  for (j in 1:10){
    
    # Perform a random pull from the environmental dataframe for each presence point in the training dataset
    for (i in 1:nrow(Kelp_Training_Data)){
      if (Kelp_Training_Data$Occurrence[i] == 1){
        set.seed(313*j+round(46*i))
        randnum <- sample(seq_len(nrow(Survey_Enviro_df)), size = 1)
        Kelp_Training_Data[i, 2:(Predictor_num+1)] <- Survey_Enviro_df[randnum,]
      }
    }
    
    # Use tryCatch to handle errors during model fitting
    tryCatch({
      # Fit an ensemble null model to the new dataset using all the same parameters as for the actual models
      # BRT
      Null_BRT <- flexsdm::fit_gbm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), n_trees = BRT$model$n.trees, n_minobsinnode = BRT$model$n.minobsinnode, shrinkage = BRT$model$shrinkage)
      
      # MaxENT
      Null_Max <- flexsdm::fit_max(data = Kelp_Training_Data %>% dplyr::filter(Occurrence == 1), response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", background = Kelp_Training_Data %>% dplyr::filter(Occurrence == 0), thr = c("equal_sens_spec"), clamp = FALSE, regmult = Max$performance$regmult, classes = Max$performance$classes)
      
      # GLM
      Null_GLM <- flexsdm::fit_glm(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GLM$model$formula, poly = 1, inter_order = 0)
      
      # GAM
      Null_GAM <- flexsdm::fit_gam(data = Kelp_Training_Data, response = "Occurrence", predictors = colnames(Kelp_Training_Data[2:(Predictor_num+1)]), partition = "fold", thr = c("equal_sens_spec"), fit_formula = GAM$model$formula, k = 3)
      
      # Fit an ensemble of all null methods
      Null_ENS <- flexsdm::fit_ensemble(models = list(Null_BRT, Null_Max, Null_GLM, Null_GAM), ens_method = "meanw", thr = c("equal_sens_spec"), thr_model = "equal_sens_spec", metric = "AUC")
      
    }, error = function(e) {
      # If an error occurs, print a message and skip this iteration
      message(paste("Error fitting model iteration", j, ":", e$message))
    })
    
    # If the fitting was successful, calculate metrics, otherwise, skip to the next iteration
    if (exists("Null_BRT") && exists("Null_Max") && exists("Null_GLM") && exists("Null_GAM")) {
      # Calculate null evaluation metrics
      
      # Predict to the testing dataframe with each individual model included in the ensemble model
      BRT_Pred <- predict.gbm(Null_BRT$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
      Max_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Testing_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
      GLM_Pred <- predict.glm(Null_GLM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
      GAM_Pred <- predict.gam(Null_GAM$model, Kelp_Testing_Data[,2:(Predictor_num+1)], type = "response")
      
      # Predict to the training data as well
      BRT_Train_Pred <- predict.gbm(Null_BRT$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
      Max_Train_Pred <- predictMaxNet(Null_Max$model, data.frame(Kelp_Training_Data[,2:(Predictor_num+1)]), TRUE, "cloglog")
      GLM_Train_Pred <- predict.glm(Null_GLM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
      GAM_Train_Pred <- predict.gam(Null_GAM$model, Kelp_Training_Data[,2:(Predictor_num+1)], type = "response")
      
      #Ensemble the predictions by AUC weighting
      ENS_Pred <- BRT_weight*BRT_Pred + Max_weight*Max_Pred + GLM_weight*GLM_Pred + GAM_weight*GAM_Pred
      ENS_Train_Pred <- BRT_weight*BRT_Train_Pred + Max_weight*Max_Train_Pred + GLM_weight*GLM_Train_Pred + GAM_weight*GAM_Train_Pred
      
      #Add a column to the testing and training data for the predictions
      Kelp_Testing_Data$pred <- ENS_Pred
      Kelp_Training_Data$pred <- ENS_Train_Pred
      
      #Calculate the testing AUC
      roc_Test <- pROC::roc(Kelp_Testing_Data$Occurrence, ENS_Pred)
      auc_Test <- pROC::auc(roc_Test)
      #Calculate the training AUC
      roc_Train <- pROC::roc(Kelp_Training_Data$Occurrence, ENS_Train_Pred)
      auc_Train <- pROC::auc(roc_Train)
      #Calculate the AUC difference between training data and testing fold
      AUCdiff <- auc_Train - auc_Test
      #Calculate the 10% omission error threshold for the training data
      #Omission_10 <- as.numeric(quantile((Kelp_Training_Data %>% dplyr::filter(Occurrence == 1))$pred, 0.1))
      Omission_Error <- nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1 & pred < Omission_10))/ nrow(dplyr::filter(Kelp_Testing_Data, Occurrence == 1))
      
      #Save the null metrics to the dataframe
      Null_metrics[j,1] <- as.numeric(auc_Train)
      Null_metrics[j,2] <- as.numeric(auc_Test)
      Null_metrics[j,3] <- as.numeric(AUCdiff)
      Null_metrics[j,4] <- as.numeric(Omission_Error)
    }
  }
  
  # Calculate final model metrics and return results
  return(list(Metrics = Metrics, Null_metrics = Null_metrics))
}


