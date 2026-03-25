#Create a function for prediction and uncertainty quantification
Prediction <- function(ENS, Kelp_Training_Data, Enviro_Layers, BRT, Max, GLM, GAM, k, Predictor_num){
  set.seed(4578)

  #Generate final Ensemble predictions onto the environmental layers
  final_pred <- flexsdm::sdm_predict(models = ENS, pred = Enviro_Layers, nchunk = 1, thr = c("equal_sens_spec"), con_thr = FALSE)
  gc()
  
  #Generate predictions from each individual model and quantify the stdev between their predictions
  #Predict with each model
  BRT_Pred <- flexsdm::sdm_predict(models = BRT, pred = Enviro_Layers, nchunk = 1, thr = c("equal_sens_spec"), con_thr = FALSE)
  gc()
  Max_Pred <- flexsdm::sdm_predict(models = Max, pred = Enviro_Layers, nchunk = 1, thr = c("equal_sens_spec"), con_thr = FALSE)
  gc()
  GLM_Pred <- flexsdm::sdm_predict(models = GLM, pred = Enviro_Layers, nchunk = 1, thr = c("equal_sens_spec"), con_thr = FALSE)
  gc()
  GAM_Pred <- flexsdm::sdm_predict(models = GAM, pred = Enviro_Layers, nchunk = 1, thr = c("equal_sens_spec"), con_thr = FALSE)
  gc()
  
  #Calculate the standard deviation between the output LO layers
  Model_Uncertainty <- terra::stdev(BRT_Pred$gbm$gbm, Max_Pred$max$max, GLM_Pred$glm$glm, GAM_Pred$gam$gam)

  return(list(final_pred$meanw$meanw, Model_Uncertainty))
}