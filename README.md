READ ME - Urchin Species Distribution Modelling

Results and data are not included in this repository, as they are subject to data sharing agreements. Please refer to Rehill (2025) MSc thesis, available through the University of Victoria archives.

Packages used: 
tidyverse, cowplot, tidyterra, ggspatial, terra, sf, tidync, factoextra, viridis, ncmeta, readxl, dplyr, tidyr,lubridate, patchwork 
terra, sf, tidyterra, ggspatial, blockCV, gbm, dismo, mop, foreach, doParallel, caret, CAST, viridian, ggExtra, gridExtra, 
flexsdm, kernlab, mccv, enmSdmX, biomod2, ggtext, usdm, ggsci, 



##############################################################################################################

1_BIIGLE_Dataset_Creation.Rmd
•	Combines multiple zip files (output of BIIGLE software) into a single csv file
•	Processes annotations relating to urchin presence/absence and relative abundance across depth bins
•	Associates urchin annotations with depth and substrate classes based on frame times, adds information about depth (min/max) and substrate type to each urchin record
•	Adds metadata to these records (coordinates + site info)
•	Corrects depth bins (based on DFO) from true urchin counts 

Inputs: 
	- All BIIGLE files (from BIIGLE site, select project, download report, and new directory)
	- transect_summary.csv (transect metadata) - raw data 
	- 2022_ROV_urchin_count.csv (manually counted urchins) - raw data 
	- 2023_ROV_urchin_count.csv (manually counted urchins) - raw data 


Outputs: 
	- data.csv (aggregation of BIIGLE files) - raw data 
	- Baum_ROV_Urchin_Records.csv - processed data 
	- species_by_Transect_Depth.csv - processed data 
	- Baum_ROV_Urchin_Counts.csv (corrected count transects) - processed_data 

##############################################################################################################

2_Generating_Datasets.Rmd
Combines different data surveys to produce final combined usable dataset 

Hakai summaries
-filter for rocky reef surveys only (ones with depth), summarize counts, presence/absence, 
relative density, and actual density of urchins to quadrat AND site level
- divide counts by number of quadrats (each quadrat 1m^2 surveyed) to get density
- Quadrat level -  NA values for min/max depth, substrate, end lat/lon, not included in data
- includes quadrats with NO urchins 

RLS 
- filter RLS for only data with depth associations
- summarize counts, presence/absence, relative density and actual density of urchins to
transect level (merge both blocks)
- Depth associated singularly per transect
- Take total surveyed area from 1 transect to be 100m^2 (divide counts by 100 to get density)
- NA values for substrate, end lat/lon, min_depth, max_depth not included in data 

DFO Count Data (includes GSU, MSp, RSU) summaries 
- filter out all NA in depth and urchin counts
- creates unique identifier for transects based on Method, Year, Month, Day, Lat, Lon and TransectNum
- creates unique identifier for quadrats based on Method, Year, Month, Day, LatitudeStart, LongitudeStart, TransectNum, QuadratNum
- Summarize counts, presence-absence, relative and actual density of urchins to quadrat level (which is the initial formating anyways)
- Matches metadata (substrate, end_lat, start_lat) from other survey datasets (that do not include urchin counts)
- Combined these datasets based on survey method, start lat/lon, depth, year/month/day, and quadrat number
- No information added from RSU surveys (do not have this data available)
- Summarize counts, presence-absence, relative and actual density of urchins to transect level 
- determine min, max and mean depth for each transect 
- transect level actual density based on number of quadrats per unique transect (so total number of urchins / number of quadrats)
- NA values for min/max depth for quadrats (this data exists for transect summaries because
you can take the min/max from quadrats for a given transect)
- Added substrate categories based on GSU, MSP supplementals. If not found there, then NA
- Take most 'dominant' substrate per transect 

DFO Relative Count Data (includes BHM, MSp) 
-Summarizes quadrat data to most dominant (most common) substrate per transect, and depth to  minimum, maximum and mean for each transect. Then, merges this information to the transect level dataframe 
- Combine additional BHM dataset by HKey, combine additional MSP dataset by Start Lat/Lon
- Note BHM dataset does not have information on purple urchins (NA)
- Filtered out records without substrate info (based on Matt's code), but maybe this needs to be changed
- binned all min/max depth similarly to ROV transect bins 


Inputs
	- quadrats_data.csv (Hakai quadrates) from Ondine Pontier - raw data 
	  source: https://catalogue.hakai.org/dataset/ca-cioos_314a0846-0fe9-4c2e-81e2-d2b24ac98b6e
	- hakai_sites.csv (metadata on Hakai site location) - raw data 
	- IMOS_2.csv (Reef Life Survey) - raw data 
	  source: https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/			48cf3cb9-caa9-4633-9baa-8bba3c4d904a
	- Urchin_Count.csv (DFO count data, includes GSU, RSU, Multispecies) - raw data 
	- BHM_PA_algae_update.csv (DFO - benthic habitat mapping) - raw data 
	- MSp_habitat_final.csv (DFO - multispecies) (DFO Multispecies surveys mapping metadata) - raw data 
	- GSU_habitat.csv (DFO- GSU) (DFO GSU metadata) - raw data 
	- MERGED_TRANSECT_RA.csv (DFO) (DFO transect data with relative abundance for MSp and BHM) - raw data 
	- Baum_ROV_Urchin_Counts.csv (Baum lab ROV survey data, BIIGLE_Dataset_Organization output) - processed data (see Note below) 

Outputs
	- hakai_quadrat_summary.csv - processed data 
	- hakai_site_summary.csv - processed data 
	- RLS_transect_summary.csv - processed data 
	- dfo_quadrat_summary.csv - processed data 
	- dfo_transect_summary.csv - processed data 

	DATASETS
	- presence_absence.csv (includes all surveys - ROV, RLS, Hakai, DFO-RSU, DFO-GSU, DFO-Msp, DFO-BHM)
	- presence_only.csv (includes all surveys - ROV, RLS, Hakai, DFO-RSU, DFO-GSU, DFO-Msp, DFO-BHM)
	- actual_density.csv (includes Hakai, RLS, DFO-RSU, DFO-GSU, DFO_MSp)
	- relative_desnsity.csv (includes only ROV and DFO-Msp, DFO-BHM) 


NOTE : second directory (SDM_Urchin_Models_2) does NOT include BATI data 

##############################################################################################################

3_Generating_Predictor_Layers.Rmd

•	prepares a suite of environmental raster layers (topobathymetry, slope, wave exposure, substrate, and currents) to be used as predictors in SDMs. Ensures all layers are at a common spatial resolution (100 m), by aggregating to the mean of all values within a cell, projected consistently, and clipped to a specified spatial domain (the BC buffer). Processed layers are saved as .tif files for later use. 
•	Combines multiple data sources (BCCM, Salish Sea Cast DFO, and UBC) into consistent formats and corrects known model biases (notably for oxygen). Applies a bias correction to DO values (subtracts 35.88 mmol/m³) to align model data with observed values. (Note- consulting with Amber Holdsworth for the oxygen, a delta needs to be applied to lower the high oxygen values in the model.  The delta was computed from the median bias between the obs and model profiles = subtracted  35.88 mmol/m3 in all simulations) 
•	Create predictor hindcast climatology for training (1993-2023)  - averages across years
•	Create predictor hindcast climatology for projection (2012-2023) - averages across years
•	Create predictor hindcast climatologies for each year (1993-2023) - averages within years 
•	No data for 2006 - create average of 2005 and 2007 
•	Assess the environmental data for the overall hindcast period for autocorrelation in the variables to determine the ideal size of spatial blocks when splitting data into cross-validation (CV) folds

Notes
- pH not included (Salish Sea cast does not include this variable) 
- Talk (proxy for pH) only has mean in oceanographic models) 
- Low (min) hyposaline conditions more ecologically relevant for urchins 
- ‘Max’ DO compromised by bias corrections 
- Autocorrelation recommends a block size = 

Inputs 
	- WEST_COAST_DEM2.tif (bathymetry layer for all of canada at 10x10) - raw data 
	-  BC_Buffer.shp - raw data 
	- DFO REI model layers - rei_sog.tif, rei_wcvi.tif, rei_qcs.tif, rei_ncc.tif, rei_hg.tif
	 (source: https://open.canada.ca/data/en/dataset/9bcdc2c8-1b32-4433-97d0-a98fc2ea2e51_ ) - raw data 
	- Substrate class layers - SOG_substrate_20m.tif, WCVI_substrate_20m.tif, QCS_substrate_20m.tif, 				        NCC_substrate_20m.tif, HG_substrate_20m.tif - raw data 
	- Nearshore_CurrentSpeedIndex.tif - raw data
	- BCCM data (temp, DO, Alk, salinity) - raw data 
	- Salish Sea Cast data  - raw data 

Outputs
	- DEM_100.tif clamped bathymetric layer for west coast, masked to study area, at 100m resolution) - processed data
	- Slope_100.tif (slope layer, 100m resolution) - processed data
	- REI_100.tif (relative exposure index, 100m resolution) - processed data
	- Rock_100.tif (percent rock layer, 100m resolution) - processed data
	- Current_100.tif
	- Predictor_Hindcast_Climatologies_1993-2023.tif - processed data
	- Predictor_Hindcast_Climatologies_2012-2023.tif - processed data
	- Predictor_hindcast_climatology_YEAR.tif (from 1993-2023)  - processed data
	- AutoCorr_Plot.jpeg

Final hindcast layers include the following variables 
- TAlk_10m_mean
- salt_10m_mean
- salt_10m_min
- temp_s_mean
- temp_s_max
- temp_s_min
- temp_10m_mean
- temp_10m_max
- temp_10m_min
- do_10m_mean
- do_10m_min
- Current
- REI
- Rock
- Slope
- Depth

##############################################################################################################

4_Generating_Occurrence_Lines.Rmd 

Plots depth distribution of records (presence, counts, and relative abundance) 
Summarizes urchin occurrence or density (adds) and associates it with a single bathymetry cell (based on closest depth) 
Removes all records above chart depth datum 

Inputs
- presence_only.csv - processed data 
- actual_density.csv - processed data 
- relative_density.csv - processed data 
- BC_Buffer.shp - raw data 
- DEM_100.tif -  processed data 

Outputs
- urchin_presence_depth_plot.jpeg - Plots
- count_urchin_depth_lot.jpeg - Plots
- rd_urchin_depth_lot.jpeg - Plots 
- Urchin_Occurrence_Transects.shp - processed_data 
- Urchin_Occurrence_Transects_Filtered.shp - processed_data 
- Urchin_Count_Transects.shp - processed_data
- Urchin_Count_Transects_Filtered.shp - processed_data
- Urchin_RDensity_Transects.shp - processed_data
- Urchin_RDensity_Transects_Filtered.shp - processed_data


- Presence_SurveyType_Summary.csv
- Count_SurveyType_Summary.csv
- RD_SurveyType_Summary.csv


##############################################################################################################

5_Sampling_v_ENV_Space.Rmd

Creates survey buffers (for PA data ONLY) 
Compares sampled predictor space to that of total hindcast climatology for each response type

Inputs
- presence_only.csv - processed data
- actual_density.csv - processed data
- relative_density.csv - processed data 
- Predictor_Hindcast_Climatologies_1993-2023.tif - processed data
- Predictor_hindcast_climatology_(for years) - processed data 


Outputs
- PA_Predictor_Hindcast_Transect_PCA_Density_Plot.jpeg - Plots
- PA_Predictor_Hindcast_Transect_PCA_Plot_Ellipses.jpeg - Plots
- AD_Predictor_Hindcast_Transect_PCA_Density_Plot.jpeg - Plots
- AD_Predictor_Hindcast_Transect_PCA_Plot_Ellipses.jpeg - Plots
- RD_Predictor_Hindcast_Transect_PCA_Density_Plot.jpeg - Plots
- RD_Predictor_Hindcast_Transect_PCA_Plot_Ellipses.jpeg - Plots
- Survey buffers for each year 



- go through and check that axis titles are correct (%) - need to run script in Q to do this


#############################################################################################################

6_Comparing_Predictor_Influence.Rmd

Interpreting model outputs by comparing predictor influence by species and response variable type 

Inputs 
- GUR_Predictor_Importance.csv
- GURcount_ensemble_variable_importance.csv
- GURord_ensemble_variable_importance.csv
- RUR_Predictor_Importance.csv
- RURcount_ensemble_variable_importance.csv
- RURord_ensemble_variable_importance.csv
- PUR_Predictor_Importance.csv
- PURord_ensemble_variable_importance.csv



Outputs
- GUR_Predictor_Plot.png
- GUR_PredictorClass_Plot.png
- RUR_Predictor_Plot.png
- RUR_PredictorClass_Plot.png
- PUR_Predictor_Plot.png
- PUR_PredictorClass_Plot.png
- Combined_PredictorClass_Plot.png
- Combined_Predictor_Plot.png
- Combined_Species_PI.png
- GUR_RUR_ES_Count_Combined.png
- GUR_RUR_ES_Ordinal_Combined.png



#############################################################################################################

7_Comparing_Distributing.Rmd

Plotting model projections, clustering for visualizations, rotating coastline
Creates congruency maps
Plots map of survey methods among different datasets
 
Inputs 
- Land.shp
- DEM_100.tif
- GUR_Thresholds.csv
- RUR_Thresholds.csv
- PUR_Thresholds.csv
- GUR_LO_Predictions.tif
- PUR_LO_Predictions.tif
- RUR_LO_Predictions.tif
- proj_GUR_current_count_ensembles_GUR_ensemble.tif
- proj_RUR_current_count_ensembles_RUR_ensemble.tif
- proj_GUR_current_ord_ensembles_PUR_ensemble.tif
- proj_RUR_current_ord_ensembles_RUR_ensemble.tif
- proj_PUR_current_ord_ensembles_GUR_ensemble.tif
- proj_GUR_current_count_ClampingMask.tif
- proj_RUR_current_count_ClampingMask.tif
- proj_GUR_current_ord_ClampingMask.tif
- proj_RUR_current_ord_ClampingMask.tif
- proj_PUR_current_ord_ClampingMask.tif
- GUR_Method_Uncertainty.tif
- PUR_Method_Uncertainty.tif
- RUR_Method_Uncertainty.tif
- RUR_Method_Uncertainty_Count.tif
- GUR_Method_Uncertainty_Count.tif
- GUR_Method_Uncertainty_RA.tif
- RUR_Method_Uncertainty_RA.tif
- GUR_Binary_Predictions.tif
- PUR_Binary_Predictions.tif
- RUR_Binary_Predictions.tif



Aggregation factors 
- GUR presence = 5
- PUR presence = 5
- RUR presence = 5



Outputs (same for all 
PRESENCE MAPS
- GUR_Point_Map.png
- GUR_Point_Map_Rotated.png
- GUR_LO_Map_rotated.png
- PUR_Point_Map.png
- PUR_Point_Map_Rotated.png
- PUR_LO_Map_Rotated.png
- RUR_Point_Map.png
- RUR_Point_Map_Rotated.png
- Combined_Presence_Map.png
- Combined_Presence_Continuous_Map.png
- GUR_Model_Uncertainty_Map_Rotate.png
- GUR_Model_Uncertainty_Map_Rotate.png
- RUR_Model_Uncertainty_Map_Rotate.png
- Combined_Presence_Uncertainty_Map.png

COUNT MAPS
- GUR_Abundance_Map.png
- GUR_Abundance_Map_Rotated.png
- RUR_Abundance_Map.png
- RUR_Abundance_Map_Rotated.png
- GUR_Count_Map.png
- RUR_Count_Map.png
- Combined_Count_Map.png
- Combined_Presence_Map.png
- Combined_Abundance_Continuous.png
- GUR_Count_Map_Rotated.png
- RUR_Count_Map_Rotated.png
- Combined_Count_Map.png
- GUR_Model_Uncertainty_Map_Count_Rotate.jpeg
- RUR_Model_Uncertainty_Map_Count_Rotate.jpeg
- Combined_Count_Uncertainty_Map.png

RA MAPS
- GUR_RelativeAbundance_Map.png
- RUR_RelativeAbundance_Map.png
- GUR_RelativeAbundance_Map_Rotated.png
- RUR_RelativeAbundance_Map_Rotated.png
- Combined_RA_Continuous.png
- GUR_Relative_Map.png
- RUR_Relative_Map.png


- Combined_RA_Map.png
- Combined_Maps.png
- GUR_Model_Uncertainty_Map_RA_Rotate.jpeg
- RUR_Model_Uncertainty_Map_RA_Rotate.jpeg
- Combined_RA_Uncertainty_Map.png


- Green_Maps.png
- Red_Maps.png

CONGRUENCY MAPS
- GUR_AlignPresence_Map.png
- GUR_AlignPA_Map.png
- RUR_AlignPresence_Map.png
- RUR_AlignPA_Map.png
- Align_Presence_Maps.png
- Align_Presence_Absence_Maps.png

#############################################################################################################

8_Comparing_Models.Rmd

Run Spearman correlations across binary, count and ordinal models 

Inputs 
- GUR_LO_Predictions.tif
- RUR_LO_Predictions.tif
- PUR_LO_Predictions.tif
- GUR_Binary_Predictions.tif
- PUR_Binary_Predictions.tif
- RUR_Binary_Predictions.tif
- proj_GUR_current_count_ensembles_GUR_ensemble.tif
- proj_RUR_current_count_ensembles_RUR_ensemble.tif
- proj_GUR_current_count_ClampingMask.tif
- proj_RUR_current_count_ClampingMask.tif
- proj_PUR_current_ord_ensembles_PUR_ensemble.tif
- proj_RUR_current_ord_ensembles_RUR_ensemble.tif
- proj_GUR_current_ord_ensembles_GUR_ensemble.tif
- proj_GUR_current_ord_ClampingMask.tif
- proj_RUR_current_ord_ClampingMask.tif
- proj_PUR_current_ord_ClampingMask.tif
- Land.shp
- DEM_100.tif



Outputs
- urchin_correlations_summary.csv


#############################################################################################################
Presence/Absence Models 

Functions
- General_Functions.R
- Spatial_Mapping_Functions_flexSDM.R
- Presence_Point_Creator.R
- Presence_Map_Creator.R
- Pseudo-absence_Creator_flexSDM.R
- Predictor_Coverage_Plotter_flexSDM.R
- PA_Map_Creator_flexSDM.R
- Model_Evaluation_flexSDM.R
- Model_Evaluation_Plots_flexSDM.R
- Model_Prediction_flexSDM_V2.R
- Spatial_Mapping_Functions_flexSDM.R
- Binary_Prediction_flexSDM.R
- Predictor_Importance_flexSDM.R
- Predictor_Evaluation_Strip_flexSDM.R

(1) creates final presence dataset per species, for each year of species surveys generates a map/raster/shapefile of where presences were observed
    Repeats for entire time period. 
(2) associates presence points with environmental information from each associated year 
(3) Generation of pseudo absences for each year presences were observed: generated background points randomly at a rate of 20x what is needed (number of presences), perform
    k-means clustering (ensures environmental variability/stratification) grouping points by environmental similarity and selecting only a single point to use as a PA. 
(4) Cross validation: k-fold (10), Block size (GUR/RUR: 140000, PUR: 90000), repeating block assignment 200 times (to optimize spatial balance), redistribute up to 10% of presence points to folds
    with less than a threshold to balance folds (threshold: GUR = 80, ), creates CAST plot
(5) Plot environmental and geographic coverage of the dataset, compares range of predictor data for each urchin species, build density plots comparing all variable ranges within presence points of         
    each data fold
(6) Fit an ensemble model to training data (algo: GLM, GAM, BRT, Maxent): 
	BRTs: tuned with grid of hyper parameters (shrinkage: 0.05-0.0005 , n.trees: 1000-2000 , n.minobsinnode: 5-40) using AUC as a performance metric
	MAXENT: tuned with grid of hyper parameters (regmult: 0.1-5, classes: linear, linear-quadratic, linear-quadratic-hinge), using AUC as performance metric
	GLM: excluded interaction terms, automated predictor selection (poly = 1, inter-order = 0)
	GAM: (smoothing parameter, k=3) 

	Ensembles were constructed using weighted AUC mean method (individual model predictions are weighted by their AUC values) 
	Final ensemble prediction used threshold where sensitivity and specificity are equal to get binary suitability values (0-1) 

(7) Model Evaluation: calculates the testing and training AUC (how do ensemble models fit to training vs testing datasets, calculate AUC difference (AUC_training - AUC testing) and 
    omission error - low omission error = model does BETTER at predicting true presences, proportion of presence points in the testing dataset that are predicted below 10%, takes 
	10% of predicted values of known presence points and calculates omission error as the proportion of actual presences predicted below this threshold (as unsuitable)
    Same metrics calculated for null models (x 1000), null model points selected randomly from environmental background

(8) Generate final LO predictions and uncertainty: generates rasters of LO and uncertainty, uncertainty determined by generating prediction from each model (algorithm) and determining the 	standard deviation between them 
(9) Determine the optimal threshold for converting LO predictions to binary presence/absence: optimal threshold is determined by TSS and Omission Error, assess 1000 threshold values (between 
	0-1) and assessed the optimal threshold of that 1) minimized OE and 2) maximizes TSS, builds density plot to visualize classification accuracy 

(10) Model/Predictor interpretation: using evaluation strip method + calculate relative importance using model permutation method (100 permutations per predictor), groups predictors into more  	general classes (sums predictor importance among classes). 
	ES method: varies one predictor at a time while holding all other predictors constant (at their mean, min and max) and then generates model predictions, gives response curves that show how LO    	changes in response to changes in that predictor, graphs plotted with standard error 


Inputs 
- BC_Buffer.shp
- GSHHS_f_L1.shp
- Urchin_Occurrence_Transects_Filtered.shp
- DEM_100.tif
- Predictor_hindcast_climatology_ x YEAR .tif 
- Survey_Buffer_ x YEAR .tif 
- Predictor_Hindcast_Climatologies_2012-2023.tif

Inputs (per species, GUR = green, PUR = purple, RUR = red) 
- GUR_Presence_Points_ x YEAR .shp + .tif 

Outputs
- GUR_Presence_Points_ x per YEAR .shp
- GUR_Presence_Raster_ x per YEAR .tif 
- GUR_Pres_Map_ x per YEAR .jpeg
- GUR_Presence_Points.shp (entire time period) 
- GUR_Presence_Raster.tif (entire time period) 
- GUR_Pres_Map.jpeg (entire time period) 
- RDS outputs
- GUR_blocks.csv
- GUR_blocks.shp
- GUR_CAST_Plot.jpeg
- GUR_Predictor_Plot.jpeg
- GUR_PA_Geog_Plot.jpeg
- GUR_Fold_Predictor_density_plot.jpeg
- GUR_Survey_Enviro_df.csv
- GUR_Eval.csv
- GUR_Null.csv
- GUR_Eval_Plot.jpeg
- GUR_SES_Plot.jpeg
- GUR_LO_Predictions.tif
- GUR_Method_Uncertainty.tif
- GUR_LO_Map.jpeg
- GUR_Model_Uncertainty_Map.jpeg
- GUR_Threshold_Plot.jpeg
- GUR_Thresholds.csv
- GUR_Predictions.csv
- GUR_Density_Plot.jpeg
- GUR_Binary_Predictions.tif
- GUR_Binary_Predictions_Map.jpeg
- GUR_Predictor_Importance.csv
- GUR_ES_Plot [Predictor ID].jpeg
- GUR_ES_Plot.jpeg (all) 
- GUR_ES_Preds.csv



A_GUR_Presence_Code.Rmd + R_Green_Urchin_Main.R (adapted to run on DRAC) 

CV_redistribute_new
Model_Eval_catch 


A_PUR_Presence_Code.Rmd + R_Purple_Urchin_Main.R (adapted to run on DRAC) 

CV_redistribute
Model_Eval_NEW

A_RUR_Presence_Code.Rmd + R_Red_Urchin_Main.R (adapted to run on DRAC) 

CV_redistribute_new
Model_Eval_catch 

###########################################################################################################

Count Models 

Functions 
-Count_Mapping_Functions.R
-CV_Functions_flexSDM.R

(1) Create maps, pictures, and raster of observations of urchin actual density per year and overall timepoint 
(2) Matching species continuous observation with annual environmental data. When multiple observations per cell, mean density calculated. 
(3) Remove collinear/correlated variables based on VIF (using usdm package vif_step() + vif_cor() - variables with VIF greater than 10 removed,  
    And pairwise Pearson correlation threshold greater than 0.7 excluded. Resulted in the exclusion of mean surface temperature, mean bottom salinity, 
    And bottom max temperature (for all species). The same predictors were removed from climatology (for projections) 
	
	Note- mean bottom DO was retained due to high explanatory power in occurrence models 
(4) Creating evaluation dataset: using blockCV, random selection, repeating block assignment 200 times (to optimize spatial balance),select random spatial fold to set aside for model evaluation, no  	redistribution
(5): Models chosen
	- GUR: EDIT HERE GLM, RF, GBM, XGBOOST
	- RUR: EDIT HERE GLM, RF, GBM, (GAM, MARS)
(5) Cross validation: k-fold validation, using 10 folds, 
(6) Model tuning: all models were tuned using R-squared as an evaluation metric, hyperparameters selected via grid search
	- GBM - reds: number of trees varied from 1000-3000, interaction depth 3-7,shrinkage 0.001-0.01,  n.minobsinnode 5-10
	- GBM - greens: number of trees varied from 3000-4000, interaction depth 1-2,shrinkage 0.1-0.05,0,01  n.minobsinnode 10, 20, 30
	- GAM: GCV.Cp method, with three knots
	- CTA: minimum tree depth from 5-20
	- Random Forest: varied number of variables selected at each split from 2-5
	- XGBOOST: max depth (2-4), number of rounds (3-5), learning rate (0.01-1), gamma (0 or 0.1)
	- MARS: degrees (1-2), pruning thresholds (20-40)
(7) Models: models run using MAE and Rsquared as evaluation metrics, 100 permutations for variable importance
(8) Single model performance: save + graph single model performance metrics, predictor importance, and response curves 
(9) Ensemble modelling: using weighted mean, only models exceeding an R^2 of the threshold (species specific) included in the ensemble, confidence interval based on 0.05
	- GUR: 0.2
	- RUR: 0.3
(9) Ensemble model performance: save + graph ensemble model performance metrics, predictor importance, and response curves
(10) Evaluates 100 null models using exact same pipelines 
(11) Model projections
(12) Creates plot of null vs. our models


Inputs
- Land.shp
- BC_Buffer.shp
- GSHHS_f_L1.shp
- DEM_100.tif

- Urchin_Count_Transects_Filtered.shp
- Predictor_hindcast_climatology_ x YEAR .tif 
- GUR_Count_Points_ x YEAR .shp


Outputs (same but for RUR) 
- GUR_Count_Points_YYYY .shp 
- GUR_AD_Points_ YYYY .shp
- GUR_AD_Raster_ YYYY .tif
- GUR_AD_Map_ .jpeg
- GUR_AD_Points.shp (whole timepoint) 
- GUR_AD_Raster.tif (whole timepoint)
- GUR_AD_Points.shp (whole timepoint)
- GUR_AD_Map.jpeg
- GURcount_point_summary.csv (evaluation: GUR fold 11, RUR fold #) 
- GURcount_point_summary.png (NOT THE EXACT FOLDS FOR SPATIAL CV, just evaluation) 
- GURcount_blocks.csv
- GURcount_blocks.shp
- single_model_scores.csv
- count_single_variable_importance.csv
- GURcombined_boxplot_model_scores.png
- GUR_response_curves.png
- ensemble_model_scores.csv
- ensemble_variable_importance.csv
- GUR_ensemble_predictor_importance.png
- GURcount_null_model_eval.csv
- GUR_count_projection.png
- GUR_count_projection_ensemble.png
- GUR_count_projection.png + .tif
- GUR_count_projection_ensemble.png + .tif
- GUR_Eval_Plot.jpeg
- GUR_ES_Count.png
- GUR_ES_Count.csv


B_GUR_Count_Code.Rmd + R_Green_Urchin_Count.R (adapted to run on DRAC) 

B_PUR_Count_Code.Rmd + R_Purple_Urchin_Count.R  (adapted to run on DRAC) 

B_RUR_Count_Code.Rmd + R_Red_Urchin_Count.R (adapted to run on DRAC) 


#######################################################################################################

Ordinal (Relative Abundance) Models 
Functions 
-Count_Mapping_Functions.R
-CV_Functions_flexSDM.R

(1) Create shapefiles, maps, pictures, and raster of observations of urchin relative density per year and overall timepoint
(2) Matching species continuous observation with annual environmental data. When multiple observations per cell, ‘average’ abundance category calculated. (1-4 mean numeric changed back to whole integer)
(3) Remove collinear/correlated variables based on VIF (using usdm package vif_step() + vif_cor() - variables with VIF greater than 10 removed,  
    And pairwise Pearson correlation threshold greater than 0.7 excluded. Resulted in the exclusion of mean surface temperature, mean bottom salinity, 
    And bottom max temperature (for all species). The same predictors were removed from climatology (for projections) 
	
	Note- mean bottom DO was retained due to high explanatory power in occurrence models 
(4) Creating evaluation dataset: using blockCV, random selection, repeating block assignment 200 times (to optimize spatial balance),select random spatial fold to set aside for model evaluation, no redistribution. Block size = 90000 for all species 
(5): Models chosen
	- GUR: EDIT HERE: CTA, XGBOOST
	- RUR: EDIT HERE: RF, XGBOOST, MARS
	- PUR: EDIT HERE: #########
(5) Cross validation: k-fold validation, using 10 folds, 
(6) Model tuning: all models were tuned using F1 as an evaluation metric, hyperparameters selected via grid search
	- CTA: minimum tree depth from 5-20
	- Random Forest: varied number of variables selected at each split from 2-5
	- XGBOOST: max depth (2-4), number of rounds (3-5), learning rate (0.01-1), gamma (0 or 0.1)
	- MARS: degrees (1-2), pruning thresholds (20-40)
(7) Models: models run using MAE and F1 as evaluation metrics, 100 permutations for variable importance
(8) Single model performance: save + graph single model performance metrics, predictor importance, and response curves 
(9) Ensemble modelling: using weighted mean, only models exceeding an F1 threshold (species specific) were included in the ensemble, confidence interval based on 0.05
	- GUR: 0.25
	- RUR: 0.45
	- PUR: #####
(9)  Ensemble model performance: save + graph ensemble model performance metrics, predictor importance, and response curves
(10) Evaluates 100 null models using exact same pipelines 
(11) Model projections
(12) Creates plot of null vs. our models



Inputs (same for RUR/PUR)
- BC_Buffer.shp
- GSHHS_f_L1.shp
- Urchin_RDensity_Transects_Filtered.shp
- Land.shp 
- Predictor_hindcast_climatology_YYYY.tif
- GUR_RDensity_Points_YYYY.shp
- Predictor_Hindcast_Climatologies_2012-2023.tif



Outputs (same for RUR/PUR)
- GUR_RDensity_Points_ YYYY.shp
- GUR_RD_Points_ YYYY.shp
- GUR_RD_Raster_ YYYY.tif
- GUR_RD_Map_YYYY.jpeg
- GUR_RD_Points.shp (overall)
- GUR_RD_Raster.tif (overall)
- GUR_RD_Points.shp (overall)
- GUR_Count_Map.jpeg (overall)
- GURord_point_summary.csv
- GURord_blocks.csv
- GURord_blocks.shp
- GURord_single_model_scores.csv
- GURord_single_variable_importance.csv
- GURord_mean_model_scores.png
- GURord_combined_boxplot_model_scores.png
- GURord_single_model_variable_importance.png
- GURord_response_curves.png
- GURord_ensemble_model_scores.csv
- GURord_ensemble_variable_importance.csv
- GURord_ensemble_eval_scores.png
- GURord_ensemble_predictor_importance.png
- GUR_response_curves_ensemble.png
- projections.tif
- GURord_null_model_eval.csv
- GUR_ES_Ordinal.csv
- GUR_ES_Ordinal.png


C_GUR_Ordinal_Code.Rmd + R_Green_Urchin_Ordinal.R (adapted to run on DRAC) 

C_PUR_Ordinal_Code.Rmd + R_Purple_Urchin_Ordinal.R  (adapted to run on DRAC) 

C_RUR_Ordinal_Code.Rmd + R_Red_Urchin_Ordinal.R (adapted to run on DRAC) 


