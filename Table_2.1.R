#Creating Table 2.1

library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(terra)
library(sf)
library(ggspatial)
library(ggplot2)
library(cowplot)
library(dplyr)
library(forcats)
library(patchwork)
library(grid)

#Read in different datasets 
presence_data <- read.csv("./processed_data/Survey_Datasets/presence_only.csv")
RA_data <- read.csv("./processed_data/Survey_Datasets/relative_density.csv")
count_data <- read.csv("./processed_data/Survey_Datasets/actual_density.csv")

#Rename DFO 
presence_data <- presence_data %>%
  mutate(survey_type = case_when(
    survey_type %in% c("RSU_RESM", "RSU_RES", "RSU_RMS", "RSU_RBS", "RSU_RBB", "GSU_GSS", "DFO_MSP", "DFO_BHM",
                       "Multispecies_Multispecies") ~ "DFO",
    TRUE ~ survey_type
  ))

RA_data <- RA_data %>%
  mutate(survey_type = case_when(
    survey_type %in% c("RSU_RESM", "RSU_RES", "RSU_RMS", "RSU_RBS", "RSU_RBB", "GSU_GSS", "DFO_MSP", "DFO_BHM",
                       "Multispecies_Multispecies") ~ "DFO",
    TRUE ~ survey_type
  ))

count_data <- count_data %>%
  mutate(survey_type = case_when(
    survey_type %in% c("RSU_RESM", "RSU_RES", "RSU_RMS", "RSU_RBS", "RSU_RBB", "GSU_GSS", "DFO_MSP", "DFO_BHM",
                       "Multispecies_Multispecies") ~ "DFO",
    TRUE ~ survey_type
  ))

#PURPLE URCHINS 

#Occurrence
#Filter for purple urchins, count per method
purple_presence <- presence_data %>%
  filter(species == "PUR") #6077

DFO_purple_presence <- purple_presence %>%
  filter(survey_type == "DFO") #5718

RL_purple_presence <- purple_presence %>%
  filter(survey_type == "RL_Survey") #52

Hakai_purple_presence <- purple_presence %>%
  filter(survey_type == "Hakai") #12

ROV_purple_presence <- purple_presence %>%
  filter(survey_type == "Baum_ROV") #295

#RED URCHIN 

#Occurrence
red_presence <- presence_data %>%
  filter(species == "RUR") #63,051

DFO_red_presence <- red_presence %>%
  filter(survey_type == "DFO") #61912

RL_red_presence <- red_presence %>%
  filter(survey_type == "RL_Survey") #110

Hakai_red_presence <- red_presence %>%
  filter(survey_type == "Hakai") #119

ROV_red_presence <- red_presence %>%
  filter(survey_type == "Baum_ROV") #910

#Count 
red_count <- count_data %>%
  filter(species == "RUR") #138,622

DFO_red_count <- red_count %>%
  filter(survey_type == "DFO") #138,312

RL_red_count <- red_count %>%
  filter(survey_type == "RL_Survey") #130

Hakai_red_count <- red_count %>%
  filter(survey_type == "Hakai") #180

ROV_red_count <- red_count %>%
  filter(survey_type == "Baum_ROV") #0

#RA 
red_RA <- RA_data %>%
  filter(species == "RUR") #40197

DFO_red_RA <- red_RA %>%
  filter(survey_type == "DFO") #36325

RL_red_RA <- red_RA %>%
  filter(survey_type == "RL_Survey") #0

Hakai_red_RA <- red_RA %>%
  filter(survey_type == "Hakai") #0

ROV_red_RA <- red_RA %>%
  filter(survey_type == "Baum_ROV") #3872

#GREEN URCHIN 

#Occurrence
green_presence <- presence_data %>%
  filter(species == "GUR") #35100

DFO_green_presence <- green_presence %>%
  filter(survey_type == "DFO") #34574

RL_green_presence <- green_presence %>%
  filter(survey_type == "RL_Survey") #56

Hakai_green_presence <- green_presence %>%
  filter(survey_type == "Hakai") #43

ROV_green_presence <- green_presence %>%
  filter(survey_type == "Baum_ROV") #427

#Count - should be same as red counts
green_count <- count_data %>%
  filter(species == "GUR") #138,622 (all species)

DFO_green_count <- green_count %>%
  filter(survey_type == "DFO") #138,312

RL_green_count <- green_count %>%
  filter(survey_type == "RL_Survey") #130

Hakai_green_count <- green_count %>%
  filter(survey_type == "Hakai") #180

ROV_green_count <- green_count %>%
  filter(survey_type == "Baum_ROV") #0

#RA - should be same as reds
green_RA <- RA_data %>%
  filter(species == "RUR") #40197

DFO_green_RA <- green_RA %>%
  filter(survey_type == "DFO") #36325

RL_green_RA <- green_RA %>%
  filter(survey_type == "RL_Survey") #0

Hakai_green_RA <- green_RA %>%
  filter(survey_type == "Hakai") #0

ROV_green_RA <- green_RA %>%
  filter(survey_type == "Baum_ROV") #3872



