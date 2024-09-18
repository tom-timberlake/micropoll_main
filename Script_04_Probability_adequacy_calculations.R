##################################################################################################################################################################
##############          MICRO-POLL SCRIPT 4 - CALCULATING PROBABILITY OF ADEQUACY FROM NUTRIENT INTAKE DATA         ##############################################
##################################################################################################################################################################

#Clear workspace
rm(list = ls())

##Install required packages
#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(data.table)
library(skimr)
library(openxlsx)
library(reshape2)
library(tidyr)
library(gridExtra)
library(bipartite)
library(dplyr)
library(car)
library(ggridges)
library(ggplot2)
library(cowplot)

######################################################################################
########  Import required datasets
######################################################################################

##Import nutrient intake data - at level of daily intake of each nutrient by respondent
#Original intake data
original_intake_data <- read.csv("output_data/Original_intake_data.csv")

#Original intake data
no_poll_intake_data <- read.csv("output_data/No-poll_intake_data.csv")

#Original intake data
poll_incr_intake_data <- read.csv("output_data/Poll-incr_intake_data.csv")

##Import EAR data
EAR_data <- data.table(read_excel("input_data/estimated_average_requirements.xlsx",
                                  sheet = "EAR", 
                                  na = c("", "---", NA))) 



######################################################################################
########  Set-up function for processing each dataset separately
######################################################################################

process_and_save <- function(data, output_file) {
  
  
  ######################################################################################
  ########  Box-cox transformation of nutrient intakes
  ######################################################################################
  
  #Not needed when using all values throughout the year - this would only be needed when  calculating adequacy with only 2-3 recall events
  
  #####################################################################################
  ########  Calculate mean daily nutrient intake for each respondent
  #####################################################################################
  
  #Replace all age values with the minimum recorded for the participant - this forces all age values to the value at enrollment
  intake_data_min_age <- data %>%
    dplyr::group_by(resp_id) %>%
    dplyr::mutate(age_y_at_recall = min(age_y_at_recall))
  #ungroup()
  
  #We are grouping not only by respondent ID, but also by their pregnancy and lactation status 
  #This means that for individuals who spend part of the year in one state and part of the year in another, we can calculate their probability of adequacy separately for these two time periods
  #Then later we take a mean of all probability of adequacy scores for each individual so that each individual has just one probability of adequacy value
  mean_daily_intakes <- intake_data_min_age %>%
    dplyr::group_by(hh_barcode, resp_id, village_code, resp_cat, sex, currently_pregnant, mother_breastfeeding_2wks) %>%
    dplyr::summarise(across(c("age_y_at_recall" ,"periods_yet", "Energy_Kcal", 
                              "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g",
                              "Insolublefibre_g", "Solublefibre_g", "Calcium_mg", "Iron_mg", 
                              "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg", 
                              "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", 
                              "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg", 
                              "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg"),
                            ~mean(., na.rm = TRUE)))
  
  
  #If 'periods_yet' = >0, transform to 1 - this means that all girls who are listed as having had their first period within the study are categorised as periods_yet = 1
  mean_daily_intakes <- mean_daily_intakes %>%
    dplyr::mutate(periods_yet = ifelse(periods_yet > 0, 1, periods_yet))
  
  
  #####################################################################################
  ########  Create categories to match with EAR data
  ######################################################################################
  
  # Define the logical statements for each category
  mean_daily_intakes_categorised <- mean_daily_intakes %>%
    dplyr::mutate(EAR_category = case_when(
      age_y_at_recall >= 0 & age_y_at_recall <= 0.5 & sex == "m"  ~ "m_0.5",
      age_y_at_recall > 0.5 & age_y_at_recall <= 1 & sex == "m"  ~ "m_1",
      age_y_at_recall > 1 & age_y_at_recall <= 3 & sex == "m"  ~ "m_3",
      age_y_at_recall > 3 & age_y_at_recall <= 6 & sex == "m"  ~ "m_6",
      age_y_at_recall > 6 & age_y_at_recall <= 9 & sex == "m"  ~ "m_9",
      age_y_at_recall > 9 & age_y_at_recall <= 15 & sex == "m"  ~ "m_15",
      age_y_at_recall > 15 & age_y_at_recall <= 18 & sex == "m"  ~ "m_18",
      age_y_at_recall > 18 & age_y_at_recall <= 50 & sex == "m"  ~ "m_50",
      age_y_at_recall > 50 & age_y_at_recall <= 65 & sex == "m"  ~ "m_65",
      age_y_at_recall > 65 & age_y_at_recall <= 100 & sex == "m"  ~ "m_100",
      age_y_at_recall >= 0 & age_y_at_recall <= 0.5 & sex == "f"  ~ "f_0.5",
      age_y_at_recall > 0.5 & age_y_at_recall <= 1 & sex == "f"  ~ "f_1",
      age_y_at_recall > 1 & age_y_at_recall <= 3 & sex == "f"  ~ "f_3",
      age_y_at_recall > 3 & age_y_at_recall <= 6 & sex == "f"  ~ "f_6",
      age_y_at_recall > 6 & age_y_at_recall <= 9 & sex == "f"  ~ "f_9",
      age_y_at_recall > 9 & age_y_at_recall <= 15 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0 & periods_yet == 0 ~ "f_15_pre",
      age_y_at_recall > 9 & age_y_at_recall <= 15 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0 & periods_yet == 1 ~ "f_15_period",
      age_y_at_recall > 15 & age_y_at_recall <= 18 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0 ~ "f_18",
      age_y_at_recall > 18 & age_y_at_recall <= 50 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0  ~ "f_50",
      age_y_at_recall > 50 & age_y_at_recall <= 65 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0  ~ "f_65",
      age_y_at_recall > 65 & age_y_at_recall <= 100 & sex == "f" & currently_pregnant == 0 & mother_breastfeeding_2wks == 0  ~ "f_100",
      sex == "f" & currently_pregnant == 1 & mother_breastfeeding_2wks == 0 ~ "pregnant",
      sex == "f" & mother_breastfeeding_2wks == 1 ~ "lactating",
      TRUE ~ NA_character_)) 
  
  
  
  #####################################################################################
  ########  Merge in EAR data based on identity of each respondent
  ######################################################################################
  
  ##Merge EAR data into dietary recall data
  diet_EAR_data <- merge(x=mean_daily_intakes_categorised, y=EAR_data,by="EAR_category",all.x=TRUE)
  
  #names(diet_EAR_data)
  
  ###########################################################################################
  ########  Calculate Z scores and PA scores for each respondent based on divergence from EAR
  ############################################################################################
  
  #Z-score calculation
  #Use the formula for Z-scores to standardize the difference between observed values and the Estimated Average Requirement (EAR), adjusting by standard deviations
  #This step normalizes the values relative to their distribution, helping you understand how far the observed values are from the EAR.
    Z_scores <- diet_EAR_data %>%
    dplyr::mutate(Calcium_Z = (Calcium_mg - Calcium_EAR) / Calcium_SD,
                  Zinc_Z = (Zinc_mg - Zinc_low_bioav_EAR) / Zinc_low_bioav_SD,
                  VitC_Z = (VitaminC_mg - Vitamin_C_EAR) / Vitamin_C_SD,
                  Thiamin_Z = (ThiaminB1_mg - Thiamin_EAR) / Thiamin_SD,
                  Riboflav_Z = (RiboflavinB2_mg - Riboflavin_EAR) / Riboflavin_SD,
                  Niacin_Z = (NiacinB3_mg - Niacin_EAR) / Niacin_SD,
                  Pyridoxine_Z = (VitaminB6pyridoxine_mg - Vitamin_B6_EAR) / Vitamin_B6_SD,
                  Folate_Z = (FolateTotal_µg - Folate_EAR) / Folate_SD,
                  VitA_Z = (VitARE_µg - Vit_A_EAR) / Vit_A_SD,
                  VitE_Z = (VitaminE_mg - Vit_E_EAR) / Vit_E_SD,
                  VitB12_Z = (VitaminB12_µg - Vit_b12_EAR) / Vit_b12_SD)
  
  #We now apply the pnorm function to the Z-scores to get cumulative probability values, converting the Z-scores into percentiles (or probabilities). 
  #This essentially represents how the observed nutrient levels rank in a normal distribution compared to the EAR.
  PA_scores <- Z_scores %>%
    dplyr::mutate(Calcium_PA = pnorm(Calcium_Z ),
                  Zinc_PA = pnorm(Zinc_Z ),
                  VitC_PA = pnorm(VitC_Z ),
                  Thiamin_PA = pnorm(Thiamin_Z ),
                  Riboflav_PA = pnorm(Riboflav_Z ),
                  Niacin_PA = pnorm(Niacin_Z ),
                  Pyridoxine_PA = pnorm(Pyridoxine_Z ),
                  Folate_PA = pnorm(Folate_Z ),
                  VitA_PA = pnorm(VitA_Z ),
                  VitE_PA = pnorm(VitE_Z ),
                  VitB12_PA = pnorm(VitB12_Z ))
  
  #Iron requirements are not normally distributed so we categorise these manually based on existing data
  # Create a new variable 'iron_PA'
  PA_scores$iron_PA <- NA  # Initialize to NA
  PA_scores$age_cat <- NA
  
  #Define child and adolescent age ranges
  PA_scores$age_cat[PA_scores$resp_cat == "adol_fem" & PA_scores$age_y_at_recall >= 9 & PA_scores$age_y_at_recall <= 13.99999] <- "adol_1"
  PA_scores$age_cat[PA_scores$resp_cat == "adol_fem" & PA_scores$age_y_at_recall >= 14 & PA_scores$age_y_at_recall <= 18.99999] <- "adol_2"
  PA_scores$age_cat[PA_scores$resp_cat == "adol_fem" & PA_scores$age_y_at_recall >= 19 & PA_scores$age_y_at_recall <= 24] <- "adol_3"
  PA_scores$age_cat[PA_scores$resp_cat == "u5_child" & PA_scores$age_y_at_recall <=1] <- "child_1"
  PA_scores$age_cat[PA_scores$resp_cat == "u5_child" & PA_scores$age_y_at_recall >= 1.0001 & PA_scores$age_y_at_recall <= 2.9999] <- "child_2"
  PA_scores$age_cat[PA_scores$resp_cat == "u5_child" & PA_scores$age_y_at_recall >= 3 & PA_scores$age_y_at_recall <= 8] <- "child_3"
  
  
  # Apply Iron PA scores to adult women (resp_cat = 1)
  PA_scores$iron_PA[PA_scores$Iron_mg < 15.91 & PA_scores$resp_cat == "adult_fem"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 15.91 & PA_scores$Iron_mg < 17.59 & PA_scores$resp_cat == "adult_fem"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 17.59 & PA_scores$Iron_mg < 19.64 & PA_scores$resp_cat == "adult_fem"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 19.64 & PA_scores$Iron_mg < 22.41 & PA_scores$resp_cat == "adult_fem"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.41 & PA_scores$Iron_mg < 24.75 & PA_scores$resp_cat == "adult_fem"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.75 & PA_scores$Iron_mg < 26.87 & PA_scores$resp_cat == "adult_fem"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.87 & PA_scores$Iron_mg < 29.07 & PA_scores$resp_cat == "adult_fem"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 29.07 & PA_scores$Iron_mg < 31.55 & PA_scores$resp_cat == "adult_fem"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 31.55 & PA_scores$Iron_mg < 34.69 & PA_scores$resp_cat == "adult_fem"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 34.69 & PA_scores$Iron_mg < 38.97 & PA_scores$resp_cat == "adult_fem"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 38.99 & PA_scores$Iron_mg < 47.00 & PA_scores$resp_cat == "adult_fem"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 47.00 & PA_scores$Iron_mg < 55.78 & PA_scores$resp_cat == "adult_fem"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 55.78 & PA_scores$Iron_mg < 65.63 & PA_scores$resp_cat == "adult_fem"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 65.63 & PA_scores$resp_cat == "adult_fem"] <- 1
  
  # Apply Iron PA scores to adult males (resp_cat = 2)
  PA_scores$iron_PA[PA_scores$Iron_mg < 14.31 & PA_scores$resp_cat == "adult_male"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 14.31 & PA_scores$Iron_mg < 15.46 & PA_scores$resp_cat == "adult_male"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 15.46 & PA_scores$Iron_mg < 16.72 & PA_scores$resp_cat == "adult_male"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.72 & PA_scores$Iron_mg < 18.34 & PA_scores$resp_cat == "adult_male"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.34 & PA_scores$Iron_mg < 19.60 & PA_scores$resp_cat == "adult_male"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 19.60 & PA_scores$Iron_mg < 20.68 & PA_scores$resp_cat == "adult_male"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 20.68 & PA_scores$Iron_mg < 21.73 & PA_scores$resp_cat == "adult_male"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 21.73 & PA_scores$Iron_mg < 22.77 & PA_scores$resp_cat == "adult_male"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.77 & PA_scores$Iron_mg < 23.96 & PA_scores$resp_cat == "adult_male"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 23.96 & PA_scores$Iron_mg < 25.36 & PA_scores$resp_cat == "adult_male"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 25.36 & PA_scores$Iron_mg < 27.70 & PA_scores$resp_cat == "adult_male"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 27.70 & PA_scores$Iron_mg < 29.03 & PA_scores$resp_cat == "adult_male"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 29.03 & PA_scores$Iron_mg < 30.58 & PA_scores$resp_cat == "adult_male"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.58 & PA_scores$resp_cat == "adult_male"] <- 1
  
  
  # Apply Iron PA scores to adolescent girls (resp_cat = 3) for age range 9-13 (=adol1)
  PA_scores$iron_PA[PA_scores$Iron_mg < 11.65 & PA_scores$age_cat == "adol_1"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 11.65 & PA_scores$Iron_mg < 12.98 & PA_scores$age_cat == "adol_1"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 12.98 & PA_scores$Iron_mg < 14.56 & PA_scores$age_cat == "adol_1"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 14.56 & PA_scores$Iron_mg < 16.54 & PA_scores$age_cat == "adol_1"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.54 & PA_scores$Iron_mg < 17.95 & PA_scores$age_cat == "adol_1"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 17.95 & PA_scores$Iron_mg < 19.21 & PA_scores$age_cat == "adol_1"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 19.21 & PA_scores$Iron_mg < 20.39 & PA_scores$age_cat == "adol_1"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 20.39 & PA_scores$Iron_mg < 21.62 & PA_scores$age_cat == "adol_1"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 21.62 & PA_scores$Iron_mg < 22.91 & PA_scores$age_cat == "adol_1"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.91 & PA_scores$Iron_mg < 24.43 & PA_scores$age_cat == "adol_1"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.43 & PA_scores$Iron_mg < 26.59 & PA_scores$age_cat == "adol_1"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.59 & PA_scores$Iron_mg < 28.39 & PA_scores$age_cat == "adol_1"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 28.39 & PA_scores$Iron_mg < 30.04 & PA_scores$age_cat == "adol_1"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.04 & PA_scores$age_cat == "adol_1"] <- 1
  
  
  # Apply Iron PA scores to adolescent girls (resp_cat = 3) for age range 14-19 (=adol2)
  PA_scores$iron_PA[PA_scores$Iron_mg < 16.69 & PA_scores$age_cat == "adol_2"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.69 & PA_scores$Iron_mg < 18.23 & PA_scores$age_cat == "adol_2"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.23 & PA_scores$Iron_mg < 20.21 & PA_scores$age_cat == "adol_2"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 20.21 & PA_scores$Iron_mg < 22.73 & PA_scores$age_cat == "adol_2"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.73 & PA_scores$Iron_mg < 24.75 & PA_scores$age_cat == "adol_2"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.75 & PA_scores$Iron_mg < 26.62 & PA_scores$age_cat == "adol_2"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.62 & PA_scores$Iron_mg < 28.49 & PA_scores$age_cat == "adol_2"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 28.49 & PA_scores$Iron_mg < 30.55 & PA_scores$age_cat == "adol_2"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.55 & PA_scores$Iron_mg < 32.96 & PA_scores$age_cat == "adol_2"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 32.96 & PA_scores$Iron_mg < 36.13 & PA_scores$age_cat == "adol_2"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 36.13 & PA_scores$Iron_mg < 41.56 & PA_scores$age_cat == "adol_2"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 41.56 & PA_scores$Iron_mg < 47.11 & PA_scores$age_cat == "adol_2"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 47.11 & PA_scores$Iron_mg < 53.46 & PA_scores$age_cat == "adol_2"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 53.46 & PA_scores$age_cat == "adol_2"] <- 1
  
  # Apply Iron PA scores to adolescent girls (resp_cat = 3) for age range 19-24 (=adol3)
  PA_scores$iron_PA[PA_scores$Iron_mg < 15.91 & PA_scores$age_cat == "adol_3"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 15.91 & PA_scores$Iron_mg < 17.59 & PA_scores$age_cat == "adol_3"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 17.59 & PA_scores$Iron_mg < 19.64 & PA_scores$age_cat == "adol_3"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 19.64 & PA_scores$Iron_mg < 22.41 & PA_scores$age_cat == "adol_3"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.41 & PA_scores$Iron_mg < 24.75 & PA_scores$age_cat == "adol_3"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.75 & PA_scores$Iron_mg < 26.87 & PA_scores$age_cat == "adol_3"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.87 & PA_scores$Iron_mg < 29.07 & PA_scores$age_cat == "adol_3"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 29.07 & PA_scores$Iron_mg < 31.55 & PA_scores$age_cat == "adol_3"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 31.55 & PA_scores$Iron_mg < 34.69 & PA_scores$age_cat == "adol_3"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 34.69 & PA_scores$Iron_mg < 38.97 & PA_scores$age_cat == "adol_3"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 38.99 & PA_scores$Iron_mg < 47.00 & PA_scores$age_cat == "adol_3"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 47.00 & PA_scores$Iron_mg < 55.78 & PA_scores$age_cat == "adol_3"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 55.78 & PA_scores$Iron_mg < 65.63 & PA_scores$age_cat == "adol_3"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 65.63 & PA_scores$age_cat == "adol_3"] <- 1
  
  
  # Apply Iron PA scores for pregnant women
  PA_scores$iron_PA[PA_scores$Iron_mg < 16.7 & PA_scores$EAR_category == "pregnant"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.7 & PA_scores$Iron_mg < 18.25 & PA_scores$EAR_category == "pregnant"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.25 & PA_scores$Iron_mg < 20.23 & PA_scores$EAR_category == "pregnant"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 20.23 & PA_scores$Iron_mg < 22.75 & PA_scores$EAR_category == "pregnant"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.75 & PA_scores$Iron_mg < 24.77 & PA_scores$EAR_category == "pregnant"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.77 & PA_scores$Iron_mg < 26.64 & PA_scores$EAR_category == "pregnant"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.64 & PA_scores$Iron_mg < 28.51 & PA_scores$EAR_category == "pregnant"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 28.51 & PA_scores$Iron_mg < 30.56 & PA_scores$EAR_category == "pregnant"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.56 & PA_scores$Iron_mg < 32.98 & PA_scores$EAR_category == "pregnant"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 32.98 & PA_scores$Iron_mg < 36.14 & PA_scores$EAR_category == "pregnant"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 36.14 & PA_scores$Iron_mg < 41.58 & PA_scores$EAR_category == "pregnant"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 41.58 & PA_scores$Iron_mg < 47.12 & PA_scores$EAR_category == "pregnant"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 47.12 & PA_scores$Iron_mg < 53.64 & PA_scores$EAR_category == "pregnant"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 53.64 & PA_scores$EAR_category == "pregnant"] <- 1
  
  # Apply Iron PA scores for breastfeeding women
  PA_scores$iron_PA[PA_scores$Iron_mg < 16.7 & PA_scores$EAR_category == "lactating"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.7 & PA_scores$Iron_mg < 18.25 & PA_scores$EAR_category == "lactating"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.25 & PA_scores$Iron_mg < 20.23 & PA_scores$EAR_category == "lactating"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 20.23 & PA_scores$Iron_mg < 22.75 & PA_scores$EAR_category == "lactating"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 22.75 & PA_scores$Iron_mg < 24.77 & PA_scores$EAR_category == "lactating"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.77 & PA_scores$Iron_mg < 26.64 & PA_scores$EAR_category == "lactating"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.64 & PA_scores$Iron_mg < 28.51 & PA_scores$EAR_category == "lactating"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 28.51 & PA_scores$Iron_mg < 30.56 & PA_scores$EAR_category == "lactating"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.56 & PA_scores$Iron_mg < 32.98 & PA_scores$EAR_category == "lactating"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 32.98 & PA_scores$Iron_mg < 36.14 & PA_scores$EAR_category == "lactating"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 36.14 & PA_scores$Iron_mg < 41.58 & PA_scores$EAR_category == "lactating"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 41.58 & PA_scores$Iron_mg < 47.12 & PA_scores$EAR_category == "lactating"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 47.12 & PA_scores$Iron_mg < 53.64 & PA_scores$EAR_category == "lactating"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 53.64 & PA_scores$EAR_category == "lactating"] <- 1
  
  # Apply Iron PA scores for <12 month children
  PA_scores$iron_PA[PA_scores$Iron_mg < 10.9 & PA_scores$age_cat == "child_1"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 10.9 & PA_scores$Iron_mg < 13.1 & PA_scores$age_cat == "child_1"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 13.1 & PA_scores$Iron_mg < 15.7 & PA_scores$age_cat == "child_1"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 15.7 & PA_scores$Iron_mg < 18.9 & PA_scores$age_cat == "child_1"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.9 & PA_scores$Iron_mg < 21.2 & PA_scores$age_cat == "child_1"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 21.2 & PA_scores$Iron_mg < 23.0 & PA_scores$age_cat == "child_1"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 23.0 & PA_scores$Iron_mg < 24.9 & PA_scores$age_cat == "child_1"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.9 & PA_scores$Iron_mg < 26.7 & PA_scores$age_cat == "child_1"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.7 & PA_scores$Iron_mg < 28.6 & PA_scores$age_cat == "child_1"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 28.6 & PA_scores$Iron_mg < 30.9 & PA_scores$age_cat == "child_1"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.9 & PA_scores$Iron_mg < 34.0 & PA_scores$age_cat == "child_1"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 34.0 & PA_scores$Iron_mg < 36.6 & PA_scores$age_cat == "child_1"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 36.6 & PA_scores$Iron_mg < 38.8 & PA_scores$age_cat == "child_1"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 38.8 & PA_scores$age_cat == "child_1"] <- 1
  
  
  # Apply Iron PA scores for 1-3 year children
  PA_scores$iron_PA[PA_scores$Iron_mg < 4.0 & PA_scores$age_cat == "child_2"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 4.0 & PA_scores$Iron_mg < 4.5 & PA_scores$age_cat == "child_2"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 4.5 & PA_scores$Iron_mg < 5.6 & PA_scores$age_cat == "child_2"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 5.6 & PA_scores$Iron_mg < 7.1 & PA_scores$age_cat == "child_2"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 7.1 & PA_scores$Iron_mg < 8.4 & PA_scores$age_cat == "child_2"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 8.4 & PA_scores$Iron_mg < 9.6 & PA_scores$age_cat == "child_2"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 9.6 & PA_scores$Iron_mg < 10.9 & PA_scores$age_cat == "child_2"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 10.9 & PA_scores$Iron_mg < 12.2 & PA_scores$age_cat == "child_2"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 12.2 & PA_scores$Iron_mg < 13.8 & PA_scores$age_cat == "child_2"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 13.8 & PA_scores$Iron_mg < 15.8 & PA_scores$age_cat == "child_2"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 15.8 & PA_scores$Iron_mg < 18.9 & PA_scores$age_cat == "child_2"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 18.9 & PA_scores$Iron_mg < 21.9 & PA_scores$age_cat == "child_2"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 21.9 & PA_scores$Iron_mg < 24.6 & PA_scores$age_cat == "child_2"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 24.6 & PA_scores$age_cat == "child_2"] <- 1
  
  # Apply Iron PA scores for 4-8 year children
  PA_scores$iron_PA[PA_scores$Iron_mg < 4.82 & PA_scores$age_cat == "child_3"] <- 0
  PA_scores$iron_PA[PA_scores$Iron_mg >= 4.82 & PA_scores$Iron_mg < 5.9 & PA_scores$age_cat == "child_3"] <- 0.04
  PA_scores$iron_PA[PA_scores$Iron_mg >= 5.9 & PA_scores$Iron_mg < 7.45 & PA_scores$age_cat == "child_3"] <- 0.07
  PA_scores$iron_PA[PA_scores$Iron_mg >= 7.45 & PA_scores$Iron_mg < 9.48 & PA_scores$age_cat == "child_3"] <- 0.15
  PA_scores$iron_PA[PA_scores$Iron_mg >= 9.48 & PA_scores$Iron_mg < 11.3 & PA_scores$age_cat == "child_3"] <- 0.25
  PA_scores$iron_PA[PA_scores$Iron_mg >= 11.3 & PA_scores$Iron_mg < 13.07 & PA_scores$age_cat == "child_3"] <- 0.35
  PA_scores$iron_PA[PA_scores$Iron_mg >= 13.07 & PA_scores$Iron_mg < 14.83 & PA_scores$age_cat == "child_3"] <- 0.45
  PA_scores$iron_PA[PA_scores$Iron_mg >= 14.83 & PA_scores$Iron_mg < 16.74 & PA_scores$age_cat == "child_3"] <- 0.55
  PA_scores$iron_PA[PA_scores$Iron_mg >= 16.74 & PA_scores$Iron_mg < 19.01 & PA_scores$age_cat == "child_3"] <- 0.65
  PA_scores$iron_PA[PA_scores$Iron_mg >= 19.01 & PA_scores$Iron_mg < 21.92 & PA_scores$age_cat == "child_3"] <- 0.75
  PA_scores$iron_PA[PA_scores$Iron_mg >= 21.92 & PA_scores$Iron_mg < 26.35 & PA_scores$age_cat == "child_3"] <- 0.85
  PA_scores$iron_PA[PA_scores$Iron_mg >= 26.35 & PA_scores$Iron_mg < 30.46 & PA_scores$age_cat == "child_3"] <- 0.92
  PA_scores$iron_PA[PA_scores$Iron_mg >= 30.46 & PA_scores$Iron_mg < 34.27 & PA_scores$age_cat == "child_3"] <- 0.96
  PA_scores$iron_PA[PA_scores$Iron_mg >= 34.27 & PA_scores$age_cat == "child_3"] <- 1
  
  
  #####################################################################################
  ########  Summarise probability of adequacy data for each participant
  ######################################################################################
  
  #We summarise the probability of adequacy scores at the participant level which means that if a woman spent part of the year pregnant and part of the year not pregnant
  #her probability of adequacy will  be calculated separately in those two different states and a mean PA score will be calculated 
  PA_summary <- PA_scores %>%
    dplyr::group_by(resp_id, resp_cat) %>%
    dplyr::summarise(across(c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                              "Pyridoxine_PA", "Folate_PA", "VitA_PA", "VitE_PA", "VitB12_PA", "iron_PA"),
                            ~mean(., na.rm = TRUE)))
  
  
  #####################################################################################
  ########  Calculate Mean Probability of Adequacy (MPA) scores for each participant 
  ######################################################################################
  
  #This is a summary of their adequacy across 11 key micronutrients
  PA_summary$MPA <- rowMeans(PA_summary[, c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                                            "Pyridoxine_PA", "Folate_PA", "VitA_PA",  "VitB12_PA", "iron_PA")], na.rm = TRUE) #MPA is based on 11 micronutrients and NOT vit E
  

  #####################################################################
  ##### End the function and save each output dataset separately
  #####################################################################
  
  # Save summary results
  write.csv(PA_summary, paste0(output_file), row.names = FALSE)
  
  # Return processed dataframes
  return(list(PA_scores = PA_scores, PA_summary = PA_summary))
  
}

# Assuming you have three datasets named dataset1, dataset2, and dataset3
datasets <- list(original_intake_data, no_poll_intake_data, poll_incr_intake_data)
output_files <- c("output_data/Prob_adequacy_original.csv", "output_data/Prob_adequacy_poll_loss.csv", "output_data/Prob_adequacy_poll_incr.csv")


# Apply the function to each dataset and save the outputs
for (i in seq_along(datasets)) {
  print(paste("Processing dataset", i))
  processed_data <- process_and_save(datasets[[i]], output_files[i])
}

#Load the three datasets as vectors
PA_scores_original <- read.csv("output_data/Prob_adequacy_original.csv")
PA_scores_no_poll <- read.csv("output_data/Prob_adequacy_poll_loss.csv")
PA_scores_poll_incr <- read.csv("output_data/Prob_adequacy_poll_incr.csv")


#########################################################################
##Summarise probability of adequacy for each group - original diets #####
#########################################################################

#Creating summary table of probability of adequacy based on original (observed) intakes
PA_summary_resp_cat_original_table <- PA_scores_original %>%
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise(across(c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                            "Pyridoxine_PA", "Folate_PA", "VitA_PA", "VitE_PA", "VitB12_PA", "iron_PA", "MPA"), 
                          list(mean = ~mean(.), sd = ~sd(.), n = ~length(.))))

#Reshape the summary table
reshaped_data <- PA_summary_resp_cat_original_table %>%
  pivot_longer(
    cols = -resp_cat,  # Pivot all columns except resp_cat
    names_to = "nutrients",  # Name the new column 'nutrients'
    values_to = "value"  # Name the new column for values as 'value'
  ) %>%
  pivot_wider(
    names_from = resp_cat,  # Pivot the respondent categories to columns
    values_from = value  # Fill the new columns with values
  )

final_table <- reshaped_data %>%
  separate(nutrients, into = c("Nutrient", "Stat"), sep = "_PA_")

#Export probability of adequacy summary table
write.csv(final_table, "output_data/Probability of adequacy_resp_cat.csv")


#Create summary table with mean values only
PA_summary_resp_cat_original <- PA_scores_original %>%
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise(across(c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                            "Pyridoxine_PA", "Folate_PA", "VitA_PA", "VitE_PA", "VitB12_PA", "iron_PA", "MPA"),
                          ~mean(.)))

#Convert to long format
PA_summary_original_long <- PA_summary_resp_cat_original %>%
  pivot_longer(cols = -c(resp_cat), names_to = "nutrient", values_to = "prob_adeq")

#Create new column to denote the pollinator scenario
PA_summary_original_long$scenario <- "original"

#########################################################################
### Summarise probability of adequacy for each group - poll loss    #####
#########################################################################

PA_summary_resp_cat_no_poll <- PA_scores_no_poll %>%
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise(across(c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                            "Pyridoxine_PA", "Folate_PA", "VitA_PA", "VitE_PA", "VitB12_PA", "iron_PA", "MPA"),
                          ~mean(.)))

#Convert to long format
PA_summary_no_poll_long <- PA_summary_resp_cat_no_poll %>%
  pivot_longer(cols = -c(resp_cat), names_to = "nutrient", values_to = "prob_adeq")

#Create new column to denote the pollinator scenario
PA_summary_no_poll_long$scenario <- "no_poll"

#########################################################################
### Summarise probability of adequacy for each group - poll incr    #####
#########################################################################

PA_summary_resp_cat_poll_incr <- PA_scores_poll_incr %>%
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise(across(c("Calcium_PA", "Zinc_PA", "VitC_PA", "Thiamin_PA", "Riboflav_PA", "Niacin_PA", 
                            "Pyridoxine_PA", "Folate_PA", "VitA_PA", "VitE_PA", "VitB12_PA", "iron_PA", "MPA"),
                          ~mean(.)))

#Convert to long format
PA_summary_poll_incr_long <- PA_summary_resp_cat_poll_incr %>%
  pivot_longer(cols = -c(resp_cat), names_to = "nutrient", values_to = "prob_adeq")

#Create new column to denote the pollinator scenario
PA_summary_poll_incr_long$scenario <- "poll_incr"

#########################################################################
###  Merge the PA data from all three scenarios to enable plotting  #####
#########################################################################

PA_summary_resp_cat_all <- rbind(PA_summary_original_long, PA_summary_no_poll_long, PA_summary_poll_incr_long)

#Remove _PA text from column
PA_summary_resp_cat_all$nutrient <- gsub("_PA", "", PA_summary_resp_cat_all$nutrient)

#Filter dataset to include only the nutrients of interest
PA_summary_resp_cat_filter_1 <- subset(PA_summary_resp_cat_all, nutrient %in% c("VitA", "Folate", "VitE", "Calcium", "VitC", "iron"))

#Filter dataset to include only the scenarios of interest
PA_summary_resp_cat_filter_2 <- subset(PA_summary_resp_cat_filter_1, scenario %in% c("original", "no_poll"))

#########################################################################
###      Plot the change in PA for each res-cat and each nutrient   #####
#########################################################################

# Define a custom color scheme for each resp_cat
custom_colors <- c("adol_fem" = "#F2C4C4", "adult_fem" = "#F29472", "adult_male" = "#5F9595", "u5_child" = "#F0BC68") # Example colors

custom_labels <- c(
  "adol_fem" = "Adolescent girls",
  "adult_fem" = "Adult women",
  "adult_male" = "Adult men",
  "u5_child" = "Children under-five"
)

# Convert the 'scenario' column to a factor with the desired order
PA_summary_resp_cat_filter_2$scenario <- factor(PA_summary_resp_cat_filter_2$scenario, levels = c("original", "no_poll"))
levels(PA_summary_resp_cat_filter_2$scenario) <- c("Original", "Pollinator loss") #Rename the factors


# Create Vitamin A plot
vitA_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "VitA"), 
                    aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1.0) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Vitamin A", x = "", y = "Probability of Adequacy", color = "Resp Category") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors) +  # Apply custom color scheme
  ylim(0, 0.1)  # Manually set y-axis limits (adjust as needed)

# Create Calcium plot
Calcium_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "Calcium"), 
                       aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Calcium", x = "", y = "", color = "Resp Category") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors) +  # Apply custom color scheme
  ylim(0, 0.1)  # Manually set y-axis limits (adjust as needed)

# Create Folate plot
Folate_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "Folate"), 
                      aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Folate", x = "", y = "Probability of Adequacy", color = "Resp Category") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors) +  # Apply custom color scheme
  ylim(0, 1)  # Manually set y-axis limits (adjust as needed)       

# Create VitC plot
VitC_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "VitC"), 
                    aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Vitamin C", x = "", y = "", color = "Resp Category") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors) +  # Apply custom color scheme
  ylim(0, 1)  # Manually set y-axis limits (adjust as needed)   

# Create Iron plot
Iron_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "iron"), 
                    aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Iron", x = "", y = "", color = "Resp Category") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors) +  # Apply custom color scheme
  ylim(0, 0.3)  # Manually set y-axis limits (adjust as needed)  

# Create VitE plot
VitE_plot <- ggplot(PA_summary_resp_cat_filter_2 %>% filter(nutrient == "VitE"), 
                    aes(x = scenario, y = prob_adeq, group = resp_cat, color = resp_cat)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Vitamin E", x = "", y = "", color = "Population subgroup") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",   # Remove legend
        plot.title = element_text(size = rel(1)),  # Adjust plot title size
        axis.title = element_text(size = rel(1))) +
  scale_color_manual(values = custom_colors, labels = custom_labels) +  # Apply custom color scheme
  ylim(0, 0.3)  # Manually set y-axis limits (adjust as needed)    

#Get the legend from the Vitamin E plot
legend <- get_legend(VitE_plot)

# Remove the legend from Folate_plot
VitE_plot_no_legend <- VitE_plot + theme(legend.position = "none")

plot_with_legend <- plot_grid(Folate_plot, VitC_plot, VitE_plot_no_legend, legend, vitA_plot, Calcium_plot, Iron_plot,  ncol = 4, nrow = 2, rel_heights = c(1, 1))

ggsave(plot=plot_with_legend, filename="plots/Key_nutrients_PA_change.svg", width=7, height=6, dpi=500, bg="white")
ggsave(plot=plot_with_legend, filename="plots/Key_nutrients_PA_change.png", width=7, height=6, dpi=500, bg="white")



#########################################################################
## Calculate numbers of people deficient in different nutrients #####
#########################################################################

sufficiency_counts_original <- PA_scores_original %>%
  rowwise() %>%
  mutate(nutrients_sufficient_original = sum(c(Calcium_PA, Zinc_PA, VitC_PA, Thiamin_PA, Riboflav_PA, Niacin_PA, 
                                               Pyridoxine_PA, Folate_PA, VitA_PA, VitE_PA, VitB12_PA, iron_PA) >= 1, na.rm = TRUE))

sufficiency_counts_original <- select(sufficiency_counts_original, resp_id, resp_cat, nutrients_sufficient_original)


sufficiency_counts_no_poll <- PA_scores_no_poll %>%
  rowwise() %>%
  mutate(nutrients_sufficient_no_poll = sum(c(Calcium_PA, Zinc_PA, VitC_PA, Thiamin_PA, Riboflav_PA, Niacin_PA, 
                                              Pyridoxine_PA, Folate_PA, VitA_PA, VitE_PA, VitB12_PA, iron_PA) >= 1, na.rm = TRUE))

sufficiency_counts_no_poll <- select(sufficiency_counts_no_poll, resp_id, resp_cat, nutrients_sufficient_no_poll)


sufficiency_counts_poll_incr <- PA_scores_poll_incr %>%
  rowwise() %>%
  mutate(nutrients_sufficient_poll_incr = sum(c(Calcium_PA, Zinc_PA, VitC_PA, Thiamin_PA, Riboflav_PA, Niacin_PA, 
                                                Pyridoxine_PA, Folate_PA, VitA_PA, VitE_PA, VitB12_PA, iron_PA) >= 1, na.rm = TRUE))

sufficiency_counts_poll_incr <- select(sufficiency_counts_poll_incr, resp_id, resp_cat, nutrients_sufficient_poll_incr)

#Merge the three different scenarios
sufficiency_orig_no_poll <- merge(sufficiency_counts_original, sufficiency_counts_no_poll, by = c("resp_id", "resp_cat"))
sufficiency_all <- merge(sufficiency_orig_no_poll, sufficiency_counts_poll_incr, by = c("resp_id", "resp_cat"))

#Calculate changes in sufficiency
sufficiency_all$change_no_poll <- sufficiency_all$nutrients_sufficient_no_poll - sufficiency_all$nutrients_sufficient_original
sufficiency_all$change_poll_incr <- sufficiency_all$nutrients_sufficient_poll_incr - sufficiency_all$nutrients_sufficient_original

#Calculate number of people newly deficient in at least one nutrient
sufficiency_summary <- sufficiency_all %>%
  dplyr::summarise(across(c("change_no_poll", "change_poll_incr"),
                          ~ sum(. !=0, na.rm = TRUE)))

#Calculate percentage of the population who have become newly sufficient/deficient for each scenario
sufficiency_summary$percent_ch_poll_loss <- (sufficiency_summary$change_no_poll / 776) * 100
sufficiency_summary$percent_ch_poll_incr <- (sufficiency_summary$change_poll_incr / 776) * 100 



##############################################################################
###### Create single dataset with PA for key nutrients under each scenario
##############################################################################

PA_scores_original_subset <- dplyr::select(PA_scores_original, resp_id, resp_cat, Calcium_PA, VitC_PA, Folate_PA, VitA_PA, iron_PA, MPA)
colnames(PA_scores_original_subset) <- gsub("_PA", "_orig", colnames(PA_scores_original_subset))
colnames(PA_scores_original_subset) <- gsub("MPA", "MPA_orig", colnames(PA_scores_original_subset))

PA_scores_no_poll_subset <- dplyr::select(PA_scores_no_poll, resp_id, resp_cat, Calcium_PA, VitC_PA, Folate_PA, VitA_PA, iron_PA, MPA)
colnames(PA_scores_no_poll_subset) <- gsub("_PA", "_no_poll", colnames(PA_scores_no_poll_subset))
colnames(PA_scores_no_poll_subset) <- gsub("MPA", "MPA_no_poll", colnames(PA_scores_no_poll_subset))

PA_scores_poll_incr_subset <- dplyr::select(PA_scores_poll_incr, resp_id, resp_cat, Calcium_PA, VitC_PA, Folate_PA, VitA_PA, iron_PA, MPA)
colnames(PA_scores_poll_incr_subset) <- gsub("_PA", "_poll_incr", colnames(PA_scores_poll_incr_subset))
colnames(PA_scores_poll_incr_subset) <- gsub("MPA", "MPA_poll_incr", colnames(PA_scores_poll_incr_subset))

PA_scores_orig_no_poll <- merge(PA_scores_original_subset, PA_scores_no_poll_subset, by = c("resp_id", "resp_cat"))

# Merge the result with df3
all_PA_scores <- merge(PA_scores_orig_no_poll, PA_scores_poll_incr_subset, by = c("resp_id", "resp_cat"))

#Calculate change in probability resulting from the two scenarios
all_PA_scores$Folate_PA_loss_change <- all_PA_scores$Folate_no_poll - all_PA_scores$Folate_orig
all_PA_scores$Folate_PA_incr_change <- all_PA_scores$Folate_poll_incr - all_PA_scores$Folate_orig

all_PA_scores$Calcium_PA_loss_change <- all_PA_scores$Calcium_no_poll - all_PA_scores$Calcium_orig
all_PA_scores$Calcium_PA_incr_change <- all_PA_scores$Calcium_poll_incr - all_PA_scores$Calcium_orig

all_PA_scores$VitC_PA_loss_change <- all_PA_scores$VitC_no_poll - all_PA_scores$VitC_orig
all_PA_scores$VitC_PA_incr_change <- all_PA_scores$VitC_poll_incr - all_PA_scores$VitC_orig

all_PA_scores$VitA_PA_loss_change <- all_PA_scores$VitA_no_poll - all_PA_scores$VitA_orig
all_PA_scores$VitA_PA_incr_change <- all_PA_scores$VitA_poll_incr - all_PA_scores$VitA_orig

all_PA_scores$iron_PA_loss_change <- all_PA_scores$iron_no_poll - all_PA_scores$iron_orig
all_PA_scores$iron_PA_incr_change <- all_PA_scores$iron_poll_incr - all_PA_scores$iron_orig

##Summarise probability of adequacy for each group
PA_change_resp_cat <- all_PA_scores %>%
  group_by(resp_cat) %>%
  summarise_all(~mean(., na.rm = TRUE))

#Create simpler summary table
PA_change_resp_cat_subset <- select(PA_change_resp_cat, resp_cat, Folate_orig, 	Folate_no_poll,	Folate_poll_incr,	VitC_orig,	VitC_no_poll,	VitC_poll_incr,	VitA_orig,	VitA_no_poll,	VitA_poll_incr,	iron_orig,	iron_no_poll,	iron_poll_incr,	Calcium_orig,	Calcium_no_poll,	Calcium_poll_incr,	MPA_orig,	MPA_no_poll,	MPA_poll_incr)



############################################################################################################################################################################
############################                END OF SCRIPT - ALL CODE AFTER THIS IS FOR ADDITIONAL CHECKS  ONLY             ################################################
############################################################################################################################################################################


