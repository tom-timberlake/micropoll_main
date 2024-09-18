##################################################################################################################################################################
##############   MICRO-POLL SCRIPT 3 - SIMULATING IMPACTS OF POLLINATOR CHANGE ON NUTRIENT INTAKE & FARMING INCOME  ##############################################
##################################################################################################################################################################

#Clear workspace
rm(list = ls())

########################################################
##### Install required packages and import data  #######
########################################################

#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(data.table)
library(skimr)
library(openxlsx)
library(reshape2)
library(tidyr)
library(gridExtra)
library(grid)
library(dplyr)
library(ggridges)
library(lme4)
library(emmeans)
library(scales)

##Import intake data
intake_data <- read.csv("output_data/Nutrient_intake_data_PD.csv")

##Import pollinator yield change data
poll_change_data <- data.table(read_excel("input_data/pollinator_change_yield_data.xlsx",
                                          sheet = "Sheet1", 
                                          na = c("", "---", NA))) 

##Import farmer questionnaire data for economic information
farmer_data <- data.table(read_excel("input_data/MP_farmer_questionnaire.xlsx",
                                     sheet = "Farmer questionnaire", 
                                     na = c("", "---", NA)))  


#Merge yield change data in with dietary intake data
diet_poll_change_data <- merge(x=intake_data, y=poll_change_data, by= "plant_barcode",all.x=TRUE)


######################################################################################
########## Calculate nutrient intakes AFTER poll-dependent component has been removed
######################################################################################

## Define pollinator dependence of imported crops as zero so that we are only altering yields of local crops when we apply pollinator change scenarios
#Note that this setting can be changed if we want to include the effect of global pollinator declines as well as local

#Create new dataset
diet_poll_change_data_local_only <- diet_poll_change_data

#Define pollinator-dependent intake columns (i.e. those ending with '_PD')
columns_PD <- grep("_PD$", names(diet_poll_change_data_local_only), value = TRUE)

# Find the rows where 'local_imported' is 'import'
import_rows <- diet_poll_change_data_local_only$local_imported == "import"

# Modify the values in the identified columns to 0 for these rows so that all imported foods are given a pollinator dependence value of zero
diet_poll_change_data_local_only[import_rows, columns_PD] <- 0


#Remove the entire local pollinator-dependent component of each person's diet (simulating complete loss of local pollinators)
remove_poll_dependence <- diet_poll_change_data_local_only %>%
  mutate(Energy_no_poll = int_Energy_Kcal - int_Energy_PD,
         Fat_no_poll = int_Fat_g - int_Fat_PD,
         Protein_no_poll = int_Protein_g - int_Protein_PD,
         Carbohydrate_no_poll = int_Carbohydrate_g - int_Carbohydrate_PD,
         TotalFibre_no_poll = int_TotalFibre_g - int_TotalFibre_PD,
         Insolublefibre_no_poll = int_Insolublefibre_g - int_Insolublefibre_PD,
         Solublefibre_no_poll = int_Solublefibre_g - int_Solublefibre_PD,
         Calcium_no_poll = int_Calcium_mg - int_Calcium_PD,
         Iron_no_poll = int_Iron_mg - int_Iron_PD,
         Zinc_no_poll = int_Zinc_mg - int_Zinc_PD,
         VitaminC_no_poll = int_VitaminC_mg - int_VitaminC_PD,
         ThiaminB1_no_poll = int_ThiaminB1_mg - int_ThiaminB1_PD,
         RiboflavinB2_no_poll = int_RiboflavinB2_mg - int_RiboflavinB2_PD,
         NiacinB3_no_poll = int_NiacinB3_mg - int_NiacinB3_PD,
         VitaminB6pyridoxine_no_poll = int_VitaminB6pyridoxine_mg - int_VitaminB6pyridoxine_PD,
         FolateTotal_no_poll = int_FolateTotal_µg - int_FolateTotal_PD,
         VitARE_no_poll = int_VitARE_µg - int_VitARE_PD,
         Retinol_no_poll = int_Retinol_µg - int_Retinol_PD,
         AlphaCarotene_no_poll = int_AlphaCarotene_µg - int_AlphaCarotene_PD,
         BetaCarotene_no_poll = int_BetaCarotene_µg - int_BetaCarotene_PD,
         VitaminE_no_poll = int_VitaminE_mg - int_VitaminE_PD,
         VitaminB12_no_poll = int_VitaminB12_µg - int_VitaminB12_PD,
         Phytate_no_poll = int_Phytate_mg - int_Phytate_PD)


##############################################################
#### Calculate nutrient intake after pollinator enhancement
#############################################################

# Changing crop yield values for imported foods and NAs so that only local changes are implemented

#Change poll-change values to 1.0 for imported foods so that no pollinator enhancement is applied to imported foods
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield = ifelse(local_imported == "import", 1, poll_incr_new_yield))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield_L95 = ifelse(local_imported == "import", 1, poll_incr_new_yield_L95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield_U95 = ifelse(local_imported == "import", 1, poll_incr_new_yield_U95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield = ifelse(local_imported == "import", 1, poll_decline_new_yield))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield_L95 = ifelse(local_imported == "import", 1, poll_decline_new_yield_L95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield_U95 = ifelse(local_imported == "import", 1, poll_decline_new_yield_U95))

#Set all NA values to 1 for poll change scenarios so that we don't change the yield of any crops/ingredients with a pollinator dependence value of NA
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield = ifelse(is.na(poll_incr_new_yield), 1, poll_incr_new_yield))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield_L95 = ifelse(is.na(poll_incr_new_yield_L95), 1, poll_incr_new_yield_L95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_incr_new_yield_U95 = ifelse(is.na(poll_incr_new_yield_U95), 1, poll_incr_new_yield_U95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield = ifelse(is.na(poll_decline_new_yield), 1, poll_decline_new_yield))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield_L95 = ifelse(is.na(poll_decline_new_yield_L95), 1, poll_decline_new_yield_L95))
remove_poll_dependence <- remove_poll_dependence %>%
  mutate(poll_decline_new_yield_U95 = ifelse(is.na(poll_decline_new_yield_U95), 1, poll_decline_new_yield_U95))


#Calculate nutrient intake after increasing yields of poll-dependent crops
#Poll_incr_new_yield is based off the percentage of yield gap which can be closed by enhancing pollination services
# Yield gap values were measured in the field and percentage of yield gaps attributable to pollination is from Garibaldi et al. (2016)
diet_poll_enhance_data <- remove_poll_dependence %>%
  mutate(int_Energy_poll_incr =  (int_Energy_Kcal * poll_incr_new_yield), 
         int_Fat_poll_incr =  (int_Fat_g * poll_incr_new_yield), 
         int_Protein_poll_incr =  (int_Protein_g * poll_incr_new_yield), 
         int_Carbohydrate_poll_incr =  (int_Carbohydrate_g * poll_incr_new_yield), 
         int_TotalFibre_poll_incr =  (int_TotalFibre_g * poll_incr_new_yield), 
         int_Insolublefibre_poll_incr =  (int_Insolublefibre_g * poll_incr_new_yield), 
         int_Solublefibre_poll_incr =  (int_Solublefibre_g * poll_incr_new_yield), 
         int_Calcium_poll_incr =  (int_Calcium_mg * poll_incr_new_yield), 
         int_Iron_poll_incr =  (int_Iron_mg * poll_incr_new_yield), 
         int_Zinc_poll_incr =  (int_Zinc_mg * poll_incr_new_yield), 
         int_VitaminC_poll_incr =  (int_VitaminC_mg * poll_incr_new_yield), 
         int_ThiaminB1_poll_incr =  (int_ThiaminB1_mg * poll_incr_new_yield), 
         int_RiboflavinB2_poll_incr =  (int_RiboflavinB2_mg * poll_incr_new_yield), 
         int_NiacinB3_poll_incr =  (int_NiacinB3_mg * poll_incr_new_yield), 
         int_VitaminB6pyridoxine_poll_incr =  (int_VitaminB6pyridoxine_mg * poll_incr_new_yield), 
         int_FolateTotal_poll_incr =  (int_FolateTotal_µg * poll_incr_new_yield), 
         int_VitARE_poll_incr =  (int_VitARE_µg * poll_incr_new_yield), 
         int_Retinol_poll_incr =  (int_Retinol_µg * poll_incr_new_yield), 
         int_AlphaCarotene_poll_incr =  (int_AlphaCarotene_µg * poll_incr_new_yield), 
         int_BetaCarotene_poll_incr =  (int_BetaCarotene_µg * poll_incr_new_yield), 
         int_VitaminE_poll_incr =  (int_VitaminE_mg * poll_incr_new_yield), 
         int_VitaminB12_poll_incr =  (int_VitaminB12_µg * poll_incr_new_yield), 
         int_Phytate_poll_incr =  (int_Phytate_mg * poll_incr_new_yield))


###################################################################
#### Calculate nutrient intake after local pollinator declines
##################################################################

### Local pollinator declines are based off values of local rates of honeybee decline from Kortsch et al. 2024 and projected forward to 2030
# We assume that all pollinators decline at the same rate as native honeybees and that the relationship between pollinator abundance and pollination service is linear
#We multiply the predicted decline in the pollination service by the pollinator-dependence of each crop to estimate the change in total crop yield - these are the values provided in the 'poll_change' dataset

diet_poll_decline_data <- diet_poll_enhance_data %>%
  mutate(int_Energy_poll_decl = (int_Energy_Kcal * poll_decline_new_yield), 
         int_Fat_poll_decl = (int_Fat_g * poll_decline_new_yield), 
         int_Protein_poll_decl = (int_Protein_g * poll_decline_new_yield), 
         int_Carbohydrate_poll_decl = (int_Carbohydrate_g * poll_decline_new_yield), 
         int_TotalFibre_poll_decl = (int_TotalFibre_g * poll_decline_new_yield), 
         int_Insolublefibre_poll_decl = (int_Insolublefibre_g * poll_decline_new_yield), 
         int_Solublefibre_poll_decl = (int_Solublefibre_g * poll_decline_new_yield), 
         int_Calcium_poll_decl = (int_Calcium_mg * poll_decline_new_yield), 
         int_Iron_poll_decl = (int_Iron_mg * poll_decline_new_yield), 
         int_Zinc_poll_decl = (int_Zinc_mg * poll_decline_new_yield), 
         int_VitaminC_poll_decl = (int_VitaminC_mg * poll_decline_new_yield), 
         int_ThiaminB1_poll_decl = (int_ThiaminB1_mg * poll_decline_new_yield), 
         int_RiboflavinB2_poll_decl = (int_RiboflavinB2_mg * poll_decline_new_yield), 
         int_NiacinB3_poll_decl = (int_NiacinB3_mg * poll_decline_new_yield), 
         int_VitaminB6pyridoxine_poll_decl = (int_VitaminB6pyridoxine_mg * poll_decline_new_yield), 
         int_FolateTotal_poll_decl = (int_FolateTotal_µg * poll_decline_new_yield), 
         int_VitARE_poll_decl = (int_VitARE_µg * poll_decline_new_yield), 
         int_Retinol_poll_decl = (int_Retinol_µg * poll_decline_new_yield), 
         int_AlphaCarotene_poll_decl = (int_AlphaCarotene_µg * poll_decline_new_yield), 
         int_BetaCarotene_poll_decl = (int_BetaCarotene_µg * poll_decline_new_yield), 
         int_VitaminE_poll_decl = (int_VitaminE_mg * poll_decline_new_yield), 
         int_VitaminB12_poll_decl = (int_VitaminB12_µg * poll_decline_new_yield), 
         int_Phytate_poll_decl = (int_Phytate_mg * poll_decline_new_yield))


#Replace all NA values with zero to prevent them messing up subsequent calculations
diet_poll_decline_data[is.na(diet_poll_decline_data)] <- 0

###############################################################################################
##########dplyr::summarise nutrient intakes for each recall event & participant
###############################################################################################

#names(diet_poll_decline_data)

#remove unwanted columns
data_subset <- diet_poll_decline_data %>%
  select(-plant_barcode, -ingredient_code, -X, -yield_gap_caluclation_notes,
         -food_item_code, -recipe_code,  -food_item_name,
         -food_cat, -recipe_grams_consumed, -recipe_food_type, -ingred_food_type, -ingredient_name.x,
         -percent_ingred_in_recipe, -ingredient_grams_consumed, 
         -local_imported, -mixed_ingredients, -breastfed_times_child, -child_breastfeeding_24h,
         -cultivated_wild, -plant_animal_mineral, -poll_dependent, -prop_focal_ing,
         -plant_category.x, -sci_name.x, -eng_name.x, -nep_name_eng,
         -final_poll_dependence.x, -poll_depend_component.x, -poll_dependence_source,
         -poll_notes, -plant_category.y, -plant_family, -sci_name.y, -eng_name.y,
         -final_poll_dependence.y, -poll_depend_component.y, -poll_loss_new_yield,
         -poll_decline_new_yield,  -poll_incr_new_yield, -yield_gap_values, -food_grouping, -retention_code_cooked)

nutrient_intake_by_day <- data_subset %>%
  dplyr::group_by(formid, hh_barcode, resp_id, village_code,
                  date, month, season, resp_cat, sex,
                  age_y_at_recall, currently_pregnant, mother_breastfeeding_2wks, periods_yet) %>%
  dplyr::summarise_all(sum, na.rm = TRUE)


######################################################################################
########## Implement replacement of missing food items based on calorific replacement with plain white rice
######################################################################################

#Calculate the loss of calories resulting from the two different scenarios of pollinator decline
nutrient_intake_by_day <- nutrient_intake_by_day %>%
  mutate(kcal_loss_no_poll = int_Energy_Kcal - Energy_no_poll,
         kcal_loss_poll_decl = int_Energy_Kcal - int_Energy_poll_decl)


#Calculate grams of rice this translates to in units of 100 grams (plain white rice has 356.358kcal per 100 gram). This tells us how much much rice we need to replace in the diet in units of 100 grams
#The reason for keeping the units as 100 grams is it allows to simply multiply the densisites of each nutrient per hundred grams in the next step, rather than having to convert them to nutrient/gram
nutrient_intake_by_day <- nutrient_intake_by_day %>%
  mutate(additional_rice_no_poll = kcal_loss_no_poll/356.358,
         additional_rice_poll_decl = kcal_loss_poll_decl/356.358)

#Add extra nutrients provided by rice - we now calculate the additional nutrients that people obtain by eating more rice and add this to their daily intakes
nutrient_intake_with_replacement <- nutrient_intake_by_day %>%
  mutate(Energy_no_poll = Energy_no_poll + (additional_rice_no_poll*356.358),  #Energy  value from FCT for 'Rice, Fine grain, Basmati'
         Fat_no_poll = Fat_no_poll + (additional_rice_no_poll*0.52),  #Fat  value from FCT for 'Rice, Fine grain, Basmati'
         Protein_no_poll = Protein_no_poll + (additional_rice_no_poll*7.94),  #Protein  value from FCT for 'Rice, Fine grain, Basmati'
         Carbohydrate_no_poll = Carbohydrate_no_poll + (additional_rice_no_poll*78.2),  #Carbohydrate  value from FCT for 'Rice, Fine grain, Basmati'
         TotalFibre_no_poll = TotalFibre_no_poll + (additional_rice_no_poll*2.81),  #TotalFibre  value from FCT for 'Rice, Fine grain, Basmati'
         Insolublefibre_no_poll = Insolublefibre_no_poll + (additional_rice_no_poll*1.99),  #Insolublefibre  value from FCT for 'Rice, Fine grain, Basmati'
         Solublefibre_no_poll = Solublefibre_no_poll + (additional_rice_no_poll*0.82),  #Solublefibre  value from FCT for 'Rice, Fine grain, Basmati'
         Calcium_no_poll = Calcium_no_poll + (additional_rice_no_poll*7.49),  #Calcium  value from FCT for 'Rice, Fine grain, Basmati'
         Iron_no_poll = Iron_no_poll + (additional_rice_no_poll*0.65),  #Iron  value from FCT for 'Rice, Fine grain, Basmati'
         Zinc_no_poll = Zinc_no_poll + (additional_rice_no_poll*1.21),  #Zinc  value from FCT for 'Rice, Fine grain, Basmati'
         VitaminC_no_poll = VitaminC_no_poll + (additional_rice_no_poll*0),  #VitaminC  value from FCT for 'Rice, Fine grain, Basmati'
         ThiaminB1_no_poll = ThiaminB1_no_poll + (additional_rice_no_poll*0.05),  #ThiaminB1  value from FCT for 'Rice, Fine grain, Basmati'
         RiboflavinB2_no_poll = RiboflavinB2_no_poll + (additional_rice_no_poll*0.05),  #RiboflavinB2  value from FCT for 'Rice, Fine grain, Basmati'
         NiacinB3_no_poll = NiacinB3_no_poll + (additional_rice_no_poll*1.69),  #NiacinB3  value from FCT for 'Rice, Fine grain, Basmati'
         VitaminB6pyridoxine_no_poll = VitaminB6pyridoxine_no_poll + (additional_rice_no_poll*0.12),  #VitaminB6pyridoxine  value from FCT for 'Rice, Fine grain, Basmati'
         FolateTotal_no_poll = FolateTotal_no_poll + (additional_rice_no_poll*9.32),  #FolateTotal  value from FCT for 'Rice, Fine grain, Basmati'
         VitARE_no_poll = VitARE_no_poll + (additional_rice_no_poll*0),  #VitARE  value from FCT for 'Rice, Fine grain, Basmati'
         Retinol_no_poll = Retinol_no_poll + (additional_rice_no_poll*0),  #Retinol  value from FCT for 'Rice, Fine grain, Basmati'
         AlphaCarotene_no_poll = AlphaCarotene_no_poll + (additional_rice_no_poll*0),  #AlphaCarotene  value from FCT for 'Rice, Fine grain, Basmati'
         BetaCarotene_no_poll = BetaCarotene_no_poll + (additional_rice_no_poll*0),  #BetaCarotene  value from FCT for 'Rice, Fine grain, Basmati'
         VitaminE_no_poll = VitaminE_no_poll + (additional_rice_no_poll*0.06),  #VitaminE  value from FCT for 'Rice, Fine grain, Basmati'
         VitaminB12_no_poll = VitaminB12_no_poll + (additional_rice_no_poll*0),  #VitaminB12  value from FCT for 'Rice, Fine grain, Basmati'
         Phytate_no_poll = Phytate_no_poll + (additional_rice_no_poll*266.1),    #Phytate  value from FCT for 'Rice, Fine grain, Basmati' 
         
         #Mean values for poll decline
         int_Energy_poll_decl = int_Energy_poll_decl +   (additional_rice_poll_decl*356.358),  
         int_Fat_poll_decl = int_Fat_poll_decl +   (additional_rice_poll_decl*0.52),  
         int_Protein_poll_decl = int_Protein_poll_decl +   (additional_rice_poll_decl*7.94),  
         int_Carbohydrate_poll_decl = int_Carbohydrate_poll_decl +   (additional_rice_poll_decl*78.2),  
         int_TotalFibre_poll_decl = int_TotalFibre_poll_decl +   (additional_rice_poll_decl*2.81),  
         int_Insolublefibre_poll_decl = int_Insolublefibre_poll_decl +   (additional_rice_poll_decl*1.99),  
         int_Solublefibre_poll_decl = int_Solublefibre_poll_decl +   (additional_rice_poll_decl*0.82),  
         int_Calcium_poll_decl = int_Calcium_poll_decl +   (additional_rice_poll_decl*7.49),  
         int_Iron_poll_decl = int_Iron_poll_decl +   (additional_rice_poll_decl*0.65),  
         int_Zinc_poll_decl = int_Zinc_poll_decl +   (additional_rice_poll_decl*1.21),  
         int_VitaminC_poll_decl = int_VitaminC_poll_decl +   (additional_rice_poll_decl*0),  
         int_ThiaminB1_poll_decl = int_ThiaminB1_poll_decl +   (additional_rice_poll_decl*0.05),  
         int_RiboflavinB2_poll_decl = int_RiboflavinB2_poll_decl +   (additional_rice_poll_decl*0.05),  
         int_NiacinB3_poll_decl = int_NiacinB3_poll_decl +   (additional_rice_poll_decl*1.69),  
         int_VitaminB6pyridoxine_poll_decl = int_VitaminB6pyridoxine_poll_decl +   (additional_rice_poll_decl*0.12),  
         int_FolateTotal_poll_decl = int_FolateTotal_poll_decl +   (additional_rice_poll_decl*9.32),  
         int_VitARE_poll_decl = int_VitARE_poll_decl +   (additional_rice_poll_decl*0),  
         int_Retinol_poll_decl = int_Retinol_poll_decl +   (additional_rice_poll_decl*0),  
         int_AlphaCarotene_poll_decl = int_AlphaCarotene_poll_decl +   (additional_rice_poll_decl*0),  
         int_BetaCarotene_poll_decl = int_BetaCarotene_poll_decl +   (additional_rice_poll_decl*0),  
         int_VitaminE_poll_decl = int_VitaminE_poll_decl +   (additional_rice_poll_decl*0.06),  
         int_VitaminB12_poll_decl = int_VitaminB12_poll_decl +   (additional_rice_poll_decl*0),  
         int_Phytate_poll_decl = int_Phytate_poll_decl +   (additional_rice_poll_decl*266.1)) %>%
  ungroup() 


##############################################################################
###### Subset data to essential columns, rename and export for MPA analysis
##############################################################################

#This is where we subdivide the main dataset into the relevant columns for each scenario and export them in a generic format 
#These datasets will be used in the next R script (Script_04) where we calculate the probability of adequacy for each micronutrient 

data_to_export <- nutrient_intake_with_replacement
colnames(data_to_export) <- gsub("int_", "", colnames(data_to_export))

#Subset to only original intake data
original_intake_data <- select(data_to_export, 
                               "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                               "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                               "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                               "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                               "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Subset to poll-loss intake data
no_poll_intake_data <- select(data_to_export, 
                              "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                              "Energy_no_poll", "Fat_no_poll", "Protein_no_poll", "Carbohydrate_no_poll", "TotalFibre_no_poll", "Insolublefibre_no_poll", "Solublefibre_no_poll",
                              "Calcium_no_poll", "Iron_no_poll", "Zinc_no_poll", "VitaminC_no_poll", "ThiaminB1_no_poll", "RiboflavinB2_no_poll",
                              "NiacinB3_no_poll", "VitaminB6pyridoxine_no_poll", "FolateTotal_no_poll", "VitARE_no_poll", "Retinol_no_poll", "AlphaCarotene_no_poll",
                              "BetaCarotene_no_poll", "VitaminE_no_poll", "VitaminB12_no_poll", "Phytate_no_poll")

#Rename columns to macth with the original intake columns so that format is consistent between all 3 datasets
colnames(no_poll_intake_data) <- c("formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                   "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                                   "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                                   "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                                   "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Subset to poll-enhancement intake data
poll_incr_intake_data <- select(data_to_export, 
                                "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                "Energy_poll_incr", 	"Fat_poll_incr",	"Protein_poll_incr",	"Carbohydrate_poll_incr",	"TotalFibre_poll_incr",	
                                "Insolublefibre_poll_incr",	"Solublefibre_poll_incr",	"Calcium_poll_incr",	"Iron_poll_incr",	"Zinc_poll_incr",	
                                "VitaminC_poll_incr",	"ThiaminB1_poll_incr",	"RiboflavinB2_poll_incr",	"NiacinB3_poll_incr",	"VitaminB6pyridoxine_poll_incr",	
                                "FolateTotal_poll_incr",	"VitARE_poll_incr",	"Retinol_poll_incr",	"AlphaCarotene_poll_incr",	"BetaCarotene_poll_incr",	"VitaminE_poll_incr",	
                                "VitaminB12_poll_incr",	"Phytate_poll_incr")

#Rename columns to macth with the original intake columns so that format is consistent between all 3 datasets
colnames(poll_incr_intake_data) <- c("formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                     "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                                     "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                                     "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                                     "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Export these data to be used in calculating the probability of adequacy for each individual

write.csv(original_intake_data, "output_data/Original_intake_data.csv")

write.csv(no_poll_intake_data, "output_data/No-poll_intake_data.csv")

write.csv(poll_incr_intake_data, "output_data/Poll-incr_intake_data.csv")


##############################################################################
###### Calculate mean daily intakes for each person 
##############################################################################

#Make copy of final dataset to enable editing
data_to_summarise <- data_to_export

#Change all units symbols in column headers to specify that these columns show original intakes
colnames(data_to_summarise) <- gsub("_Kcal", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_g", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_mg", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_µg", "_original", colnames(data_to_summarise))

#Remove the columns that we're not interested in
data_to_summarise <- select(data_to_summarise, -formid, -date, -month, -season, -age_y_at_recall, -currently_pregnant,
                            -mother_breastfeeding_2wks, -periods_yet, -kcal_loss_no_poll, -kcal_loss_poll_decl, 
                            -additional_rice_no_poll, -additional_rice_poll_decl)

#Summarise the mean daily nutrient intake for each respondent
mean_daily_intakes <- data_to_summarise %>%
 dplyr::group_by(hh_barcode, resp_id, village_code, resp_cat, sex) %>%
 dplyr::summarise_all(~mean(., na.rm = TRUE))

#Restructure data so that scenarios remain as column headers and nutrients become a new variable
df_long_scenarios <- mean_daily_intakes %>%
  pivot_longer(
    cols = -c(hh_barcode, resp_id, village_code, resp_cat, sex),
    names_to = c("nutrient", ".value"),
    names_pattern = "^(.+?)_(.+)$",
    values_transform = list(variable = factor))

#Restructure data so that scenarios also become a new variable
df_long_all <- df_long_scenarios %>%
  pivot_longer(cols = -c(hh_barcode, resp_id, village_code, resp_cat, sex, nutrient),
               names_to = "scenario",
               values_to = "value")

#Remove rows where scenarios haven't been modeled and therefore there is no associated nutrient intake data
values_to_remove <- c("decline_new_yield_L95", "decline_new_yield_U95", 
                      "incr_new_yield_L95", "incr_new_yield_U95", "PD")

# Filter the dataframe to exclude rows where the 'scenario' column matches any of these values
df_long_all <- df_long_all[!(df_long_all$scenario %in% values_to_remove), ]


##########################################################################################################################################################
##################################        MODELLING ECONOMIC CHANGES RESULTING FROM POLLINATOR CHANGE SCENARIOS         ##################################
##########################################################################################################################################################

#############################################################################
### Tidy up farmer questionnaire data and extract relevant economic data
#############################################################################

#Change NA values to zero for household income, and all other crop income
#This is because NA values result from instances in which respondents report that they do no earn any income from these crops - therefore the correct value is zero
farmer_data <- farmer_data %>%
  mutate(farming_income = ifelse(is.na(farming_income), 0, farming_income),
         apple_income = ifelse(is.na(apple_income), 0, apple_income),
         mustard_income = ifelse(is.na(mustard_income), 0, mustard_income),
         pumpkin_income = ifelse(is.na(pumpkin_income), 0, pumpkin_income),
         karela_income = ifelse(is.na(karela_income), 0, karela_income),
         buckwheat_income = ifelse(is.na(buckwheat_income), 0, buckwheat_income),
         bean_income = ifelse(is.na(bean_income), 0, bean_income))

##Subset data to relevant columns
farmer_income_data <- select(farmer_data, village_code, hh_barcode, farming_income, apple_income, bean_income, mustard_income,pumpkin_income, karela_income, buckwheat_income  )

#Calculate all income obtained from pollinator-dependent crops
farmer_income_data_PD <- farmer_income_data %>%
  mutate(PD_crop_income = apple_income + bean_income + mustard_income + pumpkin_income + karela_income + buckwheat_income)

#Calculate proportion of total farming income derived from pollinator-dependent crops
farmer_income_data_PD <- farmer_income_data_PD %>%
  mutate(PD_crop_income_prop = PD_crop_income / farming_income)

#Remove NA, inf and zero values as these are households with no farming income and therefore not relevant for our calculations
farmer_income_data_PD <- farmer_income_data_PD %>% filter(!is.na(PD_crop_income_prop) & PD_crop_income_prop != 0 & is.finite(PD_crop_income_prop))

#Summarise mean and SD values for farming income
farmer_income_data_summary <- farmer_income_data_PD %>%
 dplyr::summarise(PD_crop_income_mean = mean(PD_crop_income_prop, na.rm = TRUE),
            PD_crop_income_SD = sd(PD_crop_income_prop, na.rm = TRUE))

##########################################################################################
### Remove pollinator-dependent component of production - i.e simulating complete pollinator loss
##########################################################################################

#Write function to get yield change values for a given crop and a given nutrient
get_yield_change_value <- function(dataframe, crop, scenario) {
  value <- dataframe %>%
    filter(eng_name == crop) %>%
    select(all_of(scenario)) %>%
    pull()
  return(value)
}

#Pull the pollinator dependence values from the yield change dataset - this is the yield we expect to lose in the absence of pollinators 
apple_PD <- get_yield_change_value(poll_change_data, "Apple", "final_poll_dependence")
bean_PD <- get_yield_change_value(poll_change_data, "Jumli bean", "final_poll_dependence")
mustard_PD <- get_yield_change_value(poll_change_data, "Mustard", "final_poll_dependence")
pumpkin_PD <- get_yield_change_value(poll_change_data, "Pumpkin", "final_poll_dependence")
karela_PD <- get_yield_change_value(poll_change_data, "Slipper gourd", "final_poll_dependence")
buckwheat_PD <- get_yield_change_value(poll_change_data, "Buckwheat", "final_poll_dependence")

#Remove the pollinator-dependent component of production from each crop and calculate the new income after pollinator loss
farmer_income_data$apple_no_poll <- farmer_income_data$apple_income * (1 - apple_PD)
farmer_income_data$bean_no_poll <- farmer_income_data$bean_income * (1 - bean_PD)
farmer_income_data$mustard_no_poll <- farmer_income_data$mustard_income * (1 - mustard_PD)
farmer_income_data$pumpkin_no_poll <- farmer_income_data$pumpkin_income * (1 - pumpkin_PD)
farmer_income_data$karela_no_poll <- farmer_income_data$karela_income * (1 - karela_PD)
farmer_income_data$buckwheat_no_poll <- farmer_income_data$buckwheat_income * (1 - buckwheat_PD)

#Calculate total income changes and proportional changes
farmer_income_data <- farmer_income_data %>% mutate(crop_income = apple_income + bean_income + mustard_income + pumpkin_income + karela_income + buckwheat_income)
farmer_income_data <- farmer_income_data %>% mutate(crop_income_no_poll = apple_no_poll + bean_no_poll + mustard_no_poll + pumpkin_no_poll + karela_no_poll + buckwheat_no_poll)
farmer_income_data <- farmer_income_data %>% mutate(income_loss = crop_income_no_poll - crop_income)
farmer_income_data <- farmer_income_data %>% mutate(farming_income_no_poll = farming_income - income_loss)
farmer_income_data <- farmer_income_data %>% mutate(proportion_loss = income_loss/farming_income)

############################################################
### Calculate pollinator enhancement
############################################################

#Pull the pollinator enhancement yield change values from the yield change dataset - this is the yield we expect to achieve under optimum pollination
apple_poll <- get_yield_change_value(poll_change_data, "Apple", "poll_incr_new_yield")
bean_poll <- get_yield_change_value(poll_change_data, "Jumli bean", "poll_incr_new_yield")
mustard_poll <- get_yield_change_value(poll_change_data, "Mustard", "poll_incr_new_yield")
pumpkin_poll <- get_yield_change_value(poll_change_data, "Pumpkin", "poll_incr_new_yield")
karela_poll <- get_yield_change_value(poll_change_data, "Slipper gourd", "poll_incr_new_yield")
buckwheat_poll <- get_yield_change_value(poll_change_data, "Buckwheat", "poll_incr_new_yield")

#Calculate the new income from each crop after pollinator enhancement
farmer_income_data$apple_poll_incr <- farmer_income_data$apple_income * apple_poll
farmer_income_data$bean_poll_incr <- farmer_income_data$bean_income * bean_poll
farmer_income_data$mustard_poll_incr <- farmer_income_data$mustard_income * mustard_poll
farmer_income_data$pumpkin_poll_incr <- farmer_income_data$pumpkin_income * pumpkin_poll
farmer_income_data$karela_poll_incr <- farmer_income_data$karela_income * karela_poll
farmer_income_data$buckwheat_poll_incr <- farmer_income_data$buckwheat_income * buckwheat_poll

#Calculate total income changes and proportional changes
farmer_income_data <- farmer_income_data %>% mutate(crop_income = apple_income + bean_income + mustard_income + pumpkin_income + karela_income + buckwheat_income)
farmer_income_data <- farmer_income_data %>% mutate(crop_income_poll_incr = apple_poll_incr + bean_poll_incr + mustard_poll_incr + pumpkin_poll_incr + karela_poll_incr + buckwheat_poll_incr)
farmer_income_data <- farmer_income_data %>% mutate(income_incr = crop_income_poll_incr - crop_income)
farmer_income_data <- farmer_income_data %>% mutate(farming_income_poll_incr = farming_income + income_incr)
farmer_income_data <- farmer_income_data %>% mutate(proportion_incr = income_incr/farming_income)

############################################################
### Calculate pollinator decline (2030 scenario based on honeybee decline)
############################################################

#Pull the pollinator decline yield change values from the yield change dataset - this is the yield we expect to achieve in 2030 assuming pollinator declines continue at current rates (from Kortsch et al 2024)
apple_poll_decl <- get_yield_change_value(poll_change_data, "Apple", "poll_decline_new_yield")
bean_poll_decl <- get_yield_change_value(poll_change_data, "Jumli bean", "poll_decline_new_yield")
mustard_poll_decl <- get_yield_change_value(poll_change_data, "Mustard", "poll_decline_new_yield")
pumpkin_poll_decl <- get_yield_change_value(poll_change_data, "Pumpkin", "poll_decline_new_yield")
karela_poll_decl <- get_yield_change_value(poll_change_data, "Slipper gourd", "poll_decline_new_yield")
buckwheat_poll_decl <- get_yield_change_value(poll_change_data, "Buckwheat", "poll_decline_new_yield")

#Calculate the new income from each crop after pollinator decline
farmer_income_data$apple_poll_decl <- farmer_income_data$apple_income * apple_poll_decl
farmer_income_data$bean_poll_decl <- farmer_income_data$bean_income * bean_poll_decl
farmer_income_data$mustard_poll_decl <- farmer_income_data$mustard_income * mustard_poll_decl
farmer_income_data$pumpkin_poll_decl <- farmer_income_data$pumpkin_income * pumpkin_poll_decl
farmer_income_data$karela_poll_decl <- farmer_income_data$karela_income * karela_poll_decl
farmer_income_data$buckwheat_poll_decl <- farmer_income_data$buckwheat_income * buckwheat_poll_decl

#Calculate total income changes and proportional changes
farmer_income_data <- farmer_income_data %>% mutate(crop_income_poll_decl = apple_poll_decl + bean_poll_decl + mustard_poll_decl + pumpkin_poll_decl + karela_poll_decl + buckwheat_poll_decl)
farmer_income_data <- farmer_income_data %>% mutate(income_decl = crop_income_poll_decl - crop_income)
farmer_income_data <- farmer_income_data %>% mutate(farming_income_poll_decl = farming_income + income_decl)
farmer_income_data <- farmer_income_data %>% mutate(proportion_decl = income_decl/farming_income)


#Replace all NA and infinite values with zero to avoid calculation errors
farmer_income_data[is.na(farmer_income_data)] <- 0
farmer_income_data <- farmer_income_data %>%  mutate(proportion_loss = ifelse(!is.finite(proportion_loss), 0, proportion_loss))
farmer_income_data <- farmer_income_data %>%  mutate(proportion_incr = ifelse(!is.finite(proportion_incr), 0, proportion_incr))
farmer_income_data <- farmer_income_data %>%  mutate(proportion_decl = ifelse(!is.finite(proportion_decl), 0, proportion_decl))


##############################################################################
###### Caclulate proportional changes and convert to appropriate format
##############################################################################

#Subset to relevant columns
proportional_changes <- select(farmer_income_data,  hh_barcode, proportion_loss, proportion_incr, proportion_decl)

#Convert to long format 
df_long_proportions <- proportional_changes %>%
  pivot_longer(cols = -c(hh_barcode),
               names_to = "scenario",
               values_to = "value")

#Remove NA, inf and zero values as these are households with no farming income and therefore not relevant for our calculations
cleaned_df_long_proportions <- df_long_proportions %>% filter(!is.na(value) & value != 0 & is.finite(value))

#Change names of each scenario to match those in the dietary intake dataset
final_economic_data <- cleaned_df_long_proportions %>%
  mutate(scenario = case_when(scenario == "proportion_loss" ~ "no_poll",
                              scenario == "proportion_incr" ~ "poll_incr",
                              scenario == "proportion_decl" ~ "poll_decl"))


#Convert dataset to a format which is compatible with nutrient intake data - this involves creating some new empty columns to match those in nutrient intake dataset
final_economic_data$resp_id <- ""
final_economic_data$resp_cat <- ""
final_economic_data$sex <- ""
final_economic_data$nutrient <- "Farming income"


######################################################################################################################################################################################################################
######                              Merge nutrient and farming income data and calculate proportional changes resulting from pollinator scenarios            #########################################################
######################################################################################################################################################################################################################

#Calculate proportional changes in the intake of each nutrient
proportional_changes <- df_long_scenarios %>%
  mutate(no_poll = ((no_poll - original)/original),
         poll_incr = ((poll_incr - original)/original),
         poll_decl = ((poll_decl - original)/original))

#Convert to long format to make it compatible with the economic data
df_long_proportions <- proportional_changes %>%
  pivot_longer(cols = -c(hh_barcode, resp_id, village_code, resp_cat, sex, nutrient),
               names_to = "scenario",
               values_to = "value")

#Bind together the economic and nutritional data
nutrition_and_economic <- rbind(df_long_proportions, final_economic_data)

#Summarise means and SD across all participants (long format)
data_to_show <- nutrition_and_economic[nutrition_and_economic$scenario %in% c("no_poll", "poll_incr", "poll_decl"), ]
poll_change_summary <- data_to_show  %>%
 dplyr::group_by(nutrient,scenario)  %>%
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   st_dev = sd(value, na.rm = TRUE),
                   st_error = mean(st_dev / sqrt(n())))

#Summarise means and SD of all nutrients across all participants  (wide format)
poll_change_summary_wide <- poll_change_summary %>%
  pivot_wider(names_from = scenario,
              values_from = c(mean, st_dev, st_error),
              names_sep = "_")

#Remove the two irrelevant rows (grams and poll)
rows_to_remove <- c("grams", "poll")

# Filter the dataframe to exclude rows where the 'scenario' column matches any of these values
poll_change_summary_wide <- poll_change_summary_wide[!(poll_change_summary_wide$nutrient %in% rows_to_remove), ]

#Save as csv
write.csv(poll_change_summary_wide, "output_data/nutrients_income_prop_change_from_poll_change.csv")

###################################################################################################################################
###### Plot proportional changes in nutrient intake and farming income resulting from pollinator scenarios - all participants
###################################################################################################################################

#Plot change in nutrient intake values as a violin plot 
data_to_plot <- nutrition_and_economic[nutrition_and_economic$scenario %in% c("no_poll", "poll_incr", "poll_decl"), ] #Define which scenarios to plot
data_to_plot <- data_to_plot[data_to_plot$nutrient %in% c("Calcium", "Iron", "VitaminC", "FolateTotal", "VitARE", "Farming income"), ] #Define which nutrients to plot

#Change label names
data_to_plot <- data_to_plot %>%
  mutate(nutrient = case_when(nutrient == "VitaminC" ~ "Vitamin C",
                              nutrient == "FolateTotal" ~ "Folate",
                              nutrient == "VitARE" ~ "Vitamin A",
                              TRUE ~ nutrient))

#Define order of nutrients to plot
nutrient_order <- c("Farming income", "Vitamin A", "Folate", "Vitamin C" ,  "Calcium", "Iron")

# Convert 'nutrient' to a factor with the specified order
data_to_plot$nutrient <- factor(data_to_plot$nutrient, levels = nutrient_order)

# Manually specifying colors for each scenario
scenario_colors <- c("no_poll" = "#B80408", "poll_incr" = "#4EB265", "poll_decl" = "#EE8026")

# Scaling to percentage scale
data_to_plot$value <- data_to_plot$value * 100  # Assuming 'value' represents a percentage change

# Calculate mean values for each category so that these can be printed onto the plot
mean_values <- data_to_plot %>%
  dplyr::group_by(nutrient, scenario) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

#Plot proportional changes in nutrient intake and farming income resulting from different scenarios of pollinator change
nutrient_change_plot <- ggplot(data_to_plot, aes(x = scenario, y = value, fill = scenario)) +
  geom_violin(scale = "width", alpha = 0.8) +  # Use geom_violin instead of geom_density_ridges
  facet_wrap(~ nutrient, scales = "free_y") +  # Facet by nutrient with free scales for y-axis
  labs(title = "",
       x = "",  # Remove the x-axis label
       y = "Percentage change from current level (%)") +
  scale_fill_manual(name = "Pollinator change scenario",
                    values = scenario_colors, 
                    labels = c("Loss", "Decline (up to 2030)", "Enhancement")) +  # Define the scenario names
  scale_y_continuous(limits = c(-60, 30), breaks = seq(-60, 30, by = 10)) +  # Adjust the breaks as needed
  theme_bw() +  # Use theme_bw to retain the box around facets and facet labels
  theme(panel.grid = element_blank(),  # Remove all grid lines
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        legend.position = "bottom",  # Move the legend to the bottom
        legend.justification = "center",  # Center the legend horizontally
        legend.direction = "horizontal",  # Arrange legend items horizontally
        legend.box = "horizontal",  # Arrange legend items in a horizontal box
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Reduce margin around the legend
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Reduce margin around the legend box
        strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels with black border
        strip.text = element_text(face = "bold")) +  # Make the facet label text bold
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Horizontal line at zero
  geom_text(data = mean_values, aes(x = scenario, y = mean_value, label = round(mean_value, 1)), 
            position = position_nudge(y = 0.3), size = 3, color = "white")  # Add mean value labels 

#Save plots 
ggsave(plot=nutrient_change_plot, filename="plots/Nutrient_income_change_scenarios.svg", width=6, height=5, dpi=600, bg="white")
ggsave(plot=nutrient_change_plot, filename="plots/Nutrient_income_change_scenarios.png", width=6, height=5, dpi=600, bg="white")


###################################################################################################################################
###### Plot proportional changes in nutrient intake and farming income BY VILLAGE
###################################################################################################################################

#Summarise mean and standard error of proportional changes by village
village_level_data <- data_to_plot

#Extract village names from the resp_id codes
village_level_data <- village_level_data %>% mutate(village = str_extract(hh_barcode, "[^_]+$"))

#Summarise mean and SE values for each village
village_values <- village_level_data %>%
  dplyr::group_by(village, nutrient, scenario) %>%
  dplyr::summarise(mean = mean(value, na.rm = TRUE),
                   SE = mean(sd(value, na.rm = TRUE) / sqrt(n())))

#Set names of villages to four letter codes
village_name_mapping <- c("Chaur" = "CHAU",
                          "Chum" = "CHUM",
                          "Gadigaun" = "GADI",
                          "Lorpa" = "LORP",
                          "Lum" = "LUMA",
                          "Patmara" = "PATM",
                          "Pere" = "PERE",
                          "Rini" = "RINI",
                          "Tirkhu" = "TIRK",
                          "Urthu" = "URTH")

#Manually specify the labels for scenarios
scenario_labels <- c("no_poll" = "1) Loss",
                     "poll_decl" = "2) Decline",
                     "poll_incr" = "3) Enhancement")

#Define variable order based on scenario labels
village_values <- village_values %>%
  mutate(scenario = factor(scenario, levels = names(scenario_labels)))

#CHange vllage names from four letter codes to actual village names
village_values <- village_values %>%
  mutate(village = recode(village, !!!village_name_mapping))

#Remove rows where standard error hasn't calculated (i.e. there is only one value for the village)
village_values <- village_values %>% filter(!is.na(SE))

# Manually specify the colors for each village
village_colors <- c("CHAU" = "#863771",
                    "CHUM" = "#1A66AB",
                    "GADI" = "#7BAFDD",
                    "LORP" = "#4CB364",
                    "LUMA" = "#CAE0AC",
                    "PATM" = "#F3EC5A",
                    "PERE" = "#EE8238",
                    "RINI" = "#E96138",
                    "TIRK" = "#DC3439",
                    "URTH" = "#68312D")

# Create the plot
village_level_change <- ggplot(village_values, aes(x = scenario, y = mean, color = village)) +
  geom_point(size = 2, alpha = 0.7, position = position_jitter(width = 0.3)) +  # Increase point size and jitter points
  facet_wrap(~ nutrient, scales = "free_y", ncol = 2, nrow = 3) +  # Create separate panels for each nutrient
  scale_color_manual(values = village_colors) +  # Manually set colors
  scale_x_discrete(labels = scenario_labels) +  # Manually specify labels for 'scenario'
  theme_minimal() +  # Apply a minimal theme for clean visuals
  theme_bw() +  # Use theme_bw to retain the box around facets and facet labels
  theme(#panel.grid = element_blank(),  # Remove all grid lines
    axis.text.x = element_text(angle = 270, hjust = 0),  # Rotate x-axis labels if needed
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    strip.background = element_rect(fill = "white", color = "black"),  # White background for facet labels with black border
    strip.text = element_text(face = "bold")) +  # Make the facet label text bold
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Horizontal line at zero
  labs(title = "",
       x = "Change in the pollination service",
       y = "Mean percentage change",
       color = "Village")


ggsave(plot=village_level_change, filename="plots/Nutrient_change_scenarios_village.svg", width=5, height=7, dpi=600, bg="white")
ggsave(plot=village_level_change, filename="plots/Nutrient_change_scenarios_village.png", width=5, height=7, dpi=600, bg="white")


###################################################################################################################################
###### Testing and plotting differences in the decline of each nutrient by respondent category and village
###################################################################################################################################

#### Vitamin A 
#Subset data to vitamin A only
vitamin_A_no_poll <- subset(data_to_plot, nutrient == "Vitamin A" & scenario == "no_poll")
vitamin_A_model <- lmer(value ~ resp_cat + (1 | village_code), data = vitamin_A_no_poll)
summary(vitamin_A_model)
vitamin_A_post_hoc <- emmeans(vitamin_A_model, pairwise ~ resp_cat)
summary(vitamin_A_post_hoc)

#Extract predicted marginal means and plot
vitamin_A_predicted_means <- emmeans(vitamin_A_model, ~ resp_cat)
vitamin_A_predicted_means_df <- as.data.frame(vitamin_A_predicted_means)
vitamin_A_predicted_means_df$nutrient <- "Vitamin A"

#### Folate 
#Subset data to Folate only
Folate_no_poll <- subset(data_to_plot, nutrient == "Folate" & scenario == "no_poll")
Folate_model <- lmer(value ~ resp_cat + (1 | village_code), data = Folate_no_poll)
summary(Folate_model)

#Extract predicted marginal means and plot
Folate_predicted_means <- emmeans(Folate_model, ~ resp_cat)
Folate_predicted_means_df <- as.data.frame(Folate_predicted_means)
Folate_predicted_means_df$nutrient <- "Folate"


#### Vitamin_C 
#Subset data to Vitamin_C only
Vitamin_C_no_poll <- subset(data_to_plot, nutrient == "Vitamin C" & scenario == "no_poll")
Vitamin_C_model <- lmer(value ~ resp_cat + (1 | village_code), data = Vitamin_C_no_poll)
summary(Vitamin_C_model)

#Extract predicted marginal means and plot
Vitamin_C_predicted_means <- emmeans(Vitamin_C_model, ~ resp_cat)
Vitamin_C_predicted_means_df <- as.data.frame(Vitamin_C_predicted_means)
Vitamin_C_predicted_means_df$nutrient <- "Vitamin C"


#### Calcium 
#Subset data to Calcium only
Calcium_no_poll <- subset(data_to_plot, nutrient == "Calcium" & scenario == "no_poll")
Calcium_model <- lmer(value ~ resp_cat + (1 | village_code), data = Calcium_no_poll)
summary(Calcium_model)

#Extract predicted marginal means and plot
Calcium_predicted_means <- emmeans(Calcium_model, ~ resp_cat)
Calcium_predicted_means_df <- as.data.frame(Calcium_predicted_means)
Calcium_predicted_means_df$nutrient <- "Calcium"


#### Iron 
#Subset data to Iron only
Iron_no_poll <- subset(data_to_plot, nutrient == "Iron" & scenario == "no_poll")
Iron_model <- lmer(value ~ resp_cat + (1 | village_code), data = Iron_no_poll)
summary(Iron_model)

#Extract predicted marginal means and plot
Iron_predicted_means <- emmeans(Iron_model, ~ resp_cat)
Iron_predicted_means_df <- as.data.frame(Iron_predicted_means)
Iron_predicted_means_df$nutrient <- "Iron"

all_nutrients_predicted_means <- rbind(vitamin_A_predicted_means_df, Folate_predicted_means_df, Vitamin_C_predicted_means_df,
                                       Calcium_predicted_means_df, Iron_predicted_means_df)

#Now calculate overall means for each nutrient
overall_means <- all_nutrients_predicted_means %>%dplyr::group_by(nutrient) %>%dplyr::summarise(emmean = mean(emmean),
                                                                                    lower.CL = mean(lower.CL),
                                                                                    upper.CL = mean(upper.CL))
overall_means <- overall_means %>% mutate(resp_cat = "Overall")
combined_data_to_plot <- bind_rows(all_nutrients_predicted_means, overall_means)



#Plot the predicted means for each nutrient

# Recode the respondent categories
combined_data_to_plot <- combined_data_to_plot %>%
  mutate(resp_cat = recode(resp_cat,
                           "adol_fem" = "Adolescent girl",
                           "adult_fem" = "Adult woman",
                           "adult_male" = "Adult man",
                           "u5_child" = "u5 child"))

#Define the order of the variables to plot
combined_data_to_plot <- combined_data_to_plot %>% mutate(resp_cat = factor(resp_cat, levels = c("Overall", "Adolescent girl", "Adult woman", "Adult man", "u5 child")))


custom_colors <- c("Overall" = "black",
                   "Adolescent girl" = "#F2C4C4", 
                   "Adult woman" = "#F29472", 
                   "Adult man" = "#5F9595", 
                   "u5 child" = "#F0BC68")

resp_cat_nutrient_change_plot <- ggplot(combined_data_to_plot, aes(x = resp_cat, y = emmean/100, color = resp_cat)) + #Mean values are divided by 100 to give proportions so that percentage values plot correctly
  geom_point(size = 2) +  # Plot the predicted means
  scale_y_continuous(labels = percent_format(accuracy = 1)) + #, limits = c(-0.3, -0.05)) +  # Change y-axis to percentage and set limits
  geom_errorbar(aes(ymin = lower.CL/100, ymax = upper.CL/100), size = 1, width = 0) +  # Add error bars
  scale_color_manual(values = custom_colors) +  # Use custom colors
  facet_wrap(~ nutrient, scales = "free_y", ncol = 2, nrow = 3) +  # Create a separate panel for each nutrient
  labs(title = "",
       x = "",
       y = "Predicted decline in nutrient intake ± CI") +
  theme_minimal() +  # Use a minimal theme
  theme(legend.position = "none",  # Remove legend since it's redundant
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        strip.background = element_rect(fill = "white", color = "black"),  # White background with black border for facet labels
        strip.text = element_text(face = "bold"))  # Make the facet label text bold

ggsave(plot=resp_cat_nutrient_change_plot, filename="plots/Nutrient_change_scenarios_resp_cat.svg", width=5, height=7, dpi=600, bg="white")
ggsave(plot=resp_cat_nutrient_change_plot, filename="plots/Nutrient_change_scenarios_resp_cat.png", width=5, height=7, dpi=600, bg="white")




############################################################################################################################################################################
############################               END OF SCRIPT - ALL CODE AFTER THIS IS FOR SENSITIVITY ANALYSES ONLY             ################################################
############################################################################################################################################################################











#######################################################################################################################
########## Implement replacement of missing food items based on calorific replacement with POTATOES instead of rice
#######################################################################################################################

nutrient_intake_by_day <- nutrient_intake_by_day %>%
  mutate(kcal_loss_no_poll = int_Energy_Kcal - Energy_no_poll,
         kcal_loss_poll_decl = int_Energy_Kcal - int_Energy_poll_decl)


#Calculate grams of potatoes this translates to in units of 100 grams (potatoes have 69.814 kcal per 100 gram). This tells us how much much potato we need to replace in the diet in units of 100 grams
#The reason for keeping the units as 100 grams is it allows to simply multiply the densisites of each nutrient per hundred grams in the next step, rather than having to convert them to nutrient/gram
nutrient_intake_by_day <- nutrient_intake_by_day %>%
  mutate(additional_potato_no_poll = kcal_loss_no_poll/69.814,
         additional_potato_poll_decl = kcal_loss_poll_decl/69.814)

#Add extra nutrients provided by potato
nutrient_intake_with_replacement <- nutrient_intake_by_day %>%
  mutate(Energy_no_poll = Energy_no_poll + (additional_potato_no_poll*69.814),  #Energy  value from FCT for 'Potato (uncooked)'
         Fat_no_poll = Fat_no_poll + (additional_potato_no_poll*0.23),  #Fat  value from FCT for 'Potato (uncooked)'
         Protein_no_poll = Protein_no_poll + (additional_potato_no_poll*1.54),  #Protein  value from FCT for 'Potato (uncooked)'
         Carbohydrate_no_poll = Carbohydrate_no_poll + (additional_potato_no_poll*14.8),  #Carbohydrate  value from FCT for 'Potato (uncooked)'
         TotalFibre_no_poll = TotalFibre_no_poll + (additional_potato_no_poll*1.71),  #TotalFibre  value from FCT for 'Potato (uncooked)'
         Insolublefibre_no_poll = Insolublefibre_no_poll + (additional_potato_no_poll*1.13),  #Insolublefibre  value from FCT for 'Potato (uncooked)'
         Solublefibre_no_poll = Solublefibre_no_poll + (additional_potato_no_poll*0.58),  #Solublefibre  value from FCT for 'Potato (uncooked)'
         Calcium_no_poll = Calcium_no_poll + (additional_potato_no_poll*9.52),  #Calcium  value from FCT for 'Potato (uncooked)'
         Iron_no_poll = Iron_no_poll + (additional_potato_no_poll*0.57),  #Iron  value from FCT for 'Potato (uncooked)'
         Zinc_no_poll = Zinc_no_poll + (additional_potato_no_poll*0.28),  #Zinc  value from FCT for 'Potato (uncooked)'
         VitaminC_no_poll = VitaminC_no_poll + (additional_potato_no_poll*23.1),  #VitaminC  value from FCT for 'Potato (uncooked)'
         ThiaminB1_no_poll = ThiaminB1_no_poll + (additional_potato_no_poll*0.06),  #ThiaminB1  value from FCT for 'Potato (uncooked)'
         RiboflavinB2_no_poll = RiboflavinB2_no_poll + (additional_potato_no_poll*0.01),  #RiboflavinB2  value from FCT for 'Potato (uncooked)'
         NiacinB3_no_poll = NiacinB3_no_poll + (additional_potato_no_poll*1.04),  #NiacinB3  value from FCT for 'Potato (uncooked)'
         VitaminB6pyridoxine_no_poll = VitaminB6pyridoxine_no_poll + (additional_potato_no_poll*0.10),  #VitaminB6pyridoxine  value from FCT for 'Potato (uncooked)'
         FolateTotal_no_poll = FolateTotal_no_poll + (additional_potato_no_poll*15.5),  #FolateTotal  value from FCT for 'Potato (uncooked)'
         VitARE_no_poll = VitARE_no_poll + (additional_potato_no_poll*0),  #VitARE  value from FCT for 'Potato (uncooked)'
         Retinol_no_poll = Retinol_no_poll + (additional_potato_no_poll*0),  #Retinol  value from FCT for 'Potato (uncooked)'
         AlphaCarotene_no_poll = AlphaCarotene_no_poll + (additional_potato_no_poll*0),  #AlphaCarotene  value from FCT for 'Potato (uncooked)'
         BetaCarotene_no_poll = BetaCarotene_no_poll + (additional_potato_no_poll*0),  #BetaCarotene  value from FCT for 'Potato (uncooked)'
         VitaminE_no_poll = VitaminE_no_poll + (additional_potato_no_poll*0.06),  #VitaminE  value from FCT for 'Potato (uncooked)'
         VitaminB12_no_poll = VitaminB12_no_poll + (additional_potato_no_poll*0),  #VitaminB12  value from FCT for 'Potato (uncooked)'
         Phytate_no_poll = Phytate_no_poll + (additional_potato_no_poll*55.7),    #Phytate  value from FCT for 'Potato (uncooked)' 
         
         #Mean values for poll decline
         int_Energy_poll_decl = int_Energy_poll_decl +   (additional_potato_poll_decl*69.814),  
         int_Fat_poll_decl = int_Fat_poll_decl +   (additional_potato_poll_decl*0.23),  
         int_Protein_poll_decl = int_Protein_poll_decl +   (additional_potato_poll_decl*1.54),  
         int_Carbohydrate_poll_decl = int_Carbohydrate_poll_decl +   (additional_potato_poll_decl*14.8),  
         int_TotalFibre_poll_decl = int_TotalFibre_poll_decl +   (additional_potato_poll_decl*1.71),  
         int_Insolublefibre_poll_decl = int_Insolublefibre_poll_decl +   (additional_potato_poll_decl*1.13),  
         int_Solublefibre_poll_decl = int_Solublefibre_poll_decl +   (additional_potato_poll_decl*0.58),  
         int_Calcium_poll_decl = int_Calcium_poll_decl +   (additional_potato_poll_decl*9.52),  
         int_Iron_poll_decl = int_Iron_poll_decl +   (additional_potato_poll_decl*0.57),  
         int_Zinc_poll_decl = int_Zinc_poll_decl +   (additional_potato_poll_decl*0.28),  
         int_VitaminC_poll_decl = int_VitaminC_poll_decl +   (additional_potato_poll_decl*23.1),  
         int_ThiaminB1_poll_decl = int_ThiaminB1_poll_decl +   (additional_potato_poll_decl*0.06),  
         int_RiboflavinB2_poll_decl = int_RiboflavinB2_poll_decl +   (additional_potato_poll_decl*0.01),  
         int_NiacinB3_poll_decl = int_NiacinB3_poll_decl +   (additional_potato_poll_decl*1.04),  
         int_VitaminB6pyridoxine_poll_decl = int_VitaminB6pyridoxine_poll_decl +   (additional_potato_poll_decl*0.10),  
         int_FolateTotal_poll_decl = int_FolateTotal_poll_decl +   (additional_potato_poll_decl*15.5),  
         int_VitARE_poll_decl = int_VitARE_poll_decl +   (additional_potato_poll_decl*0),  
         int_Retinol_poll_decl = int_Retinol_poll_decl +   (additional_potato_poll_decl*0),  
         int_AlphaCarotene_poll_decl = int_AlphaCarotene_poll_decl +   (additional_potato_poll_decl*0),  
         int_BetaCarotene_poll_decl = int_BetaCarotene_poll_decl +   (additional_potato_poll_decl*0),  
         int_VitaminE_poll_decl = int_VitaminE_poll_decl +   (additional_potato_poll_decl*0.06),  
         int_VitaminB12_poll_decl = int_VitaminB12_poll_decl +   (additional_potato_poll_decl*0),  
         int_Phytate_poll_decl = int_Phytate_poll_decl +   (additional_potato_poll_decl*55.7)) %>%
  ungroup() 


##############################################################################
###### Subset data to essential columns, rename and export for MPA analysis
##############################################################################
data_to_export <- nutrient_intake_with_replacement
colnames(data_to_export) <- gsub("int_", "", colnames(data_to_export))

#Subset to only original intake data
original_intake_data <- select(data_to_export, 
                               "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                               "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                               "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                               "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                               "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Subset to poll-loss intake data
no_poll_intake_data <- select(data_to_export, 
                              "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                              "Energy_no_poll", "Fat_no_poll", "Protein_no_poll", "Carbohydrate_no_poll", "TotalFibre_no_poll", "Insolublefibre_no_poll", "Solublefibre_no_poll",
                              "Calcium_no_poll", "Iron_no_poll", "Zinc_no_poll", "VitaminC_no_poll", "ThiaminB1_no_poll", "RiboflavinB2_no_poll",
                              "NiacinB3_no_poll", "VitaminB6pyridoxine_no_poll", "FolateTotal_no_poll", "VitARE_no_poll", "Retinol_no_poll", "AlphaCarotene_no_poll",
                              "BetaCarotene_no_poll", "VitaminE_no_poll", "VitaminB12_no_poll", "Phytate_no_poll")

#Rename columns
colnames(no_poll_intake_data) <- c("formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                   "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                                   "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                                   "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                                   "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Subset to poll-enhancement intake data
poll_incr_intake_data <- select(data_to_export, 
                                "formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                "Energy_poll_incr", 	"Fat_poll_incr",	"Protein_poll_incr",	"Carbohydrate_poll_incr",	"TotalFibre_poll_incr",	
                                "Insolublefibre_poll_incr",	"Solublefibre_poll_incr",	"Calcium_poll_incr",	"Iron_poll_incr",	"Zinc_poll_incr",	
                                "VitaminC_poll_incr",	"ThiaminB1_poll_incr",	"RiboflavinB2_poll_incr",	"NiacinB3_poll_incr",	"VitaminB6pyridoxine_poll_incr",	
                                "FolateTotal_poll_incr",	"VitARE_poll_incr",	"Retinol_poll_incr",	"AlphaCarotene_poll_incr",	"BetaCarotene_poll_incr",	"VitaminE_poll_incr",	
                                "VitaminB12_poll_incr",	"Phytate_poll_incr")

#Rename columns
colnames(poll_incr_intake_data) <- c("formid", "hh_barcode", "resp_id", "village_code", "date", "month", "season", "resp_cat", "sex", "age_y_at_recall","currently_pregnant","mother_breastfeeding_2wks", "periods_yet",
                                     "Energy_Kcal", "Fat_g", "Protein_g", "Carbohydrate_g", "TotalFibre_g", "Insolublefibre_g", "Solublefibre_g",
                                     "Calcium_mg", "Iron_mg", "Zinc_mg", "VitaminC_mg", "ThiaminB1_mg", "RiboflavinB2_mg",
                                     "NiacinB3_mg", "VitaminB6pyridoxine_mg", "FolateTotal_µg", "VitARE_µg", "Retinol_µg", "AlphaCarotene_µg",
                                     "BetaCarotene_µg", "VitaminE_mg", "VitaminB12_µg", "Phytate_mg")


#Export these data to be used for calculating probability of adequacy (in Script_04 - remove hash keys if you want to conduct this sensitivity analysis at next step
#write.csv(original_intake_data, "output_data/Original_intake_data_potato_repl.csv")

#write.csv(no_poll_intake_data, "output_data/No-poll_intake_data_potato_repl.csv")

#write.csv(poll_incr_intake_data, "output_data/Poll-incr_intake_data_potato_repl.csv")


##############################################################################
###### Bind dataframes in long format 
##############################################################################
original_intake_data$scenario <- "original"
no_poll_intake_data$scenario <- "poll_loss"
poll_incr_intake_data$scenario <- "poll_incr"

##############################################################################
###### Calculate mean daily intakes for each person 
##############################################################################

#Change all units symbols in column headers to specify that these columns show original intakes
data_to_summarise <- data_to_export

colnames(data_to_summarise) <- gsub("_Kcal", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_g", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_mg", "_original", colnames(data_to_summarise))
colnames(data_to_summarise) <- gsub("_µg", "_original", colnames(data_to_summarise))

data_to_summarise <- select(data_to_summarise, -formid, -date, -month, -season, -age_y_at_recall, -currently_pregnant,
                            -mother_breastfeeding_2wks, -periods_yet, -kcal_loss_no_poll, -kcal_loss_poll_decl, 
                            -additional_rice_no_poll, -additional_rice_poll_decl)


mean_daily_intakes <- data_to_summarise %>%
 dplyr::group_by(hh_barcode, resp_id, village_code, resp_cat, sex) %>%
 dplyr::summarise_all(~mean(., na.rm = TRUE))

#Restructure data so that scenarios remain as column headers and nutrients become a new variable
df_long_scenarios <- mean_daily_intakes %>%
  pivot_longer(
    cols = -c(hh_barcode, resp_id, village_code, resp_cat, sex),
    names_to = c("nutrient", ".value"),
    names_pattern = "^(.+?)_(.+)$",
    values_transform = list(variable = factor))


###################################################################################################################################
###### Calculate proportional changes in nutrient intake and farming income for each participant resulting from pollinator scenarios
###################################################################################################################################

proportional_changes <- df_long_scenarios %>%
  mutate(no_poll = ((no_poll - original)/original),
         poll_incr = ((poll_incr - original)/original),
         poll_decl = ((poll_decl - original)/original))


#Summarise means and SD across all participants  (wide format)
poll_change_summary_wide <- proportional_changes  %>%
 dplyr::group_by(nutrient)  %>%
  dplyr::summarise(loss_mean = mean(no_poll, na.rm = TRUE),
                   loss_st_dev = sd(no_poll, na.rm = TRUE),
                   decl_mean = mean(poll_decl, na.rm = TRUE),
                   decl_st_dev = sd(poll_decl, na.rm = TRUE),
                   incr_mean = mean(poll_incr, na.rm = TRUE),
                   incr_st_dev = sd(poll_incr, na.rm = TRUE),)

#Remove the two irrelevant rows (additional, grams and poll)
rows_to_remove <- c("additional", "grams", "poll")

# Filter the dataframe to exclude rows where the 'scenario' column matches any of these values
poll_change_summary_wide <- poll_change_summary_wide[!(poll_change_summary_wide$nutrient %in% rows_to_remove), ]

#Save as csv
write.csv(poll_change_summary_wide, "output_data/Pollinator_change_summaries_potato_repl.csv")


