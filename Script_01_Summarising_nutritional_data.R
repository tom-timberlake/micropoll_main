##################################################################################################################################################################
#################   MICRO-POLL SCRIPT 1 - IMPORTING DEITARY DATA, CALCULATING INTAKES & POLLINATOR DEPENDENCE  ###################################################
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
library(haven) #Reading in STata files
library(gridExtra)
library(bipartite)
library(ggplot2)
library(plyr)
library(cowplot)
library(readstata13) #Reading in STata files

##Import dietary data
dietary_data <- read.csv("input_data/Dietary_data_condensed.csv") 


########################################################
#####  Data tidying to prepare for calculations  #######
########################################################

#Convert all NA values in the nutrient retention columns to 1, so that uncategorised foods are not ignored.
# Define nutrient retention columns
retention_columns <- c("Calcium_retention", "Iron_retention", "Zinc_retention", "VitC_retention", "Thiamin_retention",
                       "Riboflavin_retention", "Niacin_retention", "VitB6_retention", "Folate_retention", "VitB12_retention",        
                       "VitA_RE_retention", "Beta_catotene_retention", "Alpha_catotene_retention")


# Replace NA values with 1 in specified columns
dietary_data <- dietary_data %>%
  dplyr::mutate(across(all_of(retention_columns), ~replace_na(., 1)))


#####################################################################################################
########## Calculate original intake values for each nutrient, taking account of nutrient retention
#####################################################################################################

#Intake of each nutrient is calculated by multiplying grams of each ingredient consumed by the nutrient density of the ingredient 
#(values are divided by 100 because nutrient density values are expressed per 100 grams of ingredient)
#Where relevant (i.e. for micronutrients), the nutrient consumption value is then multiplied by the nutrient retention factor for that food to account for losses due to cooking
#Nutrient retention factors are taken from the USDA Table of Nutrient Retention Factors, Release 6 (2007)

#Caclulate grams of each ingredient consumed
diet_ingred_intake_data <- dietary_data %>%
  dplyr::mutate(int_Energy_Kcal = ingredient_grams_consumed * Energy_Kcal_per100g / 100,
                int_Fat_g = ingredient_grams_consumed * Fat_g_per100g / 100,
                int_Protein_g = ingredient_grams_consumed * Protein_g_per100g / 100,
                int_Carbohydrate_g = ingredient_grams_consumed * Carbohydrate_g_per100g / 100,
                int_TotalFibre_g = ingredient_grams_consumed * TotalFibre_g_per100g / 100,
                int_Insolublefibre_g = ingredient_grams_consumed * Insolublefibre_g_per100g / 100,
                int_Solublefibre_g = ingredient_grams_consumed * Solublefibre_g_per100g / 100,
                int_Calcium_mg = (ingredient_grams_consumed * Calcium_mg_per100g / 100) * Calcium_retention,
                int_Iron_mg = (ingredient_grams_consumed * Iron_mg_per100g / 100) * Iron_retention,
                int_Zinc_mg = (ingredient_grams_consumed * Zinc_mg_per100g / 100) * Zinc_retention,
                int_VitaminC_mg = (ingredient_grams_consumed * VitaminC_mg_per100g / 100) * VitC_retention,
                int_ThiaminB1_mg = (ingredient_grams_consumed * ThiaminB1_mg_per100g / 100) * Thiamin_retention,
                int_RiboflavinB2_mg = (ingredient_grams_consumed * RiboflavinB2_mg_per100g / 100) * Riboflavin_retention,
                int_NiacinB3_mg = (ingredient_grams_consumed * NiacinB3_mg_per100g / 100) * Niacin_retention,
                int_VitaminB6pyridoxine_mg = (ingredient_grams_consumed * VitaminB6pyridoxine_mg_per100g / 100) * VitB6_retention,
                int_FolateTotal_µg = (ingredient_grams_consumed * FolateTotal_µg_per100g / 100) * Folate_retention,
                int_VitARE_µg = (ingredient_grams_consumed * VitARE_µg_per100g / 100) * VitA_RE_retention,
                int_Retinol_µg = ingredient_grams_consumed * Retinol_µg_per100g / 100,
                int_AlphaCarotene_µg = (ingredient_grams_consumed * AlphaCarotene_µg_per100g / 100) * Alpha_catotene_retention,
                int_BetaCarotene_µg = (ingredient_grams_consumed * BetaCarotene_µg_per100g / 100) * Beta_catotene_retention,
                int_VitaminE_mg = ingredient_grams_consumed * VitaminE_mg_per100g / 100,
                int_VitaminB12_µg = (ingredient_grams_consumed * VitaminB12_µg_per100g / 100) * VitB12_retention,
                int_Phytate_mg = ingredient_grams_consumed * Phytate_mg_per100g / 100)


#Convert all NA values in the nutrient intake columns to 0, so that calculations can be made without throwing errors.
# Define nutrient intake columns
intake_columns <- c("ingredient_grams_consumed", "int_Energy_Kcal", "int_Fat_g", "int_Protein_g", "int_Carbohydrate_g", "int_TotalFibre_g", "int_Insolublefibre_g", "int_Solublefibre_g", "int_Calcium_mg",                
                    "int_Iron_mg", "int_Zinc_mg", "int_VitaminC_mg", "int_ThiaminB1_mg", "int_RiboflavinB2_mg", "int_NiacinB3_mg", "int_VitaminB6pyridoxine_mg",
                    "int_FolateTotal_µg", "int_VitARE_µg", "int_Retinol_µg", "int_AlphaCarotene_µg", "int_BetaCarotene_µg", "int_VitaminE_mg", "int_VitaminB12_µg", "int_Phytate_mg")

# Replace NA values with 0 in specified columns
diet_ingred_intake_data <- diet_ingred_intake_data %>%
  dplyr::mutate(across(all_of(intake_columns), ~replace_na(., 0)))

#Save nutrient intake data as csv
#write.csv(diet_ingred_intake_data, "C:/Users/tt15117/OneDrive - University of Bristol/Nepal Project/Data analysis/Output datasets/Nutrient_intake_data.csv")

#########################################################################################################
##########        Calculate daily intakes for each participant and respondent category        ##########
#########################################################################################################

#Calculte daily intake sum
daily_intakes <- diet_ingred_intake_data %>%
 dplyr::group_by(formid, date, resp_id, resp_cat) %>%
  dplyr::summarise(Grams = sum(ingredient_grams_consumed, na.rm = TRUE), 
                   Energy = sum(int_Energy_Kcal, na.rm = TRUE),
                   Fat = sum(int_Fat_g, na.rm = TRUE),
                   Protein = sum(int_Protein_g, na.rm = TRUE),
                   Carbohydrate = sum(int_Carbohydrate_g, na.rm = TRUE),
                   TotalFibre = sum(int_TotalFibre_g, na.rm = TRUE),
                   Insolublefibre = sum(int_Insolublefibre_g, na.rm = TRUE),
                   Solublefibre = sum(int_Solublefibre_g, na.rm = TRUE),
                   Calcium = sum(int_Calcium_mg, na.rm = TRUE),
                   Iron = sum(int_Iron_mg, na.rm = TRUE),
                   Zinc = sum(int_Zinc_mg, na.rm = TRUE),
                   VitaminC = sum(int_VitaminC_mg, na.rm = TRUE),
                   ThiaminB1 = sum(int_ThiaminB1_mg, na.rm = TRUE),
                   RiboflavinB2 = sum(int_RiboflavinB2_mg, na.rm = TRUE),
                   NiacinB3 = sum(int_NiacinB3_mg, na.rm = TRUE),
                   VitaminB6pyridoxine = sum(int_VitaminB6pyridoxine_mg, na.rm = TRUE),
                   FolateTotal = sum(int_FolateTotal_µg, na.rm = TRUE),
                   VitARE = sum(int_VitARE_µg, na.rm = TRUE),
                   Retinol = sum(int_Retinol_µg, na.rm = TRUE),
                   AlphaCarotene = sum(int_AlphaCarotene_µg, na.rm = TRUE),
                   BetaCarotene = sum(int_BetaCarotene_µg, na.rm = TRUE),
                   VitaminE = sum(int_VitaminE_mg, na.rm = TRUE),
                   VitaminB12 = sum(int_VitaminB12_µg, na.rm = TRUE),
                   Phytate = sum(int_Phytate_mg, na.rm = TRUE)) %>%
  ungroup()


#Remove the text '_mean' from all columns and then remove the resp_id, formid and date columns before calculating means across all other columns
daily_intakes_subset <- select(daily_intakes, -formid, -date)

#Calculate mean daily intake of each participant across all recall events
mean_daily_intakes <- daily_intakes_subset %>%
  dplyr::group_by(resp_id, resp_cat) %>%
  dplyr::summarise_all(list(mean = mean))%>%
  ungroup()

#Remove the text '_mean' from all columns and then remove the resp_id column before calculating means across all other columns
mean_daily_intakes_subset <- mean_daily_intakes %>% dplyr::rename_with(~ gsub("_mean$", "", .))
mean_daily_intakes_subset <- select(mean_daily_intakes_subset, -resp_id)

#Calculate mean daily intake of each respondent category
mean_daily_intakes_cat <- mean_daily_intakes_subset %>%
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise_all(list(mean = mean))


##############################################################################
##########    Calculate pollinator dependence of each nutrient      ##########
##############################################################################

#Change all NA values for poll-dependence to 0
diet_ingred_intake_data <- diet_ingred_intake_data %>%
  mutate(final_poll_dependence = ifelse(is.na(final_poll_dependence), 0, final_poll_dependence))

#Change all NA values for prop_focal_ing to 1
diet_ingred_intake_data <- diet_ingred_intake_data %>%
  mutate(prop_focal_ing = ifelse(is.na(prop_focal_ing), 1, prop_focal_ing))

#Create new column which specifies whether the food item is pollinator dependent or not
diet_ingred_intake_data <- diet_ingred_intake_data %>%
  mutate(PD_yes_no = ifelse(poll_depend_component %in% c("food", "seed"), 1, 0))

### Calculate TOTAL pollinator-dependence of each person's intake (including both imported and local foods)
### Calculations are made by multiplying each nutrient intake value by the proportional pollinator dependence of the food (taking into account foods where only part is PD)
diet_ingred_intake_data_PD <- diet_ingred_intake_data %>%
  mutate(int_grams_PD_y_n = ingredient_grams_consumed * PD_yes_no,
         int_grams_PD = ingredient_grams_consumed * (final_poll_dependence * prop_focal_ing),
         int_Energy_PD = int_Energy_Kcal * (final_poll_dependence * prop_focal_ing),
         int_Fat_PD = int_Fat_g * (final_poll_dependence * prop_focal_ing),
         int_Protein_PD = int_Protein_g * (final_poll_dependence * prop_focal_ing),
         int_Carbohydrate_PD = int_Carbohydrate_g * (final_poll_dependence * prop_focal_ing),
         int_TotalFibre_PD = int_TotalFibre_g * (final_poll_dependence * prop_focal_ing),
         int_Insolublefibre_PD = int_Insolublefibre_g * (final_poll_dependence * prop_focal_ing),
         int_Solublefibre_PD = int_Solublefibre_g * (final_poll_dependence * prop_focal_ing),
         int_Calcium_PD = int_Calcium_mg * (final_poll_dependence * prop_focal_ing),
         int_Iron_PD = int_Iron_mg * (final_poll_dependence * prop_focal_ing),
         int_Zinc_PD = int_Zinc_mg * (final_poll_dependence * prop_focal_ing),
         int_VitaminC_PD = int_VitaminC_mg * (final_poll_dependence * prop_focal_ing),
         int_ThiaminB1_PD = int_ThiaminB1_mg * (final_poll_dependence * prop_focal_ing),
         int_RiboflavinB2_PD = int_RiboflavinB2_mg * (final_poll_dependence * prop_focal_ing),
         int_NiacinB3_PD = int_NiacinB3_mg * (final_poll_dependence * prop_focal_ing),
         int_VitaminB6pyridoxine_PD = int_VitaminB6pyridoxine_mg * (final_poll_dependence * prop_focal_ing),
         int_FolateTotal_PD = int_FolateTotal_µg * (final_poll_dependence * prop_focal_ing),
         int_VitARE_PD = int_VitARE_µg * (final_poll_dependence * prop_focal_ing),
         int_Retinol_PD = int_Retinol_µg * (final_poll_dependence * prop_focal_ing),
         int_AlphaCarotene_PD = int_AlphaCarotene_µg * (final_poll_dependence * prop_focal_ing),
         int_BetaCarotene_PD = int_BetaCarotene_µg * (final_poll_dependence * prop_focal_ing),
         int_VitaminE_PD = int_VitaminE_mg * (final_poll_dependence * prop_focal_ing),
         int_VitaminB12_PD = int_VitaminB12_µg * (final_poll_dependence * prop_focal_ing),
         int_Phytate_PD = int_Phytate_mg * (final_poll_dependence * prop_focal_ing))

#######################################################################################################
########## Calculate & plot source of each food category & nutrient - imported versus local   #########
#######################################################################################################

#Summarise total grams of different food categories consumed and their provenance
# Cluster various food groupings into broader groups
categories_animal_product <- c("Chicken", "Egg", "Fish", "Buffalo", "Dairy", "Duck", "Goat", "Pig", "Rabbit", "Sheep", "Wild meat")
categories_other <- c("Alcohol", "Bombay mix", "Chemical", "Fortified food", "Market item", "Mixed food", "Mixed spice", "Salt", "Supplement", "Ash", "Mushroom", "NA")
categories_veg <- c("Mixed vegetables", "Wild plant")

# Change specified categories to their correct collective category
food_categories_edited <- diet_ingred_intake_data_PD
food_categories_edited$plant_category[food_categories_edited$plant_category %in% categories_animal_product] <- "Animal product"
food_categories_edited$plant_category[food_categories_edited$plant_category %in% categories_other] <- "Other"
food_categories_edited$plant_category[food_categories_edited$plant_category %in% categories_veg] <- "Crop - vegetable"


#Calculate proportion of each major food category that is imported versus local
food_cat_source_summary <- food_categories_edited %>%
 dplyr::group_by(local_imported, plant_category) %>%
  dplyr::summarise(Grams = sum(ingredient_grams_consumed, na.rm = TRUE),
                   Grams_PD = sum(int_grams_PD, na.rm = TRUE)) %>%
  ungroup()

food_cat_source_summary <- food_cat_source_summary %>% filter(!is.na(plant_category))

#Define plotting order
plotting_order_food_cat <- c("Crop - leafy vegetable",	"Crop - vegetable",	"Crop - fruit or nut",	"Animal product",	"Crop - pulse",	"Crop - herb or spice",	"Crop - cereal",	"Other",	"Crop - oilseed")

food_cat_source_summary$plant_category <- factor(food_cat_source_summary$plant_category, levels = plotting_order_food_cat)

#Plot local versus imported quantities of each major food category that is consumed in Jumla
import_local_food_cat_plot <- ggplot(food_cat_source_summary, aes(x = plant_category, y = Grams, fill = factor(local_imported))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "", x = "Food Category", y = "Percentage by grams of consumption", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("import" = "#F5DC83", "local" = "#8FA33F")) +
  scale_x_discrete(labels = c("Crop - cereal" = "Cereal crops",
                              "Animal product" = "Animal products",
                              "Crop - fruit or nut" = "Fruits & nuts",
                              "Crop - herb or spice" = "Herbs & spices",
                              "Crop - leafy vegetable" = "Leafy veg",
                              "Crop - oilseed" = "Oilseeds",
                              "Crop - pulse" = "Pulses",
                              "Crop - vegetable" = "Vegetables")) +
  theme(legend.position = "right",   # Remove legend
        axis.text.x = element_text(size = 12, angle = 45, hjust=1, vjust = 1.2),   # Increase text size for x-axis labels
        axis.text.y = element_text(size = 12), # Increase text size for y-axis labels
        axis.title.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12))

#Save plot
ggsave(plot=import_local_food_cat_plot, filename="plots/Food_cat_import_local.svg", width=6, height=5, dpi=600, bg="white")
ggsave(plot=import_local_food_cat_plot, filename="plots/Food_cat_import_local.png", width=6, height=5, dpi=600, bg="white")


#Summarise the source of each nutrient - local versus imported
nutrient_source_summary <- diet_ingred_intake_data %>%
 dplyr::group_by(local_imported) %>%
  dplyr::summarise(Grams = sum(ingredient_grams_consumed, na.rm = TRUE), 
                   Energy = sum(int_Energy_Kcal, na.rm = TRUE),
                   Fat = sum(int_Fat_g, na.rm = TRUE),
                   Protein = sum(int_Protein_g, na.rm = TRUE),
                   Carbohydrate = sum(int_Carbohydrate_g, na.rm = TRUE),
                   TotalFibre = sum(int_TotalFibre_g, na.rm = TRUE),
                   Insolublefibre = sum(int_Insolublefibre_g, na.rm = TRUE),
                   Solublefibre = sum(int_Solublefibre_g, na.rm = TRUE),
                   Calcium = sum(int_Calcium_mg, na.rm = TRUE),
                   Iron = sum(int_Iron_mg, na.rm = TRUE),
                   Zinc = sum(int_Zinc_mg, na.rm = TRUE),
                   VitaminC = sum(int_VitaminC_mg, na.rm = TRUE),
                   ThiaminB1 = sum(int_ThiaminB1_mg, na.rm = TRUE),
                   RiboflavinB2 = sum(int_RiboflavinB2_mg, na.rm = TRUE),
                   NiacinB3 = sum(int_NiacinB3_mg, na.rm = TRUE),
                   VitaminB6pyridoxine = sum(int_VitaminB6pyridoxine_mg, na.rm = TRUE),
                   FolateTotal = sum(int_FolateTotal_µg, na.rm = TRUE),
                   VitARE = sum(int_VitARE_µg, na.rm = TRUE),
                   Retinol = sum(int_Retinol_µg, na.rm = TRUE),
                   AlphaCarotene = sum(int_AlphaCarotene_µg, na.rm = TRUE),
                   BetaCarotene = sum(int_BetaCarotene_µg, na.rm = TRUE),
                   VitaminE = sum(int_VitaminE_mg, na.rm = TRUE),
                   VitaminB12 = sum(int_VitaminB12_µg, na.rm = TRUE),
                   Phytate = sum(int_Phytate_mg, na.rm = TRUE))


#Restructure local/imported data to enable plotting
nutrient_source_pivot <- as.data.frame(t(nutrient_source_summary))
colnames(nutrient_source_pivot)[1:2] <- c("Imported", "Local")
nutrient_source_pivot <- nutrient_source_pivot[-1, ]
nutrient_source_pivot[] <- lapply(nutrient_source_pivot, as.numeric)
nutrient_source_pivot <- rownames_to_column(nutrient_source_pivot, var = "Nutrient")

#Define which nutrients to show in the plot
nutrients_to_keep<-c("Energy", "Fat", "Protein", "Calcium", "Iron", "Zinc", "VitaminC", 
                     "FolateTotal", "VitARE","VitaminB12", "VitaminE")

# Create a subset of the dataframe
nutrient_source_subset <- nutrient_source_pivot[nutrient_source_pivot$Nutrient %in% nutrients_to_keep, ]

df_melted <- melt(nutrient_source_subset, id.vars = "Nutrient")

#Calculate proportion imports for each nutrient
nutrient_source_proportions <- nutrient_source_subset %>% 
  mutate(proportion_imported = Imported / (Imported + Local))

#Define plotting order of nutrients based on the proportion that is imported
plotting_order <- nutrient_source_proportions %>%
  arrange(proportion_imported) %>%
  pull(Nutrient)

#Define order of categories to plot
custom_order <- c( "Imported", "Local")

#Plot relative proportions of imports and locally-grown for each nutrient
import_local_plot <- ggplot(df_melted, aes(y = (value), x = factor(Nutrient, levels = plotting_order), fill = factor(variable, levels = custom_order))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "(a)",
       x = "",
       y = "% local versus imported") +
  labs(fill = NULL)+
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("Imported" = "#F5DC83", "Local" = "#8FA33F")) +
  theme_minimal() +
  scale_x_discrete(labels = c("Energy" = "Energy",
                              "Grams" = "Grams",
                              "Fat" = "Fat",
                              "Calcium" = "Calcium",
                              "Iron" = "Iron",
                              "Zinc" = "Zinc",
                              "VitaminC" = "Vitamin C",
                              "FolateTotal" = "Folate",
                              "VitARE" = "Vitamin A",
                              "VitaminE" = "Vitamin E",
                              "VitaminB12" = "Vitamin B12")) +
  theme(legend.position = "bottom",  # Move legend to bottom
        legend.box.margin = margin(t = -10),  # Optional: add margin above the legend
        axis.text.x = element_text(size = 12, angle = 45, hjust=1, vjust = 1.2),   # Increase text size for x-axis labels
        axis.text.y = element_text(size = 12), # Increase text size for y-axis labels
        axis.title.y = element_text(face = "bold", size = 12))   

#Note this plot gets combined with a subsequent plot and saved together at a later stage in the code


###################################################################
### Export dataset to be used in subsequent analyses
###################################################################

#Subset to relevant columns
diet_ingred_intake_data_PD_subset <- diet_ingred_intake_data_PD %>%
  select(formid,	hh_barcode,	resp_id,	village_code,	date,	month,	season,	resp_cat,	sex,	age_y_at_recall,	currently_pregnant,	mother_breastfeeding_2wks,	child_breastfeeding_24h, breastfed_times_child, periods_yet,
         food_item_code,	food_item_name, recipe_code,	ingredient_code, ingredient_name.x,	food_cat, recipe_food_type, ingred_food_type, retention_code_cooked,	recipe_grams_consumed, percent_ingred_in_recipe, 	
         ingredient_grams_consumed,	local_imported,	mixed_ingredients,	cultivated_wild,	plant_animal_mineral,	poll_dependent,	prop_focal_ing,	food_grouping,	plant_barcode,	
         plant_category,	sci_name,	eng_name,	nep_name_eng,	final_poll_dependence,	poll_depend_component,	poll_dependence_source,	poll_notes,	int_Energy_Kcal,	int_Fat_g,	int_Protein_g,	
         int_Carbohydrate_g,	int_TotalFibre_g,	int_Insolublefibre_g,	int_Solublefibre_g,	int_Calcium_mg,	int_Iron_mg,	int_Zinc_mg,	int_VitaminC_mg,	int_ThiaminB1_mg,	int_RiboflavinB2_mg,	
         int_NiacinB3_mg,	int_VitaminB6pyridoxine_mg,	int_FolateTotal_µg,	int_VitARE_µg,	int_Retinol_µg,	int_AlphaCarotene_µg,	int_BetaCarotene_µg,	int_VitaminE_mg,	
         int_VitaminB12_µg,	int_Phytate_mg,	int_grams_PD, int_Energy_PD,	int_Fat_PD,	int_Protein_PD,	int_Carbohydrate_PD,	int_TotalFibre_PD,	int_Insolublefibre_PD,	int_Solublefibre_PD,	
         int_Calcium_PD,	int_Iron_PD,	int_Zinc_PD,	int_VitaminC_PD,	int_ThiaminB1_PD,	int_RiboflavinB2_PD,	int_NiacinB3_PD,	int_VitaminB6pyridoxine_PD,	int_FolateTotal_PD,	
         int_VitARE_PD,	int_Retinol_PD,	int_AlphaCarotene_PD,	int_BetaCarotene_PD,	int_VitaminE_PD,	int_VitaminB12_PD,	int_Phytate_PD)


write.csv(diet_ingred_intake_data_PD_subset, "output_data/Nutrient_intake_data_PD.csv")

######################################################################################
########## Summarise poll-dependence of each nutrient at participant level
######################################################################################
PD_summary <- diet_ingred_intake_data_PD_subset %>%
  dplyr::group_by(village_code, resp_id, resp_cat) %>%
  dplyr::summarise(prop_PD_grams = sum(int_grams_PD, na.rm = TRUE) / sum(ingredient_grams_consumed, na.rm = TRUE),
                   prop_PD_Energy = sum(int_Energy_PD, na.rm = TRUE) / sum(int_Energy_Kcal, na.rm = TRUE),
                   prop_PD_Fat = sum(int_Fat_PD, na.rm = TRUE) / sum(int_Fat_g, na.rm = TRUE),
                   prop_PD_Protein = sum(int_Protein_PD, na.rm = TRUE) / sum(int_Protein_g, na.rm = TRUE),
                   prop_PD_Carbohydrate = sum(int_Carbohydrate_PD, na.rm = TRUE) / sum(int_Carbohydrate_g, na.rm = TRUE),
                   prop_PD_TotalFibre = sum(int_TotalFibre_PD, na.rm = TRUE) / sum(int_TotalFibre_g, na.rm = TRUE),
                   prop_PD_Insolublefibre = sum(int_Insolublefibre_PD, na.rm = TRUE) / sum(int_Insolublefibre_g, na.rm = TRUE),
                   prop_PD_Solublefibre = sum(int_Solublefibre_PD, na.rm = TRUE) / sum(int_Solublefibre_g, na.rm = TRUE),
                   prop_PD_Calcium = sum(int_Calcium_PD, na.rm = TRUE) / sum(int_Calcium_mg, na.rm = TRUE),
                   prop_PD_Iron = sum(int_Iron_PD, na.rm = TRUE) / sum(int_Iron_mg, na.rm = TRUE),
                   prop_PD_Zinc = sum(int_Zinc_PD, na.rm = TRUE) / sum(int_Zinc_mg, na.rm = TRUE),
                   prop_PD_VitaminC = sum(int_VitaminC_PD, na.rm = TRUE) / sum(int_VitaminC_mg, na.rm = TRUE),
                   prop_PD_ThiaminB1 = sum(int_ThiaminB1_PD, na.rm = TRUE) / sum(int_ThiaminB1_mg, na.rm = TRUE),
                   prop_PD_RiboflavinB2 = sum(int_RiboflavinB2_PD, na.rm = TRUE) / sum(int_RiboflavinB2_mg, na.rm = TRUE),
                   prop_PD_NiacinB3 = sum(int_NiacinB3_PD, na.rm = TRUE) / sum(int_NiacinB3_mg, na.rm = TRUE),
                   prop_PD_VitaminB6pyridoxine = sum(int_VitaminB6pyridoxine_PD, na.rm = TRUE) / sum(int_VitaminB6pyridoxine_mg, na.rm = TRUE),
                   prop_PD_FolateTotal = sum(int_FolateTotal_PD, na.rm = TRUE) / sum(int_FolateTotal_µg, na.rm = TRUE),
                   prop_PD_VitARE = sum(int_VitARE_PD, na.rm = TRUE) / sum(int_VitARE_µg, na.rm = TRUE),
                   prop_PD_Retinol = sum(int_Retinol_PD, na.rm = TRUE) / sum(int_Retinol_µg, na.rm = TRUE),
                   prop_PD_AlphaCarotene = sum(int_AlphaCarotene_PD, na.rm = TRUE) / sum(int_AlphaCarotene_µg, na.rm = TRUE),
                   prop_PD_BetaCarotene = sum(int_BetaCarotene_PD, na.rm = TRUE) / sum(int_BetaCarotene_µg, na.rm = TRUE),
                   prop_PD_VitaminE = sum(int_VitaminE_PD, na.rm = TRUE) / sum(int_VitaminE_mg, na.rm = TRUE),
                   prop_PD_VitaminB12 = sum(int_VitaminB12_PD, na.rm = TRUE) / sum(int_VitaminB12_µg, na.rm = TRUE),
                   prop_PD_Phytate = sum(int_Phytate_PD, na.rm = TRUE) / sum(int_Phytate_mg, na.rm = TRUE)) %>% 
  ungroup()


#Replace all NA values with zero
PD_summary[is.na(PD_summary)] <- 0

#Summarise at respondent category level
PD_summary_groups <- PD_summary %>%   
  dplyr::group_by(resp_cat) %>%
  dplyr::summarise(across(.cols = -resp_id,
                          .fns = list(mean = ~mean(., na.rm = TRUE), 
                                      sd = ~sd(., na.rm = TRUE), 
                                      se = ~sd(., na.rm = TRUE) / sqrt(n())
                          ),
                          .names = "{.col}_{.fn}"))

#Subset to relevant columns
PD_summary_groups_subset <- select(PD_summary_groups,resp_cat, prop_PD_AlphaCarotene_mean, prop_PD_AlphaCarotene_sd, prop_PD_BetaCarotene_mean	,
                                   prop_PD_BetaCarotene_sd	, prop_PD_Calcium_mean	, prop_PD_Calcium_sd	, prop_PD_Carbohydrate_mean	, prop_PD_Carbohydrate_sd	, prop_PD_Energy_mean	,
                                   prop_PD_Energy_sd	, prop_PD_Fat_mean	, prop_PD_Fat_sd	, prop_PD_FolateTotal_mean	, prop_PD_FolateTotal_sd	, prop_PD_Insolublefibre_mean	,
                                   prop_PD_Insolublefibre_sd	, prop_PD_Iron_mean	, prop_PD_Iron_sd	, prop_PD_NiacinB3_mean	, prop_PD_NiacinB3_sd	, prop_PD_Phytate_mean	,
                                   prop_PD_Phytate_sd	, prop_PD_Protein_mean	, prop_PD_Protein_sd	, prop_PD_Retinol_mean	, prop_PD_Retinol_sd	, prop_PD_RiboflavinB2_mean	,
                                   prop_PD_RiboflavinB2_sd	, prop_PD_Solublefibre_mean	, prop_PD_Solublefibre_sd	, prop_PD_ThiaminB1_mean	, prop_PD_ThiaminB1_sd	, prop_PD_TotalFibre_mean	,
                                   prop_PD_TotalFibre_sd	, prop_PD_VitaminB12_mean	, prop_PD_VitaminB12_sd	, prop_PD_VitaminB6pyridoxine_mean	, prop_PD_VitaminB6pyridoxine_sd	,
                                   prop_PD_VitaminC_mean	, prop_PD_VitaminC_sd	, prop_PD_VitaminE_mean	, prop_PD_VitaminE_sd	, prop_PD_VitARE_mean	, prop_PD_VitARE_sd	, prop_PD_Zinc_mean	, prop_PD_Zinc_sd)

#Transpose data frame
PD_summary_long <- transpose(PD_summary_groups_subset)

#redefine row and column names
rownames(PD_summary_long) <- colnames(PD_summary_groups_subset)
colnames(PD_summary_long) <- rownames(PD_summary_groups_subset)
column_names <- as.character(PD_summary_long[1, ])
colnames(PD_summary_long) <- column_names
PD_summary_long <- PD_summary_long[-1,]


#####Plot proportion pollinator dependence by nutrient

#Restructure data to enable plotting
PD_summary_long$metric <- sub(".*_(.*?)$", "\\1", rownames(PD_summary_long))
PD_summary_long$nutrient <- sub("^(.*?)_[^_]*$", "\\1", rownames(PD_summary_long))
PD_summary_long$nutrient <- sub("prop_poll_", "", PD_summary_long$nutrient)
rownames(PD_summary_long) <- NULL

df_long <- PD_summary_long %>%
  pivot_longer(cols = -c("metric", "nutrient"), names_to = "population_group", values_to = "value")
df_wide <- df_long %>%
  pivot_wider(names_from = metric, values_from = value)

df_wide <- df_wide %>%
  mutate(mean = as.numeric(mean),
         sd = as.numeric(sd))

df_wide$nutrient <- sub("prop_PD_", "", df_wide$nutrient)

#Define which nutrients to show in the plot
df_wide <- df_wide %>%
  filter((nutrient %in% c("Energy", "Fat", "Protein", "Calcium", "Iron", "Zinc", "VitaminC", "FolateTotal", "VitARE", "VitaminE")))


df_wide$mean_perc_PD <- df_wide$mean *100
df_wide$sd_perc_PD <- df_wide$sd * 100

# Order the nutrients by mean values in descending order
nutrient_PD_data <- df_wide %>%
  mutate(nutrient = fct_reorder(nutrient, -mean_perc_PD))

# Define the colors you want to use
custom_colors <- c("adol_fem" = "#F2C4C4", "adult_fem" = "#F29472", "adult_male" = "#5F9595", "u5_child" = "#F0BC68") # Example colors

custom_labels <- c("adol_fem" = "Adolescent girls",
                   "adult_fem" = "Adult women",
                   "adult_male" = "Adult men",
                   "u5_child" = "Children under-five")

# Create barplot
nutrient_PD_plot <- ggplot(nutrient_PD_data, aes(x = nutrient, y = mean_perc_PD, fill = population_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_perc_PD - sd_perc_PD, ymax = mean_perc_PD + sd_perc_PD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "",       y = "Percentage pollinator dependence (%)",       title = "", fill = "Population subgroup") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # Apply custom color scheme
  theme_minimal() +
  scale_x_discrete(labels = c("VitaminC" = "Vitamin C",
                              "FolateTotal" = "Folate",
                              "VitARE" = "Vitamin A",
                              "VitaminE" = "Vitamin E")) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1.2))

ggsave(plot=nutrient_PD_plot, filename="plots/nutrient_PD_incl_seed&imports.svg", width=8, height=5, dpi=600, bg="white")
ggsave(plot=nutrient_PD_plot, filename="plots/nutrient_PD_incl_seed&imports.png", width=8, height=5, dpi=600, bg="white")


######################################################
#Summarise TOTAL nutrients derived from each crop
######################################################

crop_summary <- diet_ingred_intake_data_PD_subset %>%   dplyr::group_by(local_imported,
                                                                        cultivated_wild,
                                                                        plant_animal_mineral,
                                                                        plant_barcode,	
                                                                        plant_category,	
                                                                        sci_name,	
                                                                        eng_name, 
                                                                        final_poll_dependence,	
                                                                        poll_depend_component) %>%  
  dplyr::summarise(total_grams_consumed = sum(ingredient_grams_consumed),
                   total_Energy_Kcal = sum(int_Energy_Kcal),
                   total_Fat_g = sum(int_Fat_g),
                   total_Protein_g = sum(int_Protein_g),
                   total_Carbohydrate_g  = sum(int_Carbohydrate_g ),
                   total_TotalFibre_g = sum(int_TotalFibre_g),
                   total_Insolublefibre_g = sum(int_Insolublefibre_g),
                   total_Solublefibre_g = sum(int_Solublefibre_g),
                   total_Calcium_mg = sum(int_Calcium_mg),
                   total_Iron_mg = sum(int_Iron_mg),
                   total_Zinc_mg = sum(int_Zinc_mg),
                   total_VitaminC_mg = sum(int_VitaminC_mg),
                   total_ThiaminB1_mg = sum(int_ThiaminB1_mg),
                   total_RiboflavinB2_mg = sum(int_RiboflavinB2_mg),
                   total_NiacinB3_mg = sum(int_NiacinB3_mg),
                   total_VitaminB6pyridoxine_mg = sum(int_VitaminB6pyridoxine_mg),
                   total_FolateTotal_µg = sum(int_FolateTotal_µg),
                   total_VitARE_µg = sum(int_VitARE_µg),
                   total_Retinol_µg = sum(int_Retinol_µg),
                   total_AlphaCarotene_µg = sum(int_AlphaCarotene_µg),
                   total_BetaCarotene_µg = sum(int_BetaCarotene_µg),
                   total_VitaminE_mg = sum(int_VitaminE_mg),
                   total_VitaminB12_µg = sum(int_VitaminB12_µg),
                   total_Phytate_mg = sum(int_Phytate_mg))
                                                                                                                     


#Calculate proportion of each nutrient provided by each crop
crop_summary$prop_grams <- crop_summary$total_grams_consumed / sum(crop_summary$total_grams_consumed)
crop_summary$prop_Energy <- crop_summary$total_Energy_Kcal / sum(crop_summary$total_Energy_Kcal)
crop_summary$prop_Fat <- crop_summary$total_Fat_g / sum(crop_summary$total_Fat_g)
crop_summary$prop_Protein <- crop_summary$total_Protein_g / sum(crop_summary$total_Protein_g)
crop_summary$prop_Carbohydrate <- crop_summary$total_Carbohydrate_g  / sum(crop_summary$total_Carbohydrate_g )
crop_summary$prop_TotalFibre <- crop_summary$total_TotalFibre_g / sum(crop_summary$total_TotalFibre_g)
crop_summary$prop_Insolublefibre <- crop_summary$total_Insolublefibre_g / sum(crop_summary$total_Insolublefibre_g)
crop_summary$prop_Solublefibre <- crop_summary$total_Solublefibre_g / sum(crop_summary$total_Solublefibre_g)
crop_summary$prop_Calcium <- crop_summary$total_Calcium_mg / sum(crop_summary$total_Calcium_mg)
crop_summary$prop_Iron <- crop_summary$total_Iron_mg / sum(crop_summary$total_Iron_mg)
crop_summary$prop_Zinc <- crop_summary$total_Zinc_mg / sum(crop_summary$total_Zinc_mg)
crop_summary$prop_VitaminC <- crop_summary$total_VitaminC_mg / sum(crop_summary$total_VitaminC_mg)
crop_summary$prop_ThiaminB1 <- crop_summary$total_ThiaminB1_mg / sum(crop_summary$total_ThiaminB1_mg)
crop_summary$prop_RiboflavinB2 <- crop_summary$total_RiboflavinB2_mg / sum(crop_summary$total_RiboflavinB2_mg)
crop_summary$prop_NiacinB3 <- crop_summary$total_NiacinB3_mg / sum(crop_summary$total_NiacinB3_mg)
crop_summary$prop_VitaminB6pyridoxine <- crop_summary$total_VitaminB6pyridoxine_mg / sum(crop_summary$total_VitaminB6pyridoxine_mg)
crop_summary$prop_FolateTotal <- crop_summary$total_FolateTotal_µg / sum(crop_summary$total_FolateTotal_µg)
crop_summary$prop_VitARE <- crop_summary$total_VitARE_µg / sum(crop_summary$total_VitARE_µg)
crop_summary$prop_Retinol <- crop_summary$total_Retinol_µg / sum(crop_summary$total_Retinol_µg)
crop_summary$prop_AlphaCarotene <- crop_summary$total_AlphaCarotene_µg / sum(crop_summary$total_AlphaCarotene_µg)
crop_summary$prop_BetaCarotene <- crop_summary$total_BetaCarotene_µg / sum(crop_summary$total_BetaCarotene_µg)
crop_summary$prop_VitaminE <- crop_summary$total_VitaminE_mg / sum(crop_summary$total_VitaminE_mg)
crop_summary$prop_VitaminB12 <- crop_summary$total_VitaminB12_µg / sum(crop_summary$total_VitaminB12_µg)
crop_summary$prop_Phytate <- crop_summary$total_Phytate_mg / sum(crop_summary$total_Phytate_mg)

#Change all values for poll-dependent component to 'none' when final poll dependence = 0
crop_summary$poll_depend_component[crop_summary$final_poll_dependence == 0] <- "none"

#Export crop dataset
write.csv(crop_summary, "output_data/Nutrients_by_crop.csv")

######################################################
#Summarise VILLAGE-LEVEL nutrients derived from each crop
######################################################

crop_summary_village <- diet_ingred_intake_data_PD_subset %>%   dplyr::group_by(village_code,
                                                                                local_imported,
                                                                                cultivated_wild,
                                                                                plant_animal_mineral,
                                                                                plant_barcode,	
                                                                                plant_category,	
                                                                                sci_name,	
                                                                                eng_name, 
                                                                                final_poll_dependence,	
                                                                                poll_depend_component) %>%  dplyr::summarise(total_grams_consumed = sum(ingredient_grams_consumed),
                                                                                                                             total_Energy_Kcal = sum(int_Energy_Kcal),
                                                                                                                             total_Fat_g = sum(int_Fat_g),
                                                                                                                             total_Protein_g = sum(int_Protein_g),
                                                                                                                             total_Carbohydrate_g  = sum(int_Carbohydrate_g ),
                                                                                                                             total_TotalFibre_g = sum(int_TotalFibre_g),
                                                                                                                             total_Insolublefibre_g = sum(int_Insolublefibre_g),
                                                                                                                             total_Solublefibre_g = sum(int_Solublefibre_g),
                                                                                                                             total_Calcium_mg = sum(int_Calcium_mg),
                                                                                                                             total_Iron_mg = sum(int_Iron_mg),
                                                                                                                             total_Zinc_mg = sum(int_Zinc_mg),
                                                                                                                             total_VitaminC_mg = sum(int_VitaminC_mg),
                                                                                                                             total_ThiaminB1_mg = sum(int_ThiaminB1_mg),
                                                                                                                             total_RiboflavinB2_mg = sum(int_RiboflavinB2_mg),
                                                                                                                             total_NiacinB3_mg = sum(int_NiacinB3_mg),
                                                                                                                             total_VitaminB6pyridoxine_mg = sum(int_VitaminB6pyridoxine_mg),
                                                                                                                             total_FolateTotal_µg = sum(int_FolateTotal_µg),
                                                                                                                             total_VitARE_µg = sum(int_VitARE_µg),
                                                                                                                             total_Retinol_µg = sum(int_Retinol_µg),
                                                                                                                             total_AlphaCarotene_µg = sum(int_AlphaCarotene_µg),
                                                                                                                             total_BetaCarotene_µg = sum(int_BetaCarotene_µg),
                                                                                                                             total_VitaminE_mg = sum(int_VitaminE_mg),
                                                                                                                             total_VitaminB12_µg = sum(int_VitaminB12_µg),
                                                                                                                             total_Phytate_mg = sum(int_Phytate_mg))


#Calculate proportion of each nutrient provided by each crop
crop_summary_village$prop_grams <- crop_summary_village$total_grams_consumed / sum(crop_summary_village$total_grams_consumed)
crop_summary_village$prop_Energy <- crop_summary_village$total_Energy_Kcal / sum(crop_summary_village$total_Energy_Kcal)
crop_summary_village$prop_Fat <- crop_summary_village$total_Fat_g / sum(crop_summary_village$total_Fat_g)
crop_summary_village$prop_Protein <- crop_summary_village$total_Protein_g / sum(crop_summary_village$total_Protein_g)
crop_summary_village$prop_Carbohydrate <- crop_summary_village$total_Carbohydrate_g  / sum(crop_summary_village$total_Carbohydrate_g )
crop_summary_village$prop_TotalFibre <- crop_summary_village$total_TotalFibre_g / sum(crop_summary_village$total_TotalFibre_g)
crop_summary_village$prop_Insolublefibre <- crop_summary_village$total_Insolublefibre_g / sum(crop_summary_village$total_Insolublefibre_g)
crop_summary_village$prop_Solublefibre <- crop_summary_village$total_Solublefibre_g / sum(crop_summary_village$total_Solublefibre_g)
crop_summary_village$prop_Calcium <- crop_summary_village$total_Calcium_mg / sum(crop_summary_village$total_Calcium_mg)
crop_summary_village$prop_Iron <- crop_summary_village$total_Iron_mg / sum(crop_summary_village$total_Iron_mg)
crop_summary_village$prop_Zinc <- crop_summary_village$total_Zinc_mg / sum(crop_summary_village$total_Zinc_mg)
crop_summary_village$prop_VitaminC <- crop_summary_village$total_VitaminC_mg / sum(crop_summary_village$total_VitaminC_mg)
crop_summary_village$prop_ThiaminB1 <- crop_summary_village$total_ThiaminB1_mg / sum(crop_summary_village$total_ThiaminB1_mg)
crop_summary_village$prop_RiboflavinB2 <- crop_summary_village$total_RiboflavinB2_mg / sum(crop_summary_village$total_RiboflavinB2_mg)
crop_summary_village$prop_NiacinB3 <- crop_summary_village$total_NiacinB3_mg / sum(crop_summary_village$total_NiacinB3_mg)
crop_summary_village$prop_VitaminB6pyridoxine <- crop_summary_village$total_VitaminB6pyridoxine_mg / sum(crop_summary_village$total_VitaminB6pyridoxine_mg)
crop_summary_village$prop_FolateTotal <- crop_summary_village$total_FolateTotal_µg / sum(crop_summary_village$total_FolateTotal_µg)
crop_summary_village$prop_VitARE <- crop_summary_village$total_VitARE_µg / sum(crop_summary_village$total_VitARE_µg)
crop_summary_village$prop_Retinol <- crop_summary_village$total_Retinol_µg / sum(crop_summary_village$total_Retinol_µg)
crop_summary_village$prop_AlphaCarotene <- crop_summary_village$total_AlphaCarotene_µg / sum(crop_summary_village$total_AlphaCarotene_µg)
crop_summary_village$prop_BetaCarotene <- crop_summary_village$total_BetaCarotene_µg / sum(crop_summary_village$total_BetaCarotene_µg)
crop_summary_village$prop_VitaminE <- crop_summary_village$total_VitaminE_mg / sum(crop_summary_village$total_VitaminE_mg)
crop_summary_village$prop_VitaminB12 <- crop_summary_village$total_VitaminB12_µg / sum(crop_summary_village$total_VitaminB12_µg)
crop_summary_village$prop_Phytate <- crop_summary_village$total_Phytate_mg / sum(crop_summary_village$total_Phytate_mg)

#Change all values for poll-dependent component to 'none' when final poll dependence = 0
crop_summary_village$poll_depend_component[crop_summary_village$final_poll_dependence == 0] <- "none"

#Export crop dataset
write.csv(crop_summary_village, "output_data/Nutrients_by_crop_by_village.csv")


#########################################################################################################
######## Calculate & plot the proportion of each nutrient provided by pollinator-dependent crops ########
#########################################################################################################

#Group crops which are pollinator dependent for their seed and food production  (i.e. only one category of PD)
PD_crops_summary <- crop_summary %>%
  dplyr::mutate(poll_depend_component = ifelse(poll_depend_component == 'seed', 'food', poll_depend_component))

PD_crops_summary_groups <- PD_crops_summary %>%
 dplyr::group_by(poll_depend_component) %>%  dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE))

#Subset to nutrients of interest
PD_crops_summary_groups_subset <- select(PD_crops_summary_groups, poll_depend_component,
                                         prop_grams, prop_Energy, prop_Fat, prop_Protein, prop_Calcium, prop_Iron,
                                         prop_Zinc, prop_VitaminC,prop_FolateTotal, prop_VitARE, prop_VitaminE)

#Convert to long format
PD_nutrients_long <- PD_crops_summary_groups_subset %>%
  pivot_longer(cols = -c(poll_depend_component),
               names_to = "nutrient",
               values_to = "value")

PD_nutrients_long <- PD_nutrients_long %>% filter(poll_depend_component != 'none')

PD_nutrients_long$nutrient <- gsub("prop_", "", PD_nutrients_long$nutrient)

PD_nutrients_long <- PD_nutrients_long %>%
  mutate(nutrient = case_when(
    nutrient == "FolateTotal" ~ "Folate",
    nutrient == "VitaminC" ~ "Vitamin C",
    nutrient == "VitARE" ~ "Vitamin A",
    nutrient == "VitaminE" ~ "Vitamin E",
    nutrient == "grams" ~ "Total grams",
    TRUE ~ nutrient  # Keep other values unchanged
  ))

PD_plot <- ggplot(PD_nutrients_long, aes(x = reorder(nutrient, -value), y = value)) +
  geom_bar(stat = "identity", fill = "#EE832C") +
  geom_text(aes(label = scales::percent(value, accuracy = 1)), hjust = 0.5, vjust = -0.5, size = 4) +  # Add this line
  labs(title = "(b)",
       y = "% from pollinator-dependent crops",
       x = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust=1, vjust = 1.2))+
  theme(axis.title.y = element_text(face = "bold", size = 12))+
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1))


#Combine the nutrient origin (import/local) plot with the pollinator dependence plot
nutrient_origin_and_PD_plot <- plot_grid(import_local_plot, PD_plot, ncol = 1, nrow = 2, rel_heights = c(1, 1))

ggsave(plot=nutrient_origin_and_PD_plot, filename="plots/Key_nutrients_source_&_poll_dependence.svg", width=5.5, height=8, dpi=500, bg="white")
ggsave(plot=nutrient_origin_and_PD_plot, filename="plots/Key_nutrients_source_&_poll_dependence.png", width=5.5, height=8, dpi=500, bg="white")


##########################################################################################
#######      Plot contribution of each crop to the intake of key nutrients         #######
##########################################################################################

# Define the colors you want to use
my_colors <- c("#ee7621ff", "#f5dc83ff",  "#8fa33fff") # Replace with your desired colors


###Total grams of consumption
#Filter to top 20 values
top_20_grams <- crop_summary %>%
  arrange(desc(prop_grams)) %>%
  head(20)

grams <- ggplot(top_20_grams, aes(x = reorder(eng_name, -prop_grams), y = prop_grams*100, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Percentage of diet by mass (%)",
       title = "Total consumption") +
  scale_fill_manual(values = my_colors) +
  ylim(0,40)+
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Remove the legend title

#ggsave(plot=grams, filename="C:/Users/tt15117/OneDrive - University of Bristol/Nepal Project/Data analysis/Plots/Nutrition plots/Grams_crop_contributions.svg", width=8, height=5, dpi=500, bg="white")
#ggsave(plot=grams, filename="C:/Users/tt15117/OneDrive - University of Bristol/Nepal Project/Data analysis/Plots/Nutrition plots/Grams_crop_contributions.png", width=8, height=5, dpi=500, bg="white")


###Vitamin A
#Filter to top 20 values
top_20_VitA <- crop_summary %>%
  arrange(desc(prop_VitARE)) %>%
  head(20)

VitA <- ggplot(top_20_VitA, aes(x = reorder(eng_name, -prop_VitARE), y = prop_VitARE, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Vitamin A") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  ylim(0,0.6)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5, 5, 5, 20))  # Adjust plot margins (top, right, bottom, left)))


###Folate
#Filter to top 20 values
top_20_Folate <- crop_summary %>%
  arrange(desc(prop_FolateTotal)) %>%
  head(20)

Folate <- ggplot(top_20_Folate, aes(x = reorder(eng_name, -prop_FolateTotal), y = prop_FolateTotal, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Folate") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###Calcium
#Filter to top 20 values
top_20_calcium <- crop_summary %>%
  arrange(desc(prop_Calcium)) %>%
  head(20)

Calcium <- ggplot(top_20_calcium, aes(x = reorder(eng_name, -prop_Calcium), y = prop_Calcium, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Calcium") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Vitamin C
#Filter to top 20 values
top_20_VitC <- crop_summary %>%
  arrange(desc(prop_VitaminC)) %>%
  head(20)

VitC <- ggplot(top_20_VitC, aes(x = reorder(eng_name, -prop_VitaminC), y = prop_VitaminC, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Vitamin C") +
  ylim(0,0.8)+
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Iron
#Filter to top 20 values
top_20_Iron <- crop_summary %>%
  arrange(desc(prop_Iron)) %>%
  head(20)

Iron <- ggplot(top_20_Iron, aes(x = reorder(eng_name, -prop_Iron), y = prop_Iron, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Iron") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5, 5, 5, 20))  # Adjust plot margins (top, right, bottom, left))))

###Zinc
#Filter to top 20 values
top_20_Zinc <- crop_summary %>%
  arrange(desc(prop_Zinc)) %>%
  head(20)

Zinc <- ggplot(top_20_Zinc, aes(x = reorder(eng_name, -prop_Zinc), y = prop_Zinc, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Zinc") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Vitamin E
#Filter to top 20 values
top_20_VitE <- crop_summary %>%
  arrange(desc(prop_VitaminE)) %>%
  head(20)

VitE <- ggplot(top_20_VitE, aes(x = reorder(eng_name, -prop_VitaminE), y = prop_VitaminE, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Vitamin E") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  ylim(0,0.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(5, 5, 5, 20))  # Adjust plot margins (top, right, bottom, left))))

###Energy
#Filter to top 20 values
top_20_Energy <- crop_summary %>%
  arrange(desc(prop_Energy)) %>%
  head(20)

Energy <- ggplot(top_20_Energy, aes(x = reorder(eng_name, -prop_Energy), y = prop_Energy, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Energy") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Protein
#Filter to top 20 values
top_20_Protein <- crop_summary %>%
  arrange(desc(prop_Protein)) %>%
  head(20)

Protein <- ggplot(top_20_Protein, aes(x = reorder(eng_name, -prop_Protein), y = prop_Protein, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Protein") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###TotalFibre
#Filter to top 20 values
top_20_VitaminB12 <- crop_summary %>%
  arrange(desc(prop_VitaminB12)) %>%
  head(20)

VitaminB12 <- ggplot(top_20_VitaminB12, aes(x = reorder(eng_name, -prop_VitaminB12), y = prop_VitaminB12, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Vitamin B12") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###NiacinB3
#Filter to top 20 values
top_20_NiacinB3 <- crop_summary %>%
  arrange(desc(prop_NiacinB3)) %>%
  head(20)

NiacinB3 <- ggplot(top_20_NiacinB3, aes(x = reorder(eng_name, -prop_NiacinB3), y = prop_NiacinB3, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Niacin B3") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Thiamin B1
#Filter to top 20 values
top_20_ThiaminB1 <- crop_summary %>%
  arrange(desc(prop_ThiaminB1)) %>%
  head(20)

ThiaminB1 <- ggplot(top_20_ThiaminB1, aes(x = reorder(eng_name, -prop_ThiaminB1), y = prop_ThiaminB1, fill=poll_depend_component)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(x = "",
       y = "Proportion of Nutrient",
       title = "Thiamin B1") +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5)+
  ylim(0,0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

key_nutrient_crops <- grid.arrange(VitA, Folate, VitE, Calcium, Iron, Zinc,   ncol = 2, nrow = 3)

ggsave(plot=key_nutrient_crops, filename="plots/Key_nutrients_by_crops.svg", width=8, height=12, dpi=500, bg="white")
ggsave(plot=key_nutrient_crops, filename="plots/Key_nutrients_by_crops.png", width=8, height=12, dpi=500, bg="white")


other_nutrient_crops <- grid.arrange(Energy, Protein, VitaminB12, VitC, NiacinB3, ThiaminB1,   ncol = 2, nrow = 3)

ggsave(plot=other_nutrient_crops, filename="plots/Other_nutrients_by_crops.svg", width=10, height=14, dpi=500, bg="white")
ggsave(plot=other_nutrient_crops, filename="plots/Other_nutrients_by_crops.png", width=10, height=14, dpi=500, bg="white")


##############################################################################################################################################################################
#########################################################          END OF SCRIPT 01    #######################################################################################
##############################################################################################################################################################################
