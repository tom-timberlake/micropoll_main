##################################################################################################################################################################
##############   MICRO-POLL SCRIPT 2 - LINKING CROPS, POLLINATORS & MICRONUTRIENTS TO CALCULATE & PLOT CONTRIBUTIONS  ############################################
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
library(bipartite)
library(ggplot2)
library(ggalluvial)
library(networkD3)
library(webshot2)


##Import crop nutrient data (file generated in Script_01 and saved in "output_data")
crop_summary <- read.csv("output_data/Nutrients_by_crop.csv") 

#Import plant-pollinator interaction data from main "input_data" folder
plant_poll_data <- data.table(read_excel("input_data/MP_pollinator_visitation.xlsx", 
                                         sheet = "Visitation data"))

##Import pollen carrying capacity data (number of pollen grains transported by each insect taxa) from main "input_data" folder
pollen_data <- read.csv("input_data/pollen_capacity_OTU.csv") 


##################################################################################################################################################
################################   Calculating crop, pollinator and wild plant contribution scores    ############################################
##################################################################################################################################################

#######################################################################
#######           Organising crop - nutrient data           #######
#######################################################################

#Subset crop-nutrient data to columns and nutrients of interest
crop_summary_edits <- select(crop_summary, 
                             cultivated_wild	,
                             plant_animal_mineral	,
                             plant_barcode	,
                             plant_category	,
                             sci_name	,
                             eng_name	,
                             final_poll_dependence	,
                             poll_depend_component	,
                             #prop_Energy,
                             #prop_Fat,
                             #prop_Protein,
                             prop_Calcium,
                             prop_Iron,
                             #prop_Zinc,
                             prop_VitaminC,
                             #prop_ThiaminB1,
                             #prop_RiboflavinB2,
                             #prop_NiacinB3,
                             #prop_VitaminB6pyridoxine,
                             prop_FolateTotal,
                             prop_VitARE,
                             prop_VitaminE,
                             #prop_VitaminB12
)

#Rename columns to remove prop_ from title
names(crop_summary_edits) <- gsub("prop_", "", names(crop_summary_edits))

#Convert from wide to long format
crop_summary_long <- crop_summary_edits %>%
  pivot_longer(cols = c(Calcium, Iron, VitaminC, FolateTotal, VitARE, VitaminE), # columns to be gathered
               names_to = "nutrient", # name of the new key column
               values_to = "prop_contribution")    # name of the new value column

#Create table of food item contributions to each key nutrient
crop_contribution_summary <- crop_summary_long %>%  dplyr::group_by(nutrient, sci_name, eng_name) %>%
  dplyr::summarise(prop_contribution = sum(prop_contribution))

# Remove rows where crop_contribution is 0
crop_contribution_summary <- crop_contribution_summary %>%  filter(prop_contribution != 0)  

#Create a table showing top crops for each nutrient (i.e. convert to wide format)
crop_contribution_wide <- crop_contribution_summary %>%
  pivot_wider(names_from = nutrient,  # The column that will become the new column names
              values_from = prop_contribution,  # The column that will fill the new column values
              values_fill = list(prop_contribution = 0))  # Fill missing values with 0 (or NA if preferred)

#Calculate mean contribution across all 6 nutrients
crop_contribution_wide <- crop_contribution_wide %>%  
  mutate(mean_contribution = (Calcium + FolateTotal + Iron + VitARE + VitaminC + VitaminE)/6)

crop_contribution_wide <- crop_contribution_wide %>%  arrange(desc(mean_contribution))


#######################################################################
#######           Organising poll - crop data           #######
#######################################################################

plant_poll_data_edits <- plant_poll_data

#Remove rows with NA for plant or pollinator
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(insect_OTU)
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(plant_sci_name)

#Summarise plant-pol interactions - calculate the number of visits made to each plant
plant_poll_summary <- plant_poll_data %>%  dplyr::group_by(plant_sci_name, plant_category, insect_OTU) %>%  dplyr::summarise(count = n()) %>%  ungroup()

#Merge in pollen data
pollen_transport_summary <- merge(x=plant_poll_summary, y=pollen_data, by="insect_OTU",all.x=TRUE)

#Multiply visitation frequency by pollen transport capacity
pollen_transport_summary$total_pollen_trans <- pollen_transport_summary$count * pollen_transport_summary$mean_pollen_load

#Remove NA values
pollen_transport_summary <- pollen_transport_summary %>% drop_na(total_pollen_trans)

#Calculate proportion of total pollen transported by each insect
pollen_proportion_summary <- pollen_transport_summary %>%  dplyr::group_by(plant_sci_name) %>%  mutate(proportion_pollen = total_pollen_trans / sum(total_pollen_trans))

#Change name of column header to match with crop-nutrient data
colnames(pollen_proportion_summary)[colnames(pollen_proportion_summary) == "plant_sci_name"] <- "sci_name"

#######################################################################
#######  Merge poll-importance data with crop-nutrient data     #######
#######################################################################

#Merge visitation data into crop-nutrient dataset
poll_crop_nutrient_data <- merge(x=crop_summary_long, y=pollen_proportion_summary, by="sci_name",all.x=TRUE)
poll_crop_nutrient_data[is.na(poll_crop_nutrient_data)] <- 0

# Identify rows where 'final_poll_dependence' is equal to 0 and change all insect values to zero
rows_to_zero <- poll_crop_nutrient_data$final_poll_dependence == 0
poll_crop_nutrient_data[rows_to_zero, "proportion_pollen"] <- 0

#Create new variable which calculates the proportional contribution of each insect pollinator to each nutrient (i.e. the Pollinator Contribution, PC score)
# Formula for this metric = Proportion pollen transport * Proportional contribution to nutrient * Pollinator dependence of crop
poll_crop_nutrient_data_calcs <- poll_crop_nutrient_data %>%  
  mutate(poll_contribution = prop_contribution * proportion_pollen * final_poll_dependence)

#Summarise poll_contribution score for each insect
poll_contribution_summary <- poll_crop_nutrient_data_calcs %>%  dplyr::group_by(nutrient, insect_OTU) %>%
  dplyr::summarise(poll_contribution = sum(poll_contribution))

# Remove rows where poll_contribution is 0
poll_contribution_summary <- poll_contribution_summary %>%  filter(poll_contribution != 0)  

#Create a table showing top insects for each nutrient (i.e. convert to wide format)
poll_contribution_wide <- poll_contribution_summary %>%
  pivot_wider(names_from = nutrient,  # The column that will become the new column names
              values_from = poll_contribution,  # The column that will fill the new column values
              values_fill = list(poll_contribution = 0))  # Fill missing values with 0 (or NA if preferred)

names (poll_contribution_wide)
#Calculate mean contribution across all 6 nutrients
poll_contribution_wide <- poll_contribution_wide %>%  
  mutate(mean_contribution = (Calcium + FolateTotal + Iron + VitARE + VitaminC + VitaminE)/6)

poll_contribution_wide <- poll_contribution_wide %>%  arrange(desc(mean_contribution))

##################################################################################################
### Calculate the indirect contribution to the pollination service of each wild plant 
##################################################################################################

#Start by removing all crop plant data from the plant-poll dataset as we are only interested in non-crop resources
plant_interactions_wild <- plant_poll_summary %>%  filter(plant_category != 'crop')

#Calculate the total visits made by each insect to ALL wild plants
insect_visits_all <- plant_interactions_wild %>%   dplyr::group_by(insect_OTU) %>%  dplyr::summarise(total_insect_visits= sum(count, na.rm=TRUE)) 

#Calculate the number of visits made by each insect to EACH wild plant
insect_visits_by_plant <- plant_interactions_wild %>%   dplyr::group_by(insect_OTU, plant_sci_name ) %>%  dplyr::summarise(plant_insect_visits= sum(count, na.rm=TRUE)) 

# Merge the full dataset with the total visit data so that individual plant-poll visits can be calculated as a proportion of all visits made by the insect
plant_resource_use_merge <- merge(x=insect_visits_by_plant, y=insect_visits_all, by="insect_OTU",all.x=TRUE)

#Divide each interaction frequency value by the total number number of visits performed by the insect
plant_resource_use_merge$prop_plant_use <- plant_resource_use_merge$plant_insect_visits / plant_resource_use_merge$total_insect_visits 

#Change name of column header to match with crop-nutrient data
colnames(plant_resource_use_merge)[colnames(plant_resource_use_merge) == "plant_sci_name"] <- "plant_resource_name"

#Merge scores for each plant-insect combination with crop-poll data so that total indirect contribution scores can be calculated
poll_contribution_plant_indirect <- merge(x=poll_contribution_summary, y=plant_resource_use_merge, by="insect_OTU",all.x=TRUE) 

#Create new variable which calculates the indirect contribution of each plant resource to each nutrient (i.e. the Indirect Contribution, IC score)
# Formula for this metric = Pollinator contribution * Proportional plant use
poll_contribution_plant_indirect <- poll_contribution_plant_indirect %>%  
  mutate(plant_IC_score = poll_contribution * prop_plant_use)

#Ungroup the data
poll_contribution_plant_indirect <- poll_contribution_plant_indirect %>%  ungroup()

#Summarise plant_IC_score for each plant-nutrient combination
plant_contribution_summary <- poll_contribution_plant_indirect %>%  dplyr::group_by(nutrient, plant_resource_name) %>%
  dplyr::summarise(plant_IC_score = sum(plant_IC_score))

# Remove rows where poll_contribution is 0
plant_contribution_summary <- plant_contribution_summary %>%  filter(plant_IC_score != 0)  

#Create a table showing top plants for each nutrient (i.e. convert to wide format)
plant_contribution_wide <- plant_contribution_summary %>%
  pivot_wider(names_from = nutrient,  # The column that will become the new column names
              values_from = plant_IC_score,  # The column that will fill the new column values
              values_fill = list(plant_IC_score = 0))  # Fill missing values with 0 (or NA if preferred)

#Calculate mean contribution across all 6 nutrients
plant_contribution_wide <- plant_contribution_wide %>%  
  mutate(mean_contribution = (Calcium + FolateTotal + Iron + VitARE + VitaminC + VitaminE)/6)

plant_contribution_wide <- plant_contribution_wide %>%  arrange(desc(mean_contribution))

#####################################################################################################
####### Printing out results of importance analysis for crops, pollinators and wild plants   ########
#####################################################################################################

#----Export importance summaries#
wb <- createWorkbook()
addWorksheet(wb, sheet = "crop_contribution", gridLines = TRUE)
addWorksheet(wb, sheet = "poll_contribution", gridLines = TRUE)
addWorksheet(wb, sheet = "plant_contribution", gridLines = TRUE)
writeData(wb, sheet = "crop_contribution", x = crop_contribution_wide, rowNames = FALSE)
writeData(wb, sheet = "poll_contribution", x = poll_contribution_wide, rowNames = FALSE)
writeData(wb, sheet = "plant_contribution", x = plant_contribution_wide, rowNames = FALSE)
freezePane(wb, sheet = "crop_contribution", firstRow = TRUE)
freezePane(wb, sheet = "poll_contribution", firstRow = TRUE)
freezePane(wb, sheet = "plant_contribution", firstRow = TRUE)
saveWorkbook(wb, paste0("output_data/Nutrient_contributions_crop_poll_plant.xlsx"), overwrite = TRUE)


##################################################################################################################################################
############################################     Organising data and plotting the Sankey diagram     ############################################
##################################################################################################################################################


#########################################################
#######     organise the nutrient - crop data     #######
#########################################################

#Subset to relevant columns and choose the nutrients to plot
crop_summary_subset <- select(crop_summary, 
                              cultivated_wild	,
                              plant_animal_mineral	,
                              plant_barcode	,
                              plant_category	,
                              sci_name	,
                              eng_name	,
                              final_poll_dependence	,
                              poll_depend_component	,
                              #prop_Energy,
                              #prop_Fat,
                              #prop_Protein,
                              prop_Calcium,
                              prop_Iron,
                              #prop_Zinc,
                              prop_VitaminC,
                              #prop_ThiaminB1,
                              #prop_RiboflavinB2,
                              #prop_NiacinB3,
                              #prop_VitaminB6pyridoxine,
                              prop_FolateTotal,
                              prop_VitARE,
                              prop_VitaminE,
                              #prop_VitaminB12
)
#str(crop_summary_subset)

#Rename columns
names(crop_summary_subset) <- gsub("prop_", "", names(crop_summary_subset))

#Cluster non-pollinator-dependent foods into broader categories (e.g. 'animal products' etc.)
crop_summary_clusters <- crop_summary_subset %>%
  mutate(sci_name = ifelse(final_poll_dependence == "0", "Non-PD crops", sci_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(final_poll_dependence == "0", "Non-PD crops", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(sci_name = ifelse(plant_animal_mineral == "A", "Animal products", sci_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(plant_animal_mineral == "A", "Animal products", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(sci_name == "Mixed vegetable oil", "Mixed vegetable oil", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(sci_name == "Mixed wild berries", "Mixed wild berries", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(sci_name == "Mixed wild fruit", "Mixed wild fruit", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(sci_name = ifelse(plant_barcode == "0", "other", sci_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(eng_name = ifelse(plant_barcode == "0", "other", eng_name))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(poll_depend_component = ifelse(poll_depend_component == "0", "none", poll_depend_component))

crop_summary_clusters <- crop_summary_clusters %>%
  mutate(poll_depend_component = ifelse(eng_name == "Non-PD crop", "none", poll_depend_component))

#Remove random foods with no nutrient contribution
crop_summary_clusters <- subset(crop_summary_clusters, final_poll_dependence != 1 & plant_barcode != "0" & plant_barcode != "Wild food")

#Summarise all non-PD foods
crop_summary_clusters <- subset(crop_summary_clusters, select = -c(cultivated_wild, plant_animal_mineral, plant_barcode, plant_category))
summary_crop_nutrients <- crop_summary_clusters %>%  dplyr::group_by(sci_name, eng_name, final_poll_dependence, poll_depend_component) %>%  summarize_all(sum)

names(summary_crop_nutrients)

#Calculate sum of all nutrient contributions - CHANGE COLUMN NUMBERS WHERE RELEVANT
summary_crop_nutrients$nutrient_sum <- rowSums(summary_crop_nutrients[, 5:10, drop = FALSE])

#Remove crops which have a nutrient sum below 0.001
summary_crop_nutrients_subset <- summary_crop_nutrients[summary_crop_nutrients$nutrient_sum >= 0.01, ]

#Remove the column 'nutrient_sum'
summary_crop_nutrients_subset <- select(summary_crop_nutrients_subset, -nutrient_sum)

##Convert to long format data
crops_nutrients_long <- summary_crop_nutrients_subset %>%
  pivot_longer(cols = -c(sci_name, eng_name, final_poll_dependence, poll_depend_component),
               names_to = "nutrient",
               values_to = "crop_value")

#Rename nutrients, as required
crops_nutrients_long <- crops_nutrients_long %>%
  mutate(nutrient = case_when(
    nutrient == "VitaminC" ~ "Vitamin C",
    nutrient == "FolateTotal" ~ "Folate",
    nutrient == "VitARE" ~ "Vitamin A",
    nutrient == "VitaminE" ~ "Vitamin E",
    TRUE ~ nutrient))  # Keep the original value if it doesn't match any condition

#Rename crops, as required
crops_nutrients_long <- crops_nutrients_long %>%
  mutate(eng_name = case_when(
    eng_name == "Soybean" ~ "Imported soybean",
    eng_name == "Broad leaf mustard" ~ "Mustard",
    TRUE ~ eng_name))  # Keep the original value if it doesn't match any condition

#################################################
### Plotting nutrient-crop network
#################################################

# This plots the crop-nutrient links with all nutrients equally weighted
nutr_crop_poll_web <- ggplot(data = crops_nutrients_long,
                             aes(axis1 = nutrient, 
                                 axis2 = reorder(eng_name, -crop_value), 
                                 y = crop_value)) +
  geom_alluvium(aes(fill = nutrient), curve_type = "cubic") +
  geom_stratum(fill = "white")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Nutrient", "Crop"), expand = c(0.2, 0.1)) +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_fill_viridis_d()

nutr_crop_poll_web

#################################################
### Organise the crop-poll data
#################################################

#Remove rows with NA for plant or pollinator
plant_poll_data <- plant_poll_data %>% drop_na(insect_OTU)
plant_poll_data <- plant_poll_data %>% drop_na(plant_sci_name)

#names(plant_poll_data)
#Summarise plant-pol interactions
plant_poll_summary <- plant_poll_data %>%  dplyr::group_by(plant_sci_name, plant_barcode, plant_category, insect_OTU) %>%  dplyr::summarise(count = n()) %>%  ungroup()

#Merge in pollen data
pollen_transport_summary <- merge(x=plant_poll_summary, y=pollen_data, by="insect_OTU",all.x=TRUE)

#Multiply visitation frequency by pollen transport capacity
pollen_transport_summary$total_pollen_trans <- pollen_transport_summary$count * pollen_transport_summary$mean_pollen_load

#Remove NA values
pollen_transport_summary <- pollen_transport_summary %>% drop_na(total_pollen_trans)

#Drop interactions with only one recorded interaction (i.e. singletons)
pollen_transport_summary <- subset(pollen_transport_summary, count != 1 & count != 0)

#Calculate proportion of total pollen_transport_summary pollen transported by each insect
pollen_proportion_summary <- pollen_transport_summary %>%  dplyr::group_by(plant_sci_name, plant_barcode) %>%  dplyr::mutate(proportion_pollen = total_pollen_trans / sum(total_pollen_trans))

colnames(pollen_proportion_summary)[colnames(pollen_proportion_summary) == "plant_sci_name"] <- "sci_name"

#Remove interactions which have a poll_value below 0.05 as these are unimportant
pollen_proportion_subset <- pollen_proportion_summary[pollen_proportion_summary$proportion_pollen >= 0.005, ]

##################################################################################################
### Calculate use of each wild plant by insects 
##################################################################################################

#Remove all values for crop plants as we are only interested in non-crop resources
pollen_proportion_wild <- pollen_proportion_subset %>%  filter(plant_category != 'crop')

plant_resource_use_insects <- pollen_proportion_wild %>%   dplyr::group_by(insect_OTU) %>%  dplyr::summarise(total_insect_visits= sum(count, na.rm=TRUE)) #Calculate the total visits made by each insect to all wild plants
plant_resource_use_all <- pollen_proportion_wild %>%   dplyr::group_by(insect_OTU, sci_name) %>%  dplyr::summarise(plant_insect_visits= sum(count, na.rm=TRUE)) #Calculate the total visits made by each insect to all wild plants

plant_resource_use_merge <- merge(x=plant_resource_use_all, y=plant_resource_use_insects, by="insect_OTU",all.x=TRUE) # Merge the full dataset with the total visit data so that individual plant-poll visits can be calculated as a proportion of all visits made by the insect
plant_resource_use_merge$prop_plant_use <- plant_resource_use_merge$plant_insect_visits / plant_resource_use_merge$total_insect_visits #Divide each interaction frequency value by the total number number of visits performed by the insect

#Subset data to only insect_OTU, plant names and resource importance scores
plant_resource_use_subset <- select(plant_resource_use_merge, insect_OTU, sci_name, prop_plant_use)

#####################################################################################
#######     Merge & organise the plant-crop and nutrient-crop datasets     #######
#####################################################################################

#Merge visitation data into crop-nutrient dataset
poll_crop_nutrient_data <- merge(x=crops_nutrients_long, y=pollen_proportion_subset, by="sci_name",all.x=TRUE)
poll_crop_nutrient_data[is.na(poll_crop_nutrient_data)] <- 0

# Identify rows where 'final_poll_dependence' is equal to 0 and change all insect values to zero
rows_to_zero <- poll_crop_nutrient_data$final_poll_dependence == 0

# Set values in insect columns to zero for the identified rows
poll_crop_nutrient_data[rows_to_zero, "proportion_pollen"] <- 0

#Subset to necessary columns
poll_crop_nutrient_subset <- select(poll_crop_nutrient_data, nutrient, eng_name, crop_value, insect_OTU, proportion_pollen)

#Change all zero values in insect_OTU to NA
poll_crop_nutrient_subset <- poll_crop_nutrient_subset %>%
  mutate(insect_OTU = ifelse(insect_OTU == "0", NA, insect_OTU))

#Create new column called poll_value which multiplies crop value by the proportion of pollen transported by the insect
poll_crop_nutrient_subset <- poll_crop_nutrient_subset %>%
  mutate(poll_value = crop_value * proportion_pollen)

#Merge in plant resource importance information to add a new layer to the network
poll_crop_nutrient_plants <- merge(x=poll_crop_nutrient_subset, y=plant_resource_use_subset, by="insect_OTU",all.x=TRUE) 

#Create new column called plant_value which multiplies pollinator value by the proportion of visits made to each wild plant
poll_crop_nutrient_plants <- poll_crop_nutrient_plants %>%  mutate(plant_value = poll_value * prop_plant_use)

#Calculate total plant_value score for each plant so low values can be removed
poll_plant_data <- select(poll_crop_nutrient_plants, insect_OTU, sci_name, prop_plant_use, plant_value)
poll_plant_sums <- poll_plant_data %>%   dplyr::group_by(sci_name) %>%  dplyr::summarise(total_plant_value= sum(plant_value, na.rm=TRUE))
poll_plant_data <- merge(x=poll_plant_data, y=poll_plant_sums, by="sci_name",all.x=TRUE)

#Remove all plant species with a total value less than 0.2 (this seems to be a useful cut-off beyond which plants are fairly insignificant)
filtered_poll_plant_data <- poll_plant_data %>%  filter(total_plant_value >= 0.02)

#Slim dataset back down to insect-plant interactions and the value of each plant (minus all the less valuable plants)
poll_plant_data_final <- select(filtered_poll_plant_data, insect_OTU, sci_name, prop_plant_use)
poll_plant_data_final <- distinct(poll_plant_data_final) #Remove duplicate rows

#Re-merge the slimmed-down plant list in to the crop-poll-nutrient data and re-calculate plant values
poll_crop_nutrient_plants_slimmed <- merge(x=poll_crop_nutrient_subset, y=poll_plant_data_final, by="insect_OTU",all.x=TRUE) 

#Create new column called plant_value which multiplies pollinator value by the proportion of visits made to each wild plant
poll_crop_nutrient_plants_slimmed <- poll_crop_nutrient_plants_slimmed %>%  mutate(plant_value = poll_value * prop_plant_use)

#Removes underscore from insect names
poll_crop_nutrient_plants_slimmed$insect_OTU <- gsub("_", " ", poll_crop_nutrient_plants_slimmed$insect_OTU) 

#Removes plant barcode column and prop_plant_use
poll_crop_nutrient_plants_subset <- select(poll_crop_nutrient_plants_slimmed, -prop_plant_use, -proportion_pollen)

#Re-scale some of the value scores so that the size of bars in each set of nodes are roughly equal
poll_crop_nutrient_plants_subset$plant_value <- poll_crop_nutrient_plants_subset$plant_value * 1.5

#Change all NA values in the text columns to 'unknown' and all NA values in the numeric columns to 0 - without this, the Sankey diagram misses out links
poll_crop_nutrient_plants_final <- poll_crop_nutrient_plants_subset %>% mutate(sci_name = ifelse(is.na(sci_name), "Unknown", sci_name),
                                                                               plant_value = ifelse(is.na(plant_value), "0.001", plant_value))


#####################################################################################
#######       Re-shape the data to enable plotting as Sankey diagram       #######
#####################################################################################

# Create a new column 'nutrient_id' with unique integer values for each unique nutrient
poll_crop_nutrient_sankey <- poll_crop_nutrient_plants_final %>%
  mutate(nutrient_id = as.integer(factor(nutrient, levels = unique(nutrient))))
poll_crop_nutrient_sankey$nutrient_id <- as.numeric(poll_crop_nutrient_sankey$nutrient_id)

# Find the maximum value in the 'nutrient_id' column
max_nutrient_id <- max(poll_crop_nutrient_sankey$nutrient_id, na.rm = TRUE)

# Create a new column 'crop_id' with unique integer values for each unique eng_name
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(crop_id = as.integer(factor(eng_name, levels = unique(eng_name))))
poll_crop_nutrient_sankey$crop_id <- as.numeric(poll_crop_nutrient_sankey$crop_id)

#Add the maximum value for nutrient_id to crop_id so that there is no duplication in numbers
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(crop_id = crop_id + max_nutrient_id)

# Find the maximum value in the 'crop_id' column
max_crop_id <- max(poll_crop_nutrient_sankey$crop_id, na.rm = TRUE)

# Create a new column 'poll_id' with unique integer values for each unique insect_OTU
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(poll_id = as.integer(factor(insect_OTU, levels = unique(insect_OTU))))
poll_crop_nutrient_sankey$poll_id <- as.numeric(poll_crop_nutrient_sankey$poll_id)

#Add the maximum value for crop_id to poll_id so that there is no duplication in numbers
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(poll_id = poll_id + max_crop_id)

# Find the maximum value in the 'poll_id' column
max_poll_id <- max(poll_crop_nutrient_sankey$poll_id, na.rm = TRUE)

# Create a new column 'plant_id' with unique integer values for each unique plant
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(plant_id = as.integer(factor(sci_name, levels = unique(sci_name))))
poll_crop_nutrient_sankey$plant_id <- as.numeric(poll_crop_nutrient_sankey$plant_id)

#Add the maximum value for poll_id to plant_id so that there is no duplication in numbers
poll_crop_nutrient_sankey <- poll_crop_nutrient_sankey %>%
  mutate(plant_id = plant_id + max_poll_id)


###########################################
## Create node dataframe for Sankey diagram
###########################################

#Separate out all of the columns with node names+numbers and bind together into one long list of node names
nutrient_nodes <- select(poll_crop_nutrient_sankey, nutrient_id, nutrient)
nutrient_nodes <- nutrient_nodes %>%  rename_at(vars(1, 2), ~ c("node", "name"))
nutrient_nodes$group <- nutrient_nodes$name # Create a new column called group which will be used to colour the nodes

crop_nodes <- select(poll_crop_nutrient_sankey, crop_id, eng_name)
crop_nodes <- crop_nodes %>%  rename_at(vars(1, 2), ~ c("node", "name"))
crop_nodes$group <- "PD" # Create a new column called group which will be used to colour the nodes

poll_nodes <- select(poll_crop_nutrient_sankey, poll_id, insect_OTU)
poll_nodes <- poll_nodes %>%  rename_at(vars(1, 2), ~ c("node", "name"))
poll_nodes$group <- "" # Create a new column called group which will be used to colour the nodes

plant_nodes <- select(poll_crop_nutrient_sankey, plant_id, sci_name)
plant_nodes <- plant_nodes %>%  rename_at(vars(1, 2), ~ c("node", "name"))
plant_nodes$group <- "" # Create a new column called group which will be used to colour the nodes

#Join all four node datasets
all_nodes <- rbind(nutrient_nodes, crop_nodes, poll_nodes, plant_nodes)

#Remove duplicated rows
all_nodes <- distinct(all_nodes)

#Remove NA values
all_nodes <- na.omit(all_nodes)

#Minus 1 from each node ID so that they are zero-indexed (this is required for Java Script)
all_nodes$node <- all_nodes$node - 1

#Select which nodes to label
labelled_nodes <- c("Zinc", "Iron", "Vitamin C", "Calcium", "Folate", "Vitamin A", "Vitamin E",#Nutrients to label
                    "Non-PD crops", "Jumli bean", "Mustard", "Imported soybean", "Fortified food", "Animal products", #Crops to label
                    "Apis cerana","Eristalis tenax","Apis laboriosa","Bombus tunicatus", "Bombus",  "Andrena sp 01", #Insects to label
                    "Persicaria nepalensis", "Tagetes erecta", "Cotoneaster microphyllus", "Rosa sericea") #Plants to label

# Remove all labels which don't match the names above
all_nodes$name[!(all_nodes$name %in% labelled_nodes)] <- NA

#Change grouping name for non-PD crops
all_nodes <- all_nodes %>%
  mutate(group = case_when(
    name == "Animal products" ~ "animal",
    name == "Fortified food" ~ "fortified",
    name == "Non-PD crops" ~ "Non_PD",
    TRUE ~ group  ))

###########################################
## Create link dataframe for Sankey diagram
###########################################

#Separate out all of the columns with node id + link weight and bind together into one long list of node IDs and link weights
crop_nutrient_links <- select(poll_crop_nutrient_sankey, nutrient, nutrient_id, eng_name, crop_id, crop_value)
crop_nutrient_links <- crop_nutrient_links %>%  rename_at(vars(1, 2, 3, 4, 5), ~ c("target_name", "target", "source_name", "source",  "value"))
crop_nutrient_links$value <- crop_nutrient_links$value * 1.5 #Up-scale the crop links to keep all levels of nodes in similar scaling
crop_nutrient_links$group <- "" #Add grouping variable which will be used to define the colours for the links between crops and nutrients

crop_poll_links <- select(poll_crop_nutrient_sankey, eng_name, crop_id, insect_OTU, poll_id, poll_value)
crop_poll_links <- crop_poll_links %>%  rename_at(vars(1, 2, 3, 4, 5), ~ c("target_name", "target", "source_name", "source",  "value"))
crop_poll_links$group <- "" 

poll_plant_links <- select(poll_crop_nutrient_sankey, insect_OTU, poll_id, sci_name, plant_id, plant_value)
poll_plant_links <- poll_plant_links %>%  rename_at(vars(1, 2, 3, 4, 5), ~ c("target_name", "target", "source_name", "source",  "value"))
poll_plant_links$group <- "" 


#Join the link datasets
all_links <- rbind(crop_nutrient_links, crop_poll_links, poll_plant_links)

#Remove duplicated rows
all_links <- distinct(all_links)

#Remove NA values
all_links <- na.omit(all_links)

#Minus 1 from each node ID so that they are zero-indexed (this is required for Java Script)
all_links$source <- all_links$source - 1
all_links$target <- all_links$target - 1

###########################################
## Prepare colour scheme
###########################################

# Assign grouping variables based on different factors that we want to colour

# Define the groupings for each link - in this case we are just changing the current 'PD' grouping variable to 'non-PD' for the few groups of crop which aren't pollinator dependent

#all_links <- all_links %>% #THIS CODE IS USED IF WE WANT TO COLOUR LINKS BASED ON SOURCE CROPS, RATHER THAN TAGET NUTRIENTS
#mutate(group = case_when(
#source_name == "Non-PD crops" ~ "Non_PD",
#source_name == "Fortified food" ~ "fortified",
#source_name == "Animal products" ~ "animal",
#TRUE ~ group  ))

all_links <- all_links %>% #THIS CODE IS USED IF WE WANT TO COLOUR LINKS BASED ON TARGET NUTRIENTS, RATHER THAN SOURCE CROPS
  mutate(group = case_when(
    target_name == "Vitamin C" ~ "Vitamin_C",
    target_name == "Zinc" ~ "Zinc",
    target_name == "Vitamin A" ~ "Vitamin_A",
    target_name == "Vitamin E" ~ "Vitamin_E",
    target_name == "Calcium" ~ "Calcium",
    target_name == "Iron" ~ "Iron",
    target_name == "Folate" ~ "Folate",
    TRUE ~ "None"  ))

# Define the nodes we want to colour
all_nodes <- all_nodes %>%
  mutate(group = case_when(
    name == "Vitamin C" ~ "Vitamin_C",
    name == "Zinc" ~ "Zinc",
    name == "Vitamin A" ~ "Vitamin_A",
    name == "Vitamin E" ~ "Vitamin_E",
    name == "Calcium" ~ "Calcium",
    name == "Iron" ~ "Iron",
    name == "Folate" ~ "Folate",
    name == "Apis cerana" ~ "key_poll",
    name == "Eristalis tenax" ~ "key_poll",
    name == "Andrena sp 01" ~ "key_poll",
    name == "Apis laboriosa" ~ "key_poll",
    name == "Bombus" ~ "key_poll",
    name == "Bombus tunicatus" ~ "key_poll",
    name == "Persicaria nepalensis" ~ "key_plant",
    name == "Tagetes erecta" ~ "key_plant",
    name == "Cotoneaster microphyllus" ~ "key_plant",
    name == "Rosa sericea" ~ "key_plant",
    TRUE ~ group  ))

my_colours<-'d3.scaleOrdinal() .domain(["Iron", "Vitamin_C", "Calcium", "Folate", "Vitamin_A", "Vitamin_E", "None",
"PD", "animal", "fortified", "Non_PD",
"key_poll",
"key_plant",
]) .range(["#9EC5C6",  "#CAE0AC", "#D2EDFE", "#f4c623", "#F39999", "#2f7ab9", "#b8c9dc",
"#ee7621ff", "#CDD4DC", "#CDD4DC", "#CDD4DC", 
"magenta", 
"#4CB364"])' 

my_colours<-'d3.scaleOrdinal() .domain(["Iron", "Vitamin_C", "Calcium", "Folate", "Vitamin_A", "Vitamin_E", "None",
"PD", "animal", "fortified", "Non_PD",
"key_poll",
"key_plant",
]) .range(["#7BAFDD",  "#CAE0AC", "#F3EC5A", "#68312D", "#1A66AB", "#863771", "#b8c9dc",
"#EE8238", "#CDD4DC", "#CDD4DC", "#CDD4DC", 
"#DC3439", 
"#4CB364"])' 

#"#b8c9dc","#b8c9dc",  "#b8c9dc", "#b8c9dc", "#f4c623", "#F39999", "#2f7ab9", 

#"#ffd28f","#f0b64d","#6876a4","#0064ab","#005083","#c99f6e", "#9f8a89"

#"#f4c623","#cd3122",  "#bee183", "#6c905e", "#2f533c", "#b8c9dc", "#2f7ab9"

###########################################
## Plot the Sankey diagram
###########################################

# plot the sankey
sn<-sankeyNetwork(Links = all_links, 
                  Nodes = all_nodes, 
                  Source = 'source', 
                  Target = 'target', 
                  Value = 'value', 
                  NodeID = 'name',
                  fontSize = 15, nodeWidth = 20,
                  nodePadding = 5, #determines link width
                  iterations = 5,
                  fontFamily = "sans-serif",
                  colourScale = my_colours,
                  LinkGroup = "group",
                  NodeGroup="group")

# white border around words for legibility
sn2<- htmlwidgets::onRender(
  sn,
  '
  function(el) {
    d3.select(el).selectAll(".node text"). attr("font-style", "italic");
     d3.selectAll(".node text").style("text-shadow", "1px 0 white, -1px 0 white, 0 1px white, 0-1px white")
  }
  '
)
sn

#Export the Sankey diagram

saveNetwork(sn2, "plots/Poll_crop_nutrient_sankey_web.html")
webshot("Poll_crop_nutrient_sankey_web.html", "Poll_crop_nutrient_sankey_web.png", vwidth = 800, vheight = 500, zoom = 5) #Zoom value determines the export quality
