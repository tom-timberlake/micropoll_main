##################################################################################################################################################################
##############      MICRO-POLL SCRIPT 2B - LINKING CROPS, POLLINATORS & MICRONUTRIENTS AT VILLAGE LEVEL     ############################################
##################################################################################################################################################################

#Note that this script does much of the same analysis as Script 2, but it is done at a village level instead of the meta-network level

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


##Import crop nutrient data
crop_summary <- read.csv("output_data/Nutrients_by_crop_by_village.csv") 

#Import pollinator interaction data
plant_poll_data <- data.table(read_excel("input_data/MP_pollinator_visitation.xlsx", 
                                         sheet = "Visitation data"))

##Import pollen carrying capacity data
pollen_data <- read.csv("input_data/pollen_capacity_OTU.csv") 


##################################################################################################################################################
################################   Calculating crop, pollinator and wild plant contribution scores    ############################################
##################################################################################################################################################

#######################################################################
#######           Organising crop - nutrient data           #######
#######################################################################

#Subset crop-nutrient data to columns and nutrients of interest
crop_summary_edits <- select(crop_summary,
                             village_code,
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
crop_contribution_summary <- crop_summary_long %>%  
 dplyr:: group_by(village_code, nutrient, sci_name, eng_name) %>%
 dplyr:: summarize(prop_contribution = sum(prop_contribution))

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
#######           Plot village-level crop importance          #######
#######################################################################

# Calculate mean values for each category
mean_crop_values <- crop_contribution_wide %>%
 dplyr:: group_by(eng_name) %>%
 dplyr:: summarize(mean_value = mean(mean_contribution))

##Subset to the top 25 most important crops
important_crops_top25 <- mean_crop_values %>%  top_n(25, mean_value)
crop_contribution_subset <- subset(crop_contribution_wide, eng_name %in% important_crops_top25$eng_name)


# Arrange categorical levels based on mean economic values
crop_contribution_subset$eng_name <- factor(crop_contribution_subset$eng_name, levels = mean_crop_values$eng_name[order(mean_crop_values$mean_value, decreasing = TRUE)])

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

#Plot nutritional importance of each crop
crop_value_village <- ggplot(crop_contribution_subset, aes(x = eng_name, y = mean_contribution*10)) + 
  geom_boxplot(width = 0.4, fill = "lightgray", alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = village_code), width = 0.2, alpha = 0.7, size = 3) +  # Map village_code to color
  scale_color_manual(values = village_colors) +  # Set colors manually
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentage
  labs(title = "(a)", x = "Food item", y = "Mean contribution to 6 key micronutrients", color = "Study village") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot=crop_value_village, filename="plots/Crop_value_village.svg", width=8, height=5, dpi=600, bg="white")
ggsave(plot=crop_value_village, filename="plots/Crop_value_village.png", width=8, height=5, dpi=600, bg="white")


#######################################################################
#######           Organising poll - crop data           #######
#######################################################################

plant_poll_data_edits <- plant_poll_data

#Remove rows with NA for plant or pollinator
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(insect_OTU)
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(plant_sci_name)

#Summarise plant-pol interactions - calculate the number of visits made to each plant
plant_poll_summary <- plant_poll_data %>% dplyr:: group_by(village_code, plant_sci_name, plant_category, insect_OTU) %>% dplyr:: summarize(count = n()) %>%  ungroup()

#Merge in pollen data
pollen_transport_summary <- merge(x=plant_poll_summary, y=pollen_data, by="insect_OTU",all.x=TRUE)

#Multiply visitation frequency by pollen transport capacity
pollen_transport_summary$total_pollen_trans <- pollen_transport_summary$count * pollen_transport_summary$mean_pollen_load

#Remove NA values
pollen_transport_summary <- pollen_transport_summary %>% drop_na(total_pollen_trans)

#Calculate proportion of total pollen transported by each insect
pollen_proportion_summary <- pollen_transport_summary %>% dplyr:: group_by(village_code, plant_sci_name) %>%  mutate(proportion_pollen = total_pollen_trans / sum(total_pollen_trans))

#Change name of column header to match with crop-nutrient data
colnames(pollen_proportion_summary)[colnames(pollen_proportion_summary) == "plant_sci_name"] <- "sci_name"

#######################################################################
#######  Merge poll-importance data with crop-nutrient data     #######
#######################################################################

#Merge visitation data into crop-nutrient dataset
poll_crop_nutrient_data <- merge(x=crop_summary_long, y=pollen_proportion_summary, by = c("sci_name", "village_code"),all.x=TRUE)

poll_crop_nutrient_data[is.na(poll_crop_nutrient_data)] <- 0

# Identify rows where 'final_poll_dependence' is equal to 0 and change all insect values to zero
rows_to_zero <- poll_crop_nutrient_data$final_poll_dependence == 0
poll_crop_nutrient_data[rows_to_zero, "proportion_pollen"] <- 0

#Create new variable which calculates the proportional contribution of each insect pollinator to each nutrient (i.e. the Pollinator Contribution, PC score)
# Formula for this metric = Proportion pollen transport * Proportional contribution to nutrient * Pollinator dependence of crop
poll_crop_nutrient_data_calcs <- poll_crop_nutrient_data %>%  
  mutate(poll_contribution = prop_contribution * proportion_pollen * final_poll_dependence)

#Summarise poll_contribution score for each insect
poll_contribution_summary <- poll_crop_nutrient_data_calcs %>% dplyr:: group_by(village_code, nutrient, insect_OTU) %>%
 dplyr:: summarize(poll_contribution = sum(poll_contribution))

# Remove rows where poll_contribution is 0
#poll_contribution_summary <- poll_contribution_summary %>%  filter(poll_contribution != 0)  

#Create a table showing top insects for each nutrient (i.e. convert to wide format)
poll_contribution_wide <- poll_contribution_summary %>%
  pivot_wider(names_from = nutrient,  # The column that will become the new column names
              values_from = poll_contribution,  # The column that will fill the new column values
              values_fill = list(poll_contribution = 0))  # Fill missing values with 0 (or NA if preferred)

#Calculate mean contribution across all 6 nutrients
poll_contribution_wide <- poll_contribution_wide %>%  
  mutate(mean_contribution = (Calcium + FolateTotal + Iron + VitARE + VitaminC + VitaminE)/6)

poll_contribution_wide <- poll_contribution_wide %>%  arrange(desc(mean_contribution))

#######################################################################
#######           Plot village-level insect importance          #######
#######################################################################

# Calculate mean values for each category
mean_insect_values <- poll_contribution_wide %>%
 dplyr:: group_by(insect_OTU) %>%
 dplyr:: summarize(mean_value = sum(mean_contribution)/10) #Calculating the mean importance value across all 10 villages, therefore taking into account NA values

##Subset to the top 25 most important crops
important_insects_top25 <- mean_insect_values %>%  top_n(25, mean_value)
poll_contribution_subset <- subset(poll_contribution_wide, insect_OTU %in% important_insects_top25$insect_OTU)

# Arrange categorical levels based on mean economic values
poll_contribution_subset$insect_OTU <- factor(poll_contribution_subset$insect_OTU, levels = mean_insect_values$insect_OTU[order(mean_insect_values$mean_value, decreasing = TRUE)])

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

#Plot nutritional importance of each insect
insect_value_village <- ggplot(poll_contribution_subset, aes(x = insect_OTU, y = mean_contribution*10)) + 
  geom_boxplot(width = 0.4, fill = "lightgray", alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = village_code), width = 0.2, alpha = 0.7, size = 3) +  # Map village_code to color
  scale_color_manual(values = village_colors) +  # Set colors manually
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentage
  labs(title = "(b)", x = "Insect taxon", y = "Pollinator Contribution (PC) score", color = "Study village") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot=insect_value_village, filename="plots/Insect_value_village.svg", width=8, height=5, dpi=600, bg="white")
ggsave(plot=insect_value_village, filename="plots/Insect_value_village.png", width=8, height=5, dpi=600, bg="white")



##################################################################################################
### Calculate the indirect contribution to the pollination service of each wild plant 
##################################################################################################

#Start by removing all crop plant data from the plant-poll dataset as we are only interested in non-crop resources
plant_interactions_wild <- plant_poll_summary %>%  filter(plant_category != 'crop')

#Calculate the total visits made by each insect to ALL wild plants
insect_visits_all <- plant_interactions_wild %>%  dplyr:: group_by(village_code, insect_OTU) %>%  dplyr::summarise(total_insect_visits= sum(count, na.rm=TRUE)) 

#Calculate the number of visits made by each insect to EACH wild plant
insect_visits_by_plant <- plant_interactions_wild %>%  dplyr:: group_by(village_code, insect_OTU, plant_sci_name ) %>%  dplyr::summarise(plant_insect_visits= sum(count, na.rm=TRUE)) 

# Merge the full dataset with the total visit data so that individual plant-poll visits can be calculated as a proportion of all visits made by the insect
plant_resource_use_merge <- merge(x=insect_visits_by_plant, y=insect_visits_all, by=c("insect_OTU", "village_code"), all.x=TRUE)

#Divide each interaction frequency value by the total number number of visits performed by the insect
plant_resource_use_merge$prop_plant_use <- plant_resource_use_merge$plant_insect_visits / plant_resource_use_merge$total_insect_visits 

#Change name of column header to match with crop-nutrient data
colnames(plant_resource_use_merge)[colnames(plant_resource_use_merge) == "plant_sci_name"] <- "plant_resource_name"

#Merge scores for each plant-insect combination with crop-poll data so that total indirect contribution scores can be calculated
poll_contribution_plant_indirect <- merge(x=poll_contribution_summary, y=plant_resource_use_merge, by=c("insect_OTU", "village_code"),all.x=TRUE) 

#Create new variable which calculates the indirect contribution of each plant resource to each nutrient (i.e. the Indirect Contribution, IC score)
# Formula for this metric = Pollinator contribution * Proportional plant use
poll_contribution_plant_indirect <- poll_contribution_plant_indirect %>%  
  mutate(plant_IC_score = poll_contribution * prop_plant_use)

#Ungroup the data
poll_contribution_plant_indirect <- poll_contribution_plant_indirect %>%  ungroup()

#Summarise plant_IC_score for each plant-nutrient combination
plant_contribution_summary <- poll_contribution_plant_indirect %>% dplyr:: group_by(village_code, nutrient, plant_resource_name) %>%
 dplyr:: summarize(plant_IC_score = sum(plant_IC_score))

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


#######################################################################
#######           Plot village-level plant importance          #######
#######################################################################

# Calculate mean values for each category
mean_plant_values <- plant_contribution_wide %>%
 dplyr:: group_by(plant_resource_name) %>%
 dplyr:: summarize(mean_value = sum(mean_contribution)/10) #Calculating the mean importance value across all 10 villages, therefore taking into account NA values

##Subset to the top 25 most important crops
important_plants_top25 <- mean_plant_values %>%  top_n(25, mean_value)
plant_contribution_subset <- subset(plant_contribution_wide, plant_resource_name %in% important_plants_top25$plant_resource_name)

# Arrange categorical levels based on mean economic values
plant_contribution_subset$plant_resource_name <- factor(plant_contribution_subset$plant_resource_name, levels = mean_plant_values$plant_resource_name[order(mean_plant_values$mean_value, decreasing = TRUE)])

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

#Plot nutritional importance of each plant
plant_value_village <- ggplot(plant_contribution_subset, aes(x = plant_resource_name, y = mean_contribution*10)) + 
  geom_boxplot(width = 0.4, fill = "lightgray", alpha = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = village_code), width = 0.2, alpha = 0.7, size = 3) +  # Map village_code to color
  scale_color_manual(values = village_colors) +  # Set colors manually
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Format y-axis as percentage
  labs(title = "(c)", x = "Wild plant species", y = "Plant Indirect Contribution (IC) score", color = "Study village") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.05)) +  # Set y-axis limits (0% to 5%)
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 5, 5, 20))  # Adjust plot margins (top, right, bottom, left))

ggsave(plot=plant_value_village, filename="plots/Plant_value_village.svg", width=8, height=5, dpi=600, bg="white")
ggsave(plot=plant_value_village, filename="plots/Plant_value_village.png", width=8, height=5, dpi=600, bg="white")




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
saveWorkbook(wb, file.path(output.path, paste0( "Nutrient_contributions_crop_poll_plant_", Sys.Date(), ".xlsx")), overwrite = TRUE)
