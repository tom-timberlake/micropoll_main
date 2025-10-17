##################################################################################################################################################################
#########################################     MICRO-POLL SCRIPT 3A - MODELLING CONTINUOUS POLLINATOR DECLINE      ################################################
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
library(patchwork)
library(forcats)
library(scales)
library(future.apply)
library(progressr)
library(lme4)
library(purrr)


#Import plant-pollinator interaction data from main "input_data" folder - this data shows all of the interactions between crop plants and insect pollinators
plant_poll_data <- data.table(read_excel("input_data/MP_pollinator_visitation.xlsx", 
                                         sheet = "Visitation data"))

##Import pollen carrying capacity data (number of pollen grains transported by each insect taxa) from main "input_data" folder - this data is used to weight the interaction frequency of each pollinator to take into account their effectiveness as a pollen transporter
pollen_data <- read.csv("input_data/pollen_capacity_OTU.csv") 

##Import intake data - this data shows the total quantity of each ingredient consumed by each individual at each time point and the nutritional value of the ingredients
intake_data <- read.csv("output_data/Nutrient_intake_data_PD.csv")



#######################################################################
#######           Organising poll - crop data           #######
#######################################################################

plant_poll_data_edits <- plant_poll_data

#Remove rows with NA for plant or pollinator
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(insect_OTU)
plant_poll_data_edits <- plant_poll_data_edits %>% drop_na(plant_sci_name)

#Summarise plant-pol interactions - calculate the number of visits made to each plant
plant_poll_summary <- plant_poll_data %>%  dplyr::group_by(plant_sci_name, plant_category, pollinator_taxa, insect_order, insect_family, insect_genus, insect_OTU) %>%  dplyr::summarise(count = n()) %>%  ungroup()

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

#Subset to only prop pollen transported
pollen_proportion_summary_subset <- select(pollen_proportion_summary, sci_name,pollinator_taxa, insect_order, insect_family, insect_genus, insect_OTU, proportion_pollen)
names(pollen_proportion_summary_subset)

#######################################################################
#######           Organising dietary data                #######
#######################################################################

#Subset data to columns of interest
intake_data_subset <- intake_data %>%
  select(formid,	resp_id,	date,	resp_cat, village_code,  ingredient_code, ingredient_name,	ingredient_grams_consumed,	
         sci_name,	final_poll_dependence,	
         int_Energy_Kcal,	int_Fat_g,	int_Protein_g,	
         int_Calcium_mg,	int_Iron_mg,	int_Zinc_mg,	int_VitaminC_mg,	int_FolateTotal_µg, int_VitARE_µg,	int_VitaminE_mg,
         int_ThiaminB1_mg,	int_RiboflavinB2_mg, int_NiacinB3_mg,	int_VitaminB6pyridoxine_mg,	int_VitaminB12_µg)
         
#Group by relevant categories and sum intakes of each person during each recall event
daily_intakes <- intake_data_subset %>%
  group_by(formid,resp_id, date, resp_cat, village_code, 
           sci_name,final_poll_dependence) %>%
  summarise(ingredient_grams_consumed = sum(ingredient_grams_consumed, na.rm = TRUE),
            int_Energy_Kcal = sum(int_Energy_Kcal, na.rm = TRUE),
            int_Fat_g = sum(int_Fat_g, na.rm = TRUE),
            int_Protein_g = sum(int_Protein_g, na.rm = TRUE),
            int_Calcium_mg = sum(int_Calcium_mg, na.rm = TRUE),
            int_Iron_mg = sum(int_Iron_mg, na.rm = TRUE),
            int_Zinc_mg = sum(int_Zinc_mg, na.rm = TRUE),
            int_VitaminC_mg = sum(int_VitaminC_mg, na.rm = TRUE),
            int_ThiaminB1_mg = sum(int_ThiaminB1_mg, na.rm = TRUE),
            int_RiboflavinB2_mg = sum(int_RiboflavinB2_mg, na.rm = TRUE),
            int_NiacinB3_mg = sum(int_NiacinB3_mg, na.rm = TRUE),
            int_VitaminB6pyridoxine_mg = sum(int_VitaminB6pyridoxine_mg, na.rm = TRUE),
            int_FolateTotal_µg = sum(int_FolateTotal_µg, na.rm = TRUE),
            int_VitARE_µg = sum(int_VitARE_µg, na.rm = TRUE),
            int_VitaminE_mg = sum(int_VitaminE_mg, na.rm = TRUE),
            int_VitaminB12_µg = sum(int_VitaminB12_µg, na.rm = TRUE),
            .groups = "drop")


#######################################################################
#######  Merge poll-importance data with dietary data     #######
#######################################################################

#Merge visitation data into dietary dataset matching by sci_name
intake_pollination_data <- merge(x=daily_intakes, y=pollen_proportion_summary_subset, by="sci_name",all.x=TRUE)

#For any ingredients where insect interactions were not recorded, we enter the text 'Other' into the insect ID columns and enter 1 into prop_pollen 
#This ensures that when we simulate pollinator declines, pollinator-dependent ingredients without interaction info are not ignored and will also decline

intake_pollination_data <- intake_pollination_data %>%
  dplyr::mutate(is_missing_taxa = is.na(pollinator_taxa),  # Temporary helper column
    pollinator_taxa = if_else(is_missing_taxa, "Other", pollinator_taxa),
    insect_order = if_else(is_missing_taxa, "Other", insect_order),
    insect_family = if_else(is_missing_taxa, "Other", insect_family),
    insect_genus = if_else(is_missing_taxa, "Other", insect_genus),
    insect_OTU = if_else(is_missing_taxa, "Other", insect_OTU),
    proportion_pollen = if_else(is_missing_taxa, 1, proportion_pollen)) %>%
  select(-is_missing_taxa)  # Remove helper column
  

#Calculate intake of each nutrient (from each ingredient) that is attributable to each insect taxon
# Formula for this metric = Intake of nutrient X * Pollinator dependence of crop * Proportion pollen transport by given insect
intake_pollination_data <- intake_pollination_data %>%
  dplyr::mutate(int_Energy_poll = (int_Energy_Kcal * final_poll_dependence) * proportion_pollen,
                int_Fat_poll = (int_Fat_g * final_poll_dependence) * proportion_pollen,
                int_Protein_poll = (int_Protein_g * final_poll_dependence) * proportion_pollen,
                int_Calcium_poll = (int_Calcium_mg * final_poll_dependence) * proportion_pollen,
                int_Iron_poll = (int_Iron_mg * final_poll_dependence) * proportion_pollen,
                int_Zinc_poll = (int_Zinc_mg * final_poll_dependence) * proportion_pollen,
                int_VitaminC_poll = (int_VitaminC_mg * final_poll_dependence) * proportion_pollen,
                int_FolateTotal_poll = (int_FolateTotal_µg * final_poll_dependence) * proportion_pollen,
                int_VitARE_poll = (int_VitARE_µg * final_poll_dependence) * proportion_pollen,
                int_VitaminE_poll = (int_VitaminE_mg * final_poll_dependence) * proportion_pollen,
                int_ThiaminB1_poll = (int_ThiaminB1_mg * final_poll_dependence) * proportion_pollen,
                int_RiboflavinB2_poll = (int_RiboflavinB2_mg * final_poll_dependence) * proportion_pollen,
                int_NiacinB3_poll = (int_NiacinB3_mg * final_poll_dependence) * proportion_pollen,
                int_VitaminB6_poll = (int_VitaminB6pyridoxine_mg * final_poll_dependence) * proportion_pollen,
                int_VitaminB12_poll = (int_VitaminB12_µg * final_poll_dependence) * proportion_pollen)


############################################################################################
#######  Remove pollinator-dependent component of intake by a certain proportion  - continuous   #######
############################################################################################

############
###  Warning: This next piece of code takes a long time to run!
############

# Create a sequence of pollinator decline rates from 1 to 0 by -0.05
decline_rates <- seq(1.1, 0, by = -0.1)

# Number of random replicates
n_simulations <- 100

#Calculate total number of iterations
total_iterations <- n_simulations * length(decline_rates)
iteration_counter <- 0

# Create an empty list to store population-level results
decline_results_list <- list()

# New list to store participant-level outputs
individual_decline_results_list <- list()

# Outer loop for different random seeds
for (sim in 1:n_simulations) {
  
  set.seed(100 + sim)  # Set a different seed each time (100, 101, ..., 109)

  # Inner loop for each pollinator decline rate
  for (poll_decline in decline_rates) {

    #Insert iteration counting
    iteration_counter <- iteration_counter + 1
    cat(sprintf("Progress: %3.1f%% — Simulation %d | Decline rate %.1f | Time: %s\n",
                100 * iteration_counter / total_iterations,
                sim, poll_decline, format(Sys.time(), "%H:%M:%S")))
    
    #Assign each pollinator taxa a random decline rate based on a mean of the overall poll_decline value and an sd of 0.2
    otu_lookup <- data.frame(insect_OTU = unique(intake_pollination_data$insect_OTU),
                             species_decline = rnorm(length(unique(intake_pollination_data$insect_OTU)), mean = poll_decline, sd = 0.2))
    
    # Clip to [0, 1]
    otu_lookup$species_decline[otu_lookup$species_decline < 0] <- 0
    otu_lookup$species_decline[otu_lookup$species_decline > 1] <- 1
    
    # Merge decline rates back to main dataframe
    intake_pollination_data_decl <- intake_pollination_data %>%
      left_join(otu_lookup, by = "insect_OTU")
    
    #Calculate the amount that intake of each nutrient for each food item would decline based on each pollinators specific rate of decline
    #This also calculates the amount of pollen transport that is lost
    poll_change_by_sp <- intake_pollination_data_decl %>%
      dplyr::mutate(int_Energy_poll_change = int_Energy_poll * species_decline,
                    int_Fat_poll_change = int_Fat_poll * species_decline,
                    int_Protein_poll_change = int_Protein_poll * species_decline,
                    int_Calcium_poll_change = int_Calcium_poll * species_decline,
                    int_Iron_poll_change = int_Iron_poll * species_decline,
                    int_Zinc_poll_change = int_Zinc_poll * species_decline,
                    int_VitaminC_poll_change = int_VitaminC_poll * species_decline,
                    int_FolateTotal_poll_change = int_FolateTotal_poll * species_decline,
                    int_VitARE_poll_change = int_VitARE_poll * species_decline,
                    int_VitaminE_poll_change = int_VitaminE_poll * species_decline,
                    int_ThiaminB1_poll_change = int_ThiaminB1_poll * species_decline,
                    int_RiboflavinB2_poll_change = int_RiboflavinB2_poll * species_decline,
                    int_NiacinB3_poll_change = int_NiacinB3_poll * species_decline,
                    int_VitaminB6_poll_change = int_VitaminB6_poll * species_decline,
                    int_VitaminB12_poll_change = int_VitaminB12_poll * species_decline,
                    pollen_loss = proportion_pollen * species_decline)
    
    #Sum nutrient declines by ingredients so that the decline of all pollinator species is taken into account
    poll_change_by_ingred <- poll_change_by_sp %>%
      dplyr::group_by(formid,resp_id, resp_cat, village_code, sci_name,                              
                      final_poll_dependence, ingredient_grams_consumed, int_Energy_Kcal, int_Fat_g, int_Protein_g,               
                      int_Calcium_mg, int_Iron_mg, int_Zinc_mg, int_VitaminC_mg, int_ThiaminB1_mg,         
                      int_RiboflavinB2_mg, int_NiacinB3_mg, int_VitaminB6pyridoxine_mg, int_FolateTotal_µg, int_VitARE_µg,              
                      int_VitaminE_mg, int_VitaminB12_µg ) %>%
      dplyr::summarise(int_Energy_poll_change = sum(int_Energy_poll_change),
                       int_Fat_poll_change = sum(int_Fat_poll_change),
                       int_Protein_poll_change = sum(int_Protein_poll_change),
                       int_Calcium_poll_change = sum(int_Calcium_poll_change),
                       int_Iron_poll_change = sum(int_Iron_poll_change),
                       int_Zinc_poll_change = sum(int_Zinc_poll_change),
                       int_VitaminC_poll_change = sum(int_VitaminC_poll_change),
                       int_FolateTotal_poll_change = sum(int_FolateTotal_poll_change),
                       int_VitARE_poll_change = sum(int_VitARE_poll_change),
                       int_VitaminE_poll_change = sum(int_VitaminE_poll_change),
                       int_ThiaminB1_poll_change = sum(int_ThiaminB1_poll_change),
                       int_RiboflavinB2_poll_change = sum(int_RiboflavinB2_poll_change),
                       int_NiacinB3_poll_change = sum(int_NiacinB3_poll_change),
                       int_VitaminB6_poll_change = sum(int_VitaminB6_poll_change),
                       int_VitaminB12_poll_change = sum(int_VitaminB12_poll_change),
                       int_proportion_pollen = sum(proportion_pollen),
                       int_pollen_loss = sum(pollen_loss))
    
    
    #Calculate total decline in daily intake of each nutrient after pollinator declines for each participant during each recall event
    new_daily_intakes <- poll_change_by_ingred %>%
      dplyr::group_by(formid, resp_id, resp_cat, village_code) %>%
      dplyr::summarise(
        dplyr::across(starts_with("int_"), 
                      ~sum(.x, na.rm = TRUE)),
        .groups = "drop")
    
    ######################################################################################
    ########## Implement replacement of missing food items based on calorific replacement with plain white rice
    ######################################################################################
    
    #Calculate the loss of calories resulting from pollinator decline
    new_daily_intakes <- new_daily_intakes %>%
      dplyr::mutate(kcal_loss = int_Energy_poll_change)
    
    #Calculate grams of rice this translates to in units of 100 grams (plain white rice has 356.358kcal per 100 gram). This tells us how much much rice we need to replace in the diet in units of 100 grams
    #The reason for keeping the units as 100 grams is it allows to simply multiply the densisites of each nutrient per hundred grams in the next step, rather than having to convert them to nutrient/gram
    new_daily_intakes <- new_daily_intakes %>%
      mutate(additional_rice_poll_decl = kcal_loss/356.358)
    
    
    #Add extra nutrients provided by rice - we now calculate the additional nutrients that people obtain by eating more rice and remove this from the quantity that is expected to be lost as a result of pollinator decline
    new_daily_intakes_with_replacement <- new_daily_intakes %>%
      mutate(int_Energy_poll_change = int_Energy_poll_change - (additional_rice_poll_decl*356.358),  #Energy  value from FCT for 'Rice, Fine grain, Basmati'
             int_Fat_poll_change = int_Fat_poll_change - (additional_rice_poll_decl*0.52),  #Fat  value from FCT for 'Rice, Fine grain, Basmati'
             int_Protein_poll_change = int_Protein_poll_change - (additional_rice_poll_decl*7.94),  #Protein  value from FCT for 'Rice, Fine grain, Basmati'
             int_Calcium_poll_change = int_Calcium_poll_change - (additional_rice_poll_decl*7.49),  #Calcium  value from FCT for 'Rice, Fine grain, Basmati'
             int_Iron_poll_change = int_Iron_poll_change - (additional_rice_poll_decl*0.65),  #Iron  value from FCT for 'Rice, Fine grain, Basmati'
             int_Zinc_poll_change = int_Zinc_poll_change - (additional_rice_poll_decl*1.21),  #Zinc  value from FCT for 'Rice, Fine grain, Basmati'
             int_VitaminC_poll_change = int_VitaminC_poll_change - (additional_rice_poll_decl*0),  #VitaminC  value from FCT for 'Rice, Fine grain, Basmati'
             int_ThiaminB1_poll_change = int_ThiaminB1_poll_change - (additional_rice_poll_decl*0.05),  #ThiaminB1  value from FCT for 'Rice, Fine grain, Basmati'
             int_RiboflavinB2_poll_change = int_RiboflavinB2_poll_change - (additional_rice_poll_decl*0.05),  #RiboflavinB2  value from FCT for 'Rice, Fine grain, Basmati'
             int_NiacinB3_poll_change = int_NiacinB3_poll_change - (additional_rice_poll_decl*1.69),  #NiacinB3  value from FCT for 'Rice, Fine grain, Basmati'
             int_VitaminB6_poll_change = int_VitaminB6_poll_change - (additional_rice_poll_decl*0.12),  #VitaminB6pyridoxine  value from FCT for 'Rice, Fine grain, Basmati'
             int_FolateTotal_poll_change = int_FolateTotal_poll_change - (additional_rice_poll_decl*9.32),  #FolateTotal  value from FCT for 'Rice, Fine grain, Basmati'
             int_VitARE_poll_change = int_VitARE_poll_change - (additional_rice_poll_decl*0),  #VitARE  value from FCT for 'Rice, Fine grain, Basmati'
             int_VitaminE_poll_change = int_VitaminE_poll_change - (additional_rice_poll_decl*0.06),  #VitaminE  value from FCT for 'Rice, Fine grain, Basmati'
             int_VitaminB12_poll_change = int_VitaminB12_poll_change - (additional_rice_poll_decl*0))  #VitaminB12  value from FCT for 'Rice, Fine grain, Basmati'
    
    
    #Calculate mean daily intake of each nutrient after pollinator declines for each participant during the course of the year
    new_daily_intakes_mean <- new_daily_intakes_with_replacement %>%
      dplyr::group_by(resp_id, resp_cat, village_code) %>%
      dplyr::summarise(
        dplyr::across(starts_with("int_"), 
                      ~mean(.x, na.rm = TRUE)),
        .groups = "drop")
    
    
    #Calculate percentage change in daily intake for each participant in each nutrient
    #Also calculate percentage change in pollen transport as a result of decline
    new_daily_intakes_mean <- new_daily_intakes_mean %>%
      dplyr::mutate(propdecl_Energy = int_Energy_poll_change / int_Energy_Kcal,
                    propdecl_Fat = int_Fat_poll_change / int_Fat_g,
                    propdecl_Protein = int_Protein_poll_change / int_Protein_g,
                    propdecl_Calcium = int_Calcium_poll_change / int_Calcium_mg,
                    propdecl_Iron = int_Iron_poll_change / int_Iron_mg,
                    propdecl_Zinc = int_Zinc_poll_change / int_Zinc_mg,
                    propdecl_VitC = int_VitaminC_poll_change / int_VitaminC_mg,
                    propdecl_VitB1 = int_ThiaminB1_poll_change / int_ThiaminB1_mg,
                    propdecl_VitB2 = int_RiboflavinB2_poll_change / int_RiboflavinB2_mg,
                    propdecl_VitB3 = int_NiacinB3_poll_change / int_NiacinB3_mg,
                    propdecl_VitB6 = int_VitaminB6_poll_change / int_VitaminB6pyridoxine_mg,
                    propdecl_Folate = int_FolateTotal_poll_change / int_FolateTotal_µg,
                    propdecl_VitA = int_VitARE_poll_change / int_VitARE_µg,
                    propdecl_VitE = int_VitaminE_poll_change / int_VitaminE_mg,
                    propdecl_VitB12 = int_VitaminB12_poll_change / int_VitaminB12_µg,
                    propdecl_pollen = int_pollen_loss/ int_proportion_pollen)
    
    
    new_daily_intakes_mean <- dplyr::mutate_all(new_daily_intakes_mean, ~replace_na(.x, 0))
    
    # Add identifiers for simulation and decline level to the individual-level data
    new_daily_intakes_mean$decline_rate <- poll_decline
    new_daily_intakes_mean$simulation <- sim
    
    # Save to list
    individual_decline_results_list[[paste0("sim", sim, "_decl", poll_decline)]] <- new_daily_intakes_mean
    
    #Summarise mean decline across the whole population
    decline_summary <- new_daily_intakes_mean %>%
      dplyr::summarise(propdecl_Energy = mean(propdecl_Energy), 
                       propdecl_Fat = mean(propdecl_Fat), 
                       propdecl_Protein = mean(propdecl_Protein), 
                       propdecl_Calcium = mean(propdecl_Calcium), 
                       propdecl_Iron = mean(propdecl_Iron), 
                       propdecl_Zinc = mean(propdecl_Zinc), 
                       propdecl_VitC = mean(propdecl_VitC), 
                       propdecl_VitB1 = mean(propdecl_VitB1), 
                       propdecl_VitB2 = mean(propdecl_VitB2), 
                       propdecl_VitB3 = mean(propdecl_VitB3), 
                       propdecl_VitB6 = mean(propdecl_VitB6), 
                       propdecl_Folate = mean(propdecl_Folate), 
                       propdecl_VitA = mean(propdecl_VitA), 
                       propdecl_VitE = mean(propdecl_VitE), 
                       propdecl_VitB12 = mean(propdecl_VitB12),
                       propdecl_pollen = mean(propdecl_pollen))
    
    
    # Add identifiers for simulation and decline level
    decline_summary$decline_rate <- poll_decline
    decline_summary$simulation <- sim
    
    # Save summary to list with a composite name
    decline_results_list[[paste0("sim", sim, "_decl", poll_decline)]] <- decline_summary
  }
}

######
## Population-level results
######

# Combine all population-level summaries into one data frame
decline_results_population <- dplyr::bind_rows(decline_results_list)

#Add zero % nutrient intake decline to zero% pollination decline (this is currently absent because the model never generates a true zero% pollination decline scenario)
zero_rows_pop <- decline_results_population %>%
  distinct(simulation) %>%  # Get unique simulation values
  rowwise() %>%
  mutate(data = list(as_tibble(setNames(as.list(rep(0, ncol(decline_results_population) - 1)), names(decline_results_population)[names(decline_results_population) != "simulation"])))) %>%
  unnest(cols = c(data)) %>%
  select(names(decline_results_population))  # Ensure column order matches

# Add the zero rows to your original data
decline_results_population_final <- bind_rows(decline_results_population, zero_rows_pop)

# Export population-level results
write.csv(decline_results_population_final, "output_data/Pollinator_decline_results_population.csv", row.names = FALSE)


######
## Individual-level results
######

# Combine all individual-level results into one data frame
decline_results_individual <- dplyr::bind_rows(individual_decline_results_list)

#Add zero % nutrient intake decline to zero% pollination decline (this is currently absent because the model never generates a true zero% pollination decline scenario)

decline_results_individual_final <- decline_results_individual %>%
  dplyr::mutate(across(
    starts_with("propdecl_"),
    ~ ifelse(decline_rate == 0, 0, .)))
  

# Export individual-level results
#write.csv(decline_results_individual_final, "output_data/Pollinator_decline_results_individual.csv", row.names = FALSE)



######################################################################################################
###############   Summarise and Plot population-level results      ###################################
######################################################################################################

# Pivot to long
decline_results_pop_long <- decline_results_population_final %>%
  select(simulation, decline_rate, propdecl_pollen, starts_with("propdecl_")) %>%
  pivot_longer(
    cols = starts_with("propdecl_") & !starts_with("propdecl_pollen"),  # exclude pollen
    names_to = "nutrient",
    values_to = "decline_value") %>%
  mutate(
    nutrient = gsub("propdecl_", "", nutrient)) 


summary_stats_pop <- decline_results_pop_long %>%
  group_by(decline_rate, nutrient) %>%
  summarise(mean_decline_pollen = mean(propdecl_pollen, na.rm = TRUE),
    mean_decline = mean(decline_value, na.rm = TRUE),
    lower_CI = quantile(decline_value, 0.025, na.rm = TRUE),
    upper_CI = quantile(decline_value, 0.975, na.rm = TRUE),
    .groups = "drop")

#Add zero values into the dataset
all_nutrients <- unique(summary_stats_pop$nutrient)
baseline_rows <- tibble(decline_rate = 0,
  nutrient = all_nutrients,
  mean_decline_pollen = 0,
  mean_decline = 0,
  lower_CI = 0,
  upper_CI = 0)
summary_stats_full <- summary_stats_pop %>%
  bind_rows(baseline_rows)
  
# List of nutrients you want to plot
nutrients_to_plot <- c("VitA",  "VitE", "VitC","Folate", "Calcium", "Iron") 

# Custom labels
nutrient_labels <- c(
  "VitA" = "Vitamin A",
  "Folate" = "Folate",
  "Iron" = "Iron",
  "Calcium" = "Calcium",
  "VitC" = "Vitamin C",
  "VitE" = "Vitamin E"
)

# Filter and reorder nutrients
summary_stats_filtered <- summary_stats_pop %>%
  filter(nutrient %in% nutrients_to_plot) %>%
  mutate(nutrient = factor(nutrient, levels = nutrients_to_plot))

# Create the plot
poll_decline_popultation_level <- ggplot(summary_stats_filtered, aes(x = mean_decline_pollen, y = -mean_decline)) +
  geom_line() +
  geom_ribbon(aes(ymin = -upper_CI, ymax = -lower_CI), alpha = 0.2) +
  facet_wrap(~ nutrient, labeller = labeller(nutrient = nutrient_labels)) +
  labs(
    x = "Pollination Service Decline (%)",
    y = "Decline in Nutrient Intake (%)"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0)) +  # adjust range as needed
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1)

  
#Save plot
ggsave(plot=poll_decline_popultation_level, filename="plots/Poll_decline_popultation_level.svg", width=7, height=5, dpi=600, bg="white")
ggsave(plot=poll_decline_popultation_level, filename="plots/Poll_decline_popultation_level.png", width=7, height=5, dpi=600, bg="white")


######################################################################################################
###############     Summarise individual results  by resp_cat and village     ###################################
######################################################################################################


# Pivot to long
decline_results_indiv_long <- decline_results_individual_final %>%
  select(resp_id, resp_cat, village_code, simulation, decline_rate, propdecl_pollen, starts_with("propdecl_")) %>%
  pivot_longer(
    cols = starts_with("propdecl_") & !starts_with("propdecl_pollen"),  # exclude pollen
    names_to = "nutrient",
    values_to = "decline_value") %>%
  mutate(
    nutrient = gsub("propdecl_", "", nutrient)) 


resp_id_summary <- decline_results_indiv_long %>%
  group_by(resp_id, resp_cat, village_code, decline_rate, nutrient) %>%
  summarise(propdecl_pollen = mean(propdecl_pollen, na.rm = TRUE),
            decline_value = mean(decline_value, na.rm = TRUE), .groups = "drop")

resp_cat_summary <- resp_id_summary %>%
  group_by(resp_cat, decline_rate, nutrient) %>%
  summarise(propdecl_pollen = mean(propdecl_pollen, na.rm = TRUE),
            decline_value = mean(decline_value, na.rm = TRUE),
            lower_CI = quantile(decline_value, 0.025),
            upper_CI = quantile(decline_value, 0.975),
            .groups = "drop")

village_summary <- resp_id_summary %>%
  group_by(village_code, decline_rate, nutrient) %>%
  summarise(propdecl_pollen = mean(propdecl_pollen, na.rm = TRUE),
            decline_value = mean(decline_value, na.rm = TRUE),
            lower_CI = quantile(decline_value, 0.025),
            upper_CI = quantile(decline_value, 0.975),
            .groups = "drop")

#Define which nutrients we want to plot
nutrients_to_plot <- c("VitA", "Folate", "Iron", "Calcium", "VitC", "VitE")

nutrient_labels <- c(
  "VitA" = "Vitamin A",
  "Folate" = "Folate",
  "Iron" = "Iron",
  "Calcium" = "Calcium",
  "VitC" = "Vitamin C",
  "VitE" = "Vitamin E"
)

#Filter to only nutrients of interest
resp_cat_filtered <- resp_cat_summary %>%
  filter(nutrient %in% nutrients_to_plot) %>%
  mutate(nutrient = factor(nutrient, levels = nutrients_to_plot))

#Filter to only nutrients of interest
village_filtered <- village_summary %>%
  filter(nutrient %in% nutrients_to_plot) %>%
  mutate(nutrient = factor(nutrient, levels = nutrients_to_plot))

##################################################################################
###############     Plot results by resp_cat   ###################################
##################################################################################

# Define color scheme and labels
resp_cat_colors <- c("adol_fem" = "lightblue",
  "adult_fem" = "#F29472",
  "adult_male" = "#5F9595",
  "u5_child" = "#F0BC68")

custom_labels <- c("adol_fem" = "Adolescent girls",
  "adult_fem" = "Adult women",
  "adult_male" = "Adult men",
  "u5_child" = "Children under-five")


# Plot resp cat data
poll_decline_by_resp_cat <- ggplot(resp_cat_filtered, aes(x = propdecl_pollen, y = -decline_value, color = resp_cat, fill = resp_cat)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ nutrient, labeller = labeller(nutrient = nutrient_labels)) +
  labs(
    x = "Pollination Service Decline (%)",
    y = "Decline in Nutrient Intake (%)",
    color = "Respondent Category",
    fill = "Respondent Category"
  ) +
  scale_color_manual(values = resp_cat_colors, labels = custom_labels) +
  scale_fill_manual(values = resp_cat_colors, labels = custom_labels) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0)) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1)

#Save plot
ggsave(plot=poll_decline_by_resp_cat, filename="plots/Poll_decline_resp_cat_level.svg", width=9, height=5, dpi=600, bg="white")
ggsave(plot=poll_decline_by_resp_cat, filename="plots/Poll_decline_resp_cat_level.png", width=9, height=5, dpi=600, bg="white")


##################################################################################
###############     Plot results by village   ###################################
##################################################################################


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

poll_decline_by_village <- ggplot(village_filtered, aes(x = propdecl_pollen, y = -decline_value, color = village_code, fill = village_code)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ nutrient, labeller = labeller(nutrient = nutrient_labels)) +
  labs(
    x = "Pollination Service Decline (%)",
    y = "Decline in Nutrient Intake (%)",
    color = "Village code",
    fill = "Village code"
  ) +
  scale_color_manual(values = village_colors) +
  scale_fill_manual(values = village_colors) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.3, 0)) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1)

#Save plot
ggsave(plot=poll_decline_by_village, filename="plots/Poll_decline_village_level.svg", width=9, height=5, dpi=600, bg="white")
ggsave(plot=poll_decline_by_village, filename="plots/Poll_decline_village_level.png", width=9, height=5, dpi=600, bg="white")



############################################################################################
#######  Remove pollinator-dependent component of intake species by species    #######
############################################################################################

# Get all unique OTUs
otus <- unique(intake_pollination_data$insect_OTU)

# Initialize results storage
species_decline_results_list <- list()  # for population-level
individual_decline_results_list <- list()  # for individual-level

iteration_counter <- 0
total_iterations <- length(otus)


# Loop over each OTU and simulate its removal
for (otu in otus) {
  
  iteration_counter <- iteration_counter + 1
  cat(sprintf("Progress: %3.1f%% — Removing OTU: %s | Time: %s\n",
              100 * iteration_counter / total_iterations,
              otu, format(Sys.time(), "%H:%M:%S")))
  
  # Set proportion_pollen to zero for that OTU
  intake_pollination_data_decl <- intake_pollination_data %>%
    mutate(species_decline = if_else(insect_OTU == otu, 1, 0))
  
glimpse(intake_pollination_data_decl)
  
  poll_change_by_sp <- intake_pollination_data_decl %>%
    dplyr::mutate(int_Energy_poll_change = int_Energy_poll * species_decline,
                  int_Fat_poll_change = int_Fat_poll * species_decline,
                  int_Protein_poll_change = int_Protein_poll * species_decline,
                  int_Calcium_poll_change = int_Calcium_poll * species_decline,
                  int_Iron_poll_change = int_Iron_poll * species_decline,
                  int_Zinc_poll_change = int_Zinc_poll * species_decline,
                  int_VitaminC_poll_change = int_VitaminC_poll * species_decline,
                  int_FolateTotal_poll_change = int_FolateTotal_poll * species_decline,
                  int_VitARE_poll_change = int_VitARE_poll * species_decline,
                  int_VitaminE_poll_change = int_VitaminE_poll * species_decline,
                  int_ThiaminB1_poll_change = int_ThiaminB1_poll * species_decline,
                  int_RiboflavinB2_poll_change = int_RiboflavinB2_poll * species_decline,
                  int_NiacinB3_poll_change = int_NiacinB3_poll * species_decline,
                  int_VitaminB6_poll_change = int_VitaminB6_poll * species_decline,
                  int_VitaminB12_poll_change = int_VitaminB12_poll * species_decline,
                  pollen_loss = proportion_pollen * species_decline)
   
  #Sum nutrient declines by ingredients so that the decline of all pollinator species is taken into account
  poll_change_by_ingred <- poll_change_by_sp %>%
    dplyr::group_by(formid,resp_id, resp_cat, village_code, sci_name,                              
                    final_poll_dependence, ingredient_grams_consumed, int_Energy_Kcal, int_Fat_g, int_Protein_g,               
                    int_Calcium_mg, int_Iron_mg, int_Zinc_mg, int_VitaminC_mg, int_ThiaminB1_mg,         
                    int_RiboflavinB2_mg, int_NiacinB3_mg, int_VitaminB6pyridoxine_mg, int_FolateTotal_µg, int_VitARE_µg,              
                    int_VitaminE_mg, int_VitaminB12_µg ) %>%
    dplyr::summarise(int_Energy_poll_change = sum(int_Energy_poll_change),
                     int_Fat_poll_change = sum(int_Fat_poll_change),
                     int_Protein_poll_change = sum(int_Protein_poll_change),
                     int_Calcium_poll_change = sum(int_Calcium_poll_change),
                     int_Iron_poll_change = sum(int_Iron_poll_change),
                     int_Zinc_poll_change = sum(int_Zinc_poll_change),
                     int_VitaminC_poll_change = sum(int_VitaminC_poll_change),
                     int_FolateTotal_poll_change = sum(int_FolateTotal_poll_change),
                     int_VitARE_poll_change = sum(int_VitARE_poll_change),
                     int_VitaminE_poll_change = sum(int_VitaminE_poll_change),
                     int_ThiaminB1_poll_change = sum(int_ThiaminB1_poll_change),
                     int_RiboflavinB2_poll_change = sum(int_RiboflavinB2_poll_change),
                     int_NiacinB3_poll_change = sum(int_NiacinB3_poll_change),
                     int_VitaminB6_poll_change = sum(int_VitaminB6_poll_change),
                     int_VitaminB12_poll_change = sum(int_VitaminB12_poll_change),
                     int_proportion_pollen = sum(proportion_pollen),
                     int_pollen_loss = sum(pollen_loss))
  
  
  #Calculate total decline in daily intake of each nutrient after pollinator declines for each participant during each recall event
  new_daily_intakes <- poll_change_by_ingred %>%
    dplyr::group_by(formid, resp_id, resp_cat, village_code) %>%
    dplyr::summarise(
      dplyr::across(starts_with("int_"), 
                    ~sum(.x, na.rm = TRUE)),
      .groups = "drop")
  
  ######################################################################################
  ########## Implement replacement of missing food items based on calorific replacement with plain white rice
  ######################################################################################
  
  #Calculate the loss of calories resulting from pollinator decline
  new_daily_intakes <- new_daily_intakes %>%
    dplyr::mutate(kcal_loss = int_Energy_poll_change)
  
  #Calculate grams of rice this translates to in units of 100 grams (plain white rice has 356.358kcal per 100 gram). This tells us how much much rice we need to replace in the diet in units of 100 grams
  #The reason for keeping the units as 100 grams is it allows to simply multiply the densisites of each nutrient per hundred grams in the next step, rather than having to convert them to nutrient/gram
  new_daily_intakes <- new_daily_intakes %>%
    mutate(additional_rice_poll_decl = kcal_loss/356.358)
  
  
  #Add extra nutrients provided by rice - we now calculate the additional nutrients that people obtain by eating more rice and remove this from the quantity that is expected to be lost as a result of pollinator decline
  new_daily_intakes_with_replacement <- new_daily_intakes %>%
    mutate(int_Energy_poll_change = int_Energy_poll_change - (additional_rice_poll_decl*356.358),  #Energy  value from FCT for 'Rice, Fine grain, Basmati'
           int_Fat_poll_change = int_Fat_poll_change - (additional_rice_poll_decl*0.52),  #Fat  value from FCT for 'Rice, Fine grain, Basmati'
           int_Protein_poll_change = int_Protein_poll_change - (additional_rice_poll_decl*7.94),  #Protein  value from FCT for 'Rice, Fine grain, Basmati'
           int_Calcium_poll_change = int_Calcium_poll_change - (additional_rice_poll_decl*7.49),  #Calcium  value from FCT for 'Rice, Fine grain, Basmati'
           int_Iron_poll_change = int_Iron_poll_change - (additional_rice_poll_decl*0.65),  #Iron  value from FCT for 'Rice, Fine grain, Basmati'
           int_Zinc_poll_change = int_Zinc_poll_change - (additional_rice_poll_decl*1.21),  #Zinc  value from FCT for 'Rice, Fine grain, Basmati'
           int_VitaminC_poll_change = int_VitaminC_poll_change - (additional_rice_poll_decl*0),  #VitaminC  value from FCT for 'Rice, Fine grain, Basmati'
           int_ThiaminB1_poll_change = int_ThiaminB1_poll_change - (additional_rice_poll_decl*0.05),  #ThiaminB1  value from FCT for 'Rice, Fine grain, Basmati'
           int_RiboflavinB2_poll_change = int_RiboflavinB2_poll_change - (additional_rice_poll_decl*0.05),  #RiboflavinB2  value from FCT for 'Rice, Fine grain, Basmati'
           int_NiacinB3_poll_change = int_NiacinB3_poll_change - (additional_rice_poll_decl*1.69),  #NiacinB3  value from FCT for 'Rice, Fine grain, Basmati'
           int_VitaminB6_poll_change = int_VitaminB6_poll_change - (additional_rice_poll_decl*0.12),  #VitaminB6pyridoxine  value from FCT for 'Rice, Fine grain, Basmati'
           int_FolateTotal_poll_change = int_FolateTotal_poll_change - (additional_rice_poll_decl*9.32),  #FolateTotal  value from FCT for 'Rice, Fine grain, Basmati'
           int_VitARE_poll_change = int_VitARE_poll_change - (additional_rice_poll_decl*0),  #VitARE  value from FCT for 'Rice, Fine grain, Basmati'
           int_VitaminE_poll_change = int_VitaminE_poll_change - (additional_rice_poll_decl*0.06),  #VitaminE  value from FCT for 'Rice, Fine grain, Basmati'
           int_VitaminB12_poll_change = int_VitaminB12_poll_change - (additional_rice_poll_decl*0))  #VitaminB12  value from FCT for 'Rice, Fine grain, Basmati'
  
  
  #Calculate mean daily intake of each nutrient after pollinator declines for each participant during the course of the year
  new_daily_intakes_mean <- new_daily_intakes_with_replacement %>%
    dplyr::group_by(resp_id, resp_cat, village_code) %>%
    dplyr::summarise(
      dplyr::across(starts_with("int_"), 
                    ~mean(.x, na.rm = TRUE)),
      .groups = "drop")
  
  
  #Calculate percentage change in daily intake for each participant in each nutrient
  #Also calculate percentage change in pollen transport as a result of decline
  new_daily_intakes_mean <- new_daily_intakes_mean %>%
    dplyr::mutate(propdecl_Energy = int_Energy_poll_change / int_Energy_Kcal,
                  propdecl_Fat = int_Fat_poll_change / int_Fat_g,
                  propdecl_Protein = int_Protein_poll_change / int_Protein_g,
                  propdecl_Calcium = int_Calcium_poll_change / int_Calcium_mg,
                  propdecl_Iron = int_Iron_poll_change / int_Iron_mg,
                  propdecl_Zinc = int_Zinc_poll_change / int_Zinc_mg,
                  propdecl_VitC = int_VitaminC_poll_change / int_VitaminC_mg,
                  propdecl_VitB1 = int_ThiaminB1_poll_change / int_ThiaminB1_mg,
                  propdecl_VitB2 = int_RiboflavinB2_poll_change / int_RiboflavinB2_mg,
                  propdecl_VitB3 = int_NiacinB3_poll_change / int_NiacinB3_mg,
                  propdecl_VitB6 = int_VitaminB6_poll_change / int_VitaminB6pyridoxine_mg,
                  propdecl_Folate = int_FolateTotal_poll_change / int_FolateTotal_µg,
                  propdecl_VitA = int_VitARE_poll_change / int_VitARE_µg,
                  propdecl_VitE = int_VitaminE_poll_change / int_VitaminE_mg,
                  propdecl_VitB12 = int_VitaminB12_poll_change / int_VitaminB12_µg,
                  propdecl_pollen = int_pollen_loss/ int_proportion_pollen)
  
  
  new_daily_intakes_mean <- dplyr::mutate_all(new_daily_intakes_mean, ~replace_na(.x, 0))
  

    # Store results with identifier
  new_daily_intakes_mean$removed_OTU <- otu
  individual_decline_results_list[[otu]] <- new_daily_intakes_mean  
  
  # Population-level mean decline + 95% confidence intervals
  decline_summary <- new_daily_intakes_mean %>%
    dplyr::summarise(
      dplyr::across(starts_with("propdecl_"),
        list(mean = ~mean(.x, na.rm = TRUE),
          lower95 = ~mean(.x, na.rm = TRUE) - 1.96 * sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
          upper95 = ~mean(.x, na.rm = TRUE) + 1.96 * sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
        .names = "{.col}_{.fn}")) %>%
    mutate(removed_OTU = otu)
      
  
  species_decline_results_list[[otu]] <- decline_summary
}

# Combine results
species_decline_results_population <- bind_rows(species_decline_results_list)
individual_decline_results_all <- bind_rows(individual_decline_results_list)

#Remove 95%CI columns as they aren't useful in this context
species_decline_results_population <- species_decline_results_population %>%
  select( !contains("95") )

# Export both datasets
write.csv(species_decline_results_population, "output_data/OTU_Removal_Results_Population.csv", row.names = FALSE)
write.csv(individual_decline_results_all, "output_data/OTU_Removal_Results_Individual.csv", row.names = FALSE)

#####################################################################
#### Plot cumulative loss of each nutrient as species are lost in order of most to least significant
#####################################################################

#Import modelling results if not already present in the environment
#species_decline_results_population <- read.csv("output_data/OTU_Removal_Results_Population.csv")

#Redistribute values for 'Other' pollinators in proportion to the value for each known insect OTU

# Get the names of all nutrient columns
nutrient_cols <- names(species_decline_results_population)[grepl("^propdecl_.*_mean$", names(species_decline_results_population))]

# Make a copy to preserve original
species_decline_results_redistributed <- species_decline_results_population

# Loop over each nutrient column
for (col in nutrient_cols) {
  
  # Extract the value from the "Other" OTU
  value_to_redistribute <- species_decline_results_redistributed %>%
    filter(removed_OTU == "Other") %>%
    pull(!!sym(col))
  
  # Total of all other OTUs for this nutrient
  total_other_otus <- species_decline_results_redistributed %>%
    filter(removed_OTU != "Other") %>%
    summarise(total = sum(!!sym(col), na.rm = TRUE)) %>%
    pull(total)
  
  # If total is > 0, perform redistribution
  if (total_other_otus != 0) {
    species_decline_results_redistributed <- species_decline_results_redistributed %>%
      mutate(proportion = if_else(removed_OTU != "Other",!!sym(col) / total_other_otus,0),
        redistribution = proportion * value_to_redistribute,
        !!sym(col) := if_else(removed_OTU != "Other",!!sym(col) + redistribution,0)) %>%
    
      select(-proportion, -redistribution)
  } else {
    warning(paste("Skipping", col, "– sum of other OTUs is zero."))
  }
}


# Define nutrients to include
nutrients_to_plot <- c("VitA", "Folate", "Iron", "Calcium", "VitC", "VitE")

# Pivot your data to long format
species_decline_results_long <- species_decline_results_redistributed %>%
  pivot_longer(cols = starts_with("propdecl_") & ends_with("_mean") & !contains("pollen"),
               names_to = "nutrient",
               values_to = "decline") %>%
  mutate(nutrient = str_replace(nutrient, "propdecl_", ""),  # Clean names
         nutrient = str_replace(nutrient, "_mean", "")) %>%
  filter(nutrient %in% nutrients_to_plot) %>%
  mutate(removed_OTU = str_replace_all(removed_OTU, "_", " "))


# Function to generate plot per nutrient
plot_nutrient <- function(nutrient_name) {
  df <- species_decline_results_long %>%
    filter(nutrient == nutrient_name) %>%
    slice_max(order_by = abs(decline), n = 20, with_ties = FALSE) %>%
    arrange(desc(decline)) %>%
    mutate(cumulative_decline = cumsum(decline),
      otu_label = factor(removed_OTU, levels = removed_OTU))
      
  # Add a "start" row at cumulative_decline = 0
  df <- bind_rows(tibble(nutrient = nutrient_name,
      removed_OTU = " ",
      decline = 0,
      cumulative_decline = 0,
      otu_label = factor(" ", levels = c(" ", levels(df$otu_label)))
      ),
        df
    ) %>%
    mutate(otu_label = factor(otu_label, levels = unique(otu_label)))  # Preserve order incl. "Start")
      
    ggplot(df, aes(x = otu_label, y = cumulative_decline)) +
    geom_line(group = 1) +
    geom_point() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +  # Convert to %
    labs(x = "Insect taxon removed",
      y = "Decline in nutrient intake (cumulative)",
      title = nutrient_name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face= "italic"))
   
}

# Generate one plot per nutrient
plots <- map(nutrients_to_plot, plot_nutrient)

# Combine with patchwork
all_nutrients_by_species <- wrap_plots(plots, ncol = 2) 

#Save plot
ggsave(plot=all_nutrients_by_species, filename="plots/Poll_decline_by_species.svg", width=8, height=10, dpi=600, bg="white")
ggsave(plot=all_nutrients_by_species, filename="plots/Poll_decline_by_species.png", width=8, height=10, dpi=600, bg="white")



##############################################################################################################################################################################
#########################################################          END OF SCRIPT 03a    #######################################################################################
##############################################################################################################################################################################


