##################################################################################################################################################################
##############   MICRO-POLL SCRIPT 6 - SUMMARISING BASIC INFORMATION ABOUT THE POPULATION TO PROVIDE SOCIOECONOMIC CONTEXT ##############################################
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
library(viridis)
library(truncnorm)

##Import household registration info
registration_data <- read.csv("input_data/hh_enrolment_all_data.csv")

##Import household census data
census_data <- data.table(read_excel("input_data/household_census_all.xlsx",
                                          sheet = "census_data_HH", 
                                          na = c("", "---", NA))) 

##Import farmer questionnaire data for landholding and economic information
farmer_data <- data.table(read_excel("input_data/MP_farmer_questionnaire.xlsx",
                                     sheet = "Farmer questionnaire", 
                                     na = c("", "---", NA)))  

##Import FAO and Ellis et al data for comparison plots
comparison_data <- data.table(read_excel("input_data/smallholder_global_comparisons_data.xlsx",
                                     sheet = "all_comparisons", 
                                     na = c("", "---", NA)))  


#Rename HH id column to match census data
registration_data <- registration_data %>%
  dplyr::rename(HH_ID = hhid_census)

#Merge census data in with household registration data
household_data <- merge(x=registration_data, y=census_data, by= "HH_ID",all.x=TRUE)

#Merge farming information into hh info based on hh_barcode
household_farming_data <- merge(x=farmer_data, y=household_data, by= "hh_barcode",all.x=TRUE)


######################################################################################
########## Summarise socioeconomic features of the study population
######################################################################################

#Create new variable which calculates the number of different crops grown by each household
crop_list <- c("grow_almonds", "grow_apple", "grow_apricot", "grow_asparagus", "grow_aubergine", 
               "grow_barley", "grow_beetroot", "grow_bittergourd", "grow_blood_amaranth", "grow_broad_bean", 
               "grow_buckwheat", "grow_cabbage", "grow_cannabis", "grow_carrot", "grow_cauliflower", 
               "grow_chard", "grow_chilli", "grow_coriander", "grow_cowpea", "grow_cress", 
               "grow_cucumber", "grow_daikon", "grow_dill", "grow_fennel", "grow_fenugreek", 
               "grow_field_pea", "grow_finger_milllet", "grow_fox_amaranth", "grow_foxtail_millet", 
               "grow_garden_pea", "grow_garlic", "grow_green_amaranth", "grow_green_bean", "grow_horse_gram", 
               "grow_jumli_bean", "grow_maize", "grow_mustard_leaf", "grow_mustard_seed", "grow_naked_barley", 
               "grow_onion", "grow_peach", "grow_pear", "grow_perilla", "grow_plum", 
               "grow_potato", "grow_prince_feather", "grow_proso_millet", "grow_pumpkin", "grow_radish", 
               "grow_rice", "grow_saffron", "grow_scarlet_bean", "grow_slipper_gourd", "grow_sorghum", 
               "grow_soybean", "grow_spinach", "grow_sunflower", "grow_taro", "grow_tartary_buckwheat", 
               "grow_tomato", "grow_tree_tomato", "grow_turnip", "grow_walnut", "grow_wheat")

#Calculate total crops grown by each household
household_farming_data <- household_farming_data %>%
  mutate(total_crops = rowSums(select(., all_of(crop_list)), na.rm = TRUE))

#Create new variable which reports the number of years of schooling by the household head
household_farming_data <- household_farming_data %>%
  mutate(hh_head_school_years = recode(highest_grade,
                               "grade 1" = 1,
                               "grade 2" = 2,
                               "grade 3" = 3,
                               "grade 4" = 4,
                               "grade 5" = 5,
                               "grade 6" = 6,
                               "grade 7" = 7,
                               "grade 8" = 8,
                               "grade 9" = 9,
                               "plus 2 pass" = 12,
                               "SLC pass" = 10,
                               .default = NA_real_)) %>%
  mutate(hh_head_school_years = if_else(is.na(hh_head_school_years), 0, hh_head_school_years))  # Replace NA with 0 as this means no schooling in this case

#Change income of households which don't sell any produce to NA so that this doesn't affect the calculation of mean farming income
household_farming_data <- household_farming_data %>%
  mutate(farming_income = if_else(farming_income == 0, NA_real_, farming_income))

#Calculate agricultural income per hectare
household_farming_data <- household_farming_data %>%
  mutate(farming_income_per_ha = farming_income/(farm_sq_metre * 0.0001))

#Calculate mean +- standard deviation of key features of study population

population_summary <- household_farming_data %>%
  summarise(hh_size_median = median(hh_size, na.rm = TRUE),
            hh_size_mean = mean(hh_size, na.rm = TRUE),
            hh_size_sd = sd(hh_size, na.rm = TRUE),
            collect_water_time_median = median(collect_water_time, na.rm = TRUE),
            collect_water_time_mean = mean(collect_water_time, na.rm = TRUE),
            collect_water_time_sd = sd(collect_water_time, na.rm = TRUE),
            room_number_median = median(room_number, na.rm = TRUE),
            room_number_mean = mean(room_number, na.rm = TRUE),
            room_number_sd = sd(room_number, na.rm = TRUE),
            prop_income_agriculture = sum(hh_income %in% c("Selling own livestock (meat & poultry)", "Selling own crop production"), na.rm = TRUE) / sum(!is.na(hh_income)),
            lead_farmer_age_median = median(respondent_age, na.rm = TRUE),
            lead_farmer_age_mean = mean(respondent_age, na.rm = TRUE),
            lead_farmer_age_sd = sd(respondent_age, na.rm = TRUE),
            lead_farmer_female = sum(gender == "Female", na.rm = TRUE)/ sum(!is.na(gender)),
            hh_head_school_years_median = median(hh_head_school_years, na.rm = TRUE),
            hh_head_school_years_mean = mean(hh_head_school_years, na.rm = TRUE),
            hh_head_school_years_sd = sd(hh_head_school_years, na.rm = TRUE),
            iliteracy_rate = sum(literacy == "Cannot read", na.rm = TRUE) / sum(!is.na(literacy)),
            land_ownership_ha_median = median(own_sq_metre * 0.0001, na.rm = TRUE),
            land_ownership_ha_mean = mean(own_sq_metre * 0.0001, na.rm = TRUE),
            land_ownership_ha_sd = sd(own_sq_metre * 0.0001, na.rm = TRUE),
            land_farmed_ha_median = median(farm_sq_metre * 0.0001, na.rm = TRUE),
            land_farmed_ha_mean = mean(farm_sq_metre * 0.0001, na.rm = TRUE),
            land_farmed_ha_sd = sd(farm_sq_metre * 0.0001, na.rm = TRUE),
            farming_purpose_feed_fam = sum(farming_purpose %in% c("feed family", "feed fam & sell some"), na.rm = TRUE) / sum(!is.na(farming_purpose)),
            farming_income_usd_median = median((farming_income_per_ha*0.0072), na.rm = TRUE),
            farming_income_usd_mean = mean((farming_income_per_ha*0.0072), na.rm = TRUE),
            farming_income_usd_sd = sd((farming_income_per_ha*0.0072), na.rm = TRUE),
            farming_income_per_ha_usd_mean = mean((farming_income_per_ha*0.0072), na.rm = TRUE), #Nepalee rupee to us dollar exchange rate Jan 2025
            total_crops_median = median(total_crops, na.rm = TRUE),
            total_crops_mean = mean(total_crops, na.rm = TRUE),
            total_crops_sd = sd(total_crops, na.rm = TRUE),
            knowledge_of_pollination = sum(pollination_understanding %in% c("basic understanding", "good understanding"), na.rm = TRUE) / sum(!is.na(pollination_understanding)))

#Restructure the data

population_summary_long <- population_summary %>%
  pivot_longer(cols = everything(),  # Pivot all columns
               names_to = "metric",  # The column names will be moved into a new column called 'metric'
               values_to = "value")  # The values will go into a new column called 'value'


#Save summary data
write.csv(population_summary_long, "output_data/study_population_summary.csv")

#######################################################################################################################
#############   Plot comparisons of our data with smallholder data from the world (FAO smallholder profile) ###########
#######################################################################################################################

comparison_data_long <- comparison_data %>%
  gather(key = "variable", value = "value", -country, -region) %>%
  filter(!is.na(value))

# Plot for land_size_ha_mean
plot_land_size <- ggplot(comparison_data_long %>% filter(variable == 'land_size_ha_mean'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = 'Mean landholding size (ha)', title = 'A') +
  scale_fill_manual(values = c('gray', 'red')) + # Highlight 'Jumla' with red
  theme_minimal()

# Plot for prop_income_agriculture
plot_income_agriculture <- ggplot(comparison_data_long %>% filter(variable == 'prop_income_agriculture'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% HH income from agriculture', title = 'B') +
  scale_fill_manual(values = c('gray', 'red')) + # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for farming_income_per_ha_usd_mean
plot_farming_income <- ggplot(comparison_data_long %>% filter(variable == 'farming_income_per_ha_usd_mean'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = 'Farming income per ha (USD)', title = 'C') +
  scale_fill_manual(values = c('gray', 'red')) + # Highlight 'Jumla' with red
  theme_minimal()

# Plot for hh_head_school_years_mean
plot_school_years <- ggplot(comparison_data_long %>% filter(variable == 'hh_head_school_years_mean'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = 'Mean years of schooling for HH head', title = 'D') +
  scale_fill_manual(values = c('gray', 'red')) + # Highlight 'Jumla' with red
  theme_minimal()

# Plot for hh_size_mean
plot_hh_size <- ggplot(comparison_data_long %>% filter(variable == 'hh_size_mean'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = 'Mean HH size', title = 'E') +
  scale_fill_manual(values = c('gray', 'red')) + # Highlight 'Jumla' with red
  theme_minimal()

# Combine the plots into a single grid
combined_smallholder_plot <- grid.arrange(plot_land_size, plot_income_agriculture, plot_farming_income, 
             plot_school_years, plot_hh_size, ncol = 2)

#Print the plot
ggsave(plot=combined_smallholder_plot, filename="plots/Comparing_smallholders_by_country.svg", width=7, height=9, dpi=600, bg="white")
ggsave(plot=combined_smallholder_plot, filename="plots/Comparing_smallholders_by_country.png", width=7, height=9, dpi=600, bg="white")

#######################################################################################################################
#############     Plot comparisons of our data with Ellis et al 2015 data on poll-dependence of diets     ###########
#######################################################################################################################

#Filter data to only that with values from Ellis et al 2015
poll_dependence_data <- comparison_data_long %>%
  filter(country %in% c("Jumla", "Mozambique", "Zambia", "Uganda", "Bangladesh"))

# Plot for Energy_prop_poll_depend
plot_energy_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Energy_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Energy') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for Vit_A_prop_poll_depend
plot_vit_a_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Vit_A_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Vitamin A') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for Calcium_prop_poll_depend
plot_calcium_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Calcium_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Calcium') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for Folate_prop_poll_depend
plot_folate_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Folate_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Folate') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for Iron_prop_poll_depend
plot_iron_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Iron_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Iron') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Plot for Zinc_prop_poll_depend
plot_zinc_poll_depend <- ggplot(poll_dependence_data %>% filter(variable == 'Zinc_prop_poll_depend'), aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', aes(fill = country == 'Jumla'), show.legend = FALSE) +
  coord_flip() +
  labs(x = '', y = '% from poll-dependent crops', title = 'Zinc') +
  scale_fill_manual(values = c('gray', 'red')) +  # Highlight 'Jumla' with red
  scale_y_continuous(limits = c(0, 1), labels = label_percent(scale = 100)) +  # Convert y-axis to % 
  theme_minimal()

# Combine the plots into a single grid
combined_micronutrient_plot <- grid.arrange(plot_vit_a_poll_depend, plot_folate_poll_depend, 
                                            plot_calcium_poll_depend, plot_iron_poll_depend, plot_zinc_poll_depend, plot_energy_poll_depend, ncol = 2)

#Print the plot
ggsave(plot=combined_micronutrient_plot, filename="plots/Comparing_poll_dependence_by_country.svg",  width=7, height=8,  dpi=600, bg="white")
ggsave(plot=combined_micronutrient_plot, filename="plots/Comparing_poll_dependence_by_country.png",  width=7, height=8,  dpi=600, bg="white")


######################################################################
##### Calculating proportion of crop production that is consumed  #######
######################################################################



#Convert range of consumption proportion to numeric values with normal distribution
convert_range_to_random <- function(x) {
  x <- as.character(x)
  sapply(x, function(val) {
    if (is.na(val)) return(NA_real_)
      # Define bounds and defaults
    if (val == "0-10%") {
      rtruncnorm(1, a = 0.00, b = 0.10, mean = 0.05, sd = 0.02)
    } else if (val == "10-50%") {
      rtruncnorm(1, a = 0.10, b = 0.50, mean = 0.30, sd = 0.10)
    } else if (val == "50-80%") {
      rtruncnorm(1, a = 0.50, b = 0.80, mean = 0.65, sd = 0.07)
    } else if (val == ">80%") {
      rtruncnorm(1, a = 0.80, b = 1.00, mean = 0.90, sd = 0.05)
    } else {
      NA_real_
    }
  })
}

# Apply to selected columns
farmer_data <- farmer_data %>%
  dplyr::mutate(across(c(apple_consume_proportion, mustard_consume_proportion, karela_consume_proportion, 
                         pumpkin_consume_proportion, buckwheat_consume_proportion, bean_consume_proportion),
                       convert_range_to_random,
                       .names = "{.col}_numeric"
  ))




convert_range_to_midpoint <- function(x) {
  x <- as.character(x)
  case_when(x == "0-10%"   ~ 0.05,
            x == "10-50%"  ~ 0.30,
            x == "50-80%"  ~ 0.65,
            x == ">80%"    ~ 0.90,
            TRUE ~ NA_real_
  )
}

# Apply to selected columns
farmer_data <- farmer_data %>%
  dplyr::mutate(across(c(apple_consume_proportion, mustard_consume_proportion, karela_consume_proportion, 
                         pumpkin_consume_proportion, buckwheat_consume_proportion, bean_consume_proportion),
                       convert_range_to_midpoint,
                       .names = "{.col}_numeric"
  ))


own_consumption_data <- select(farmer_data_scaled, village_code, hh_barcode,apple_consume_proportion_numeric, mustard_consume_proportion_numeric, karela_consume_proportion_numeric, 
                               pumpkin_consume_proportion_numeric, bean_consume_proportion_numeric)


# Convert to long format
own_consumption_data_long <- own_consumption_data %>%
  pivot_longer(cols = ends_with("_consume_proportion_numeric"),
    names_to = "crop",
    names_pattern = "(.*)_consume_proportion_numeric",
    values_to = "prop_own_consumption")

#Summarise mean and standard deviation by crop
summary_stats <- own_consumption_data_long %>%
  group_by(crop) %>%
  summarise(mean_prop = mean(prop_own_consumption, na.rm = TRUE),
    sd_prop = sd(prop_own_consumption, na.rm = TRUE),
    .groups = "drop")
  

# Box and whisker plot
own_production_consumed <- ggplot(own_consumption_data_long, aes(x = crop, y = prop_own_consumption)) +
  geom_boxplot() +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),  # <- add this line
    labels = label_percent(accuracy = 1)
  ) +
  labs(title = "",
       x = "Crop",
       y = "Proportion of own production consumed (%)") +
  theme_minimal()
  
#Print the plot
ggsave(plot=own_production_consumed, filename="plots/Proportion_own_production_consumed.svg",  width=5, height=5,  dpi=600, bg="white")
ggsave(plot=own_production_consumed, filename="plots/Proportion_own_production_consumed.png",  width=5, height=5,  dpi=600, bg="white")


##############################################################################################################################################################################
#########################################################          END OF SCRIPT 06    #######################################################################################
##############################################################################################################################################################################


