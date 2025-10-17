##################################################################################################################################################################
######################################    MICRO-POLL SCRIPT 4 - CALCULATING CROP-POLLINATOR NETWORK METRICS  #####################################################
##################################################################################################################################################################

#The purpose of this script is to calculate species-level network metrics (of pollinator taxa) and test their ability to predict nutritional outcomes when pollinator taxa are removed.
#This analysis relates to manuscript Question 3: How does crop-pollinator network structure influence nutritional outcomes?

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
library(car)         # for outlierTest
library(broom)       # for tidy residuals
library(patchwork)   # for side-by-side plots
library(robustbase)  # For robust regressions
library(scales)



#Import plant-pollinator interaction data from main "input_data" folder
plant_poll_data <- data.table(read_excel("input_data/MP_pollinator_visitation.xlsx", 
                                         sheet = "Visitation data"))

#Import results of species decline modelling
species_decline_results_population <- read.csv("output_data/OTU_Removal_Results_Population.csv")

#######################################################################
#######           Filtering interactions to crop plants only          #######
#######################################################################

crop_poll_data <- plant_poll_data %>%
  filter(plant_category == "crop")

crop_poll_data_simple <- select(crop_poll_data, plant_sci_name, insect_OTU)


interaction_matrix <- table(crop_poll_data_simple$plant_sci_name, crop_poll_data_simple$insect_OTU)

#Calculate network metrics for each insect_OTU
insect_metrics <- specieslevel(interaction_matrix, level = "higher")  # 'higher' = insect level

insect_metrics_df <- as.data.frame(insect_metrics)

insect_metrics_df$insect_OTU <- rownames(insect_metrics_df)



###################################################################################################################################
#####  Merge datasets and get into correct format  ############################################
####################################################################################################################################

#Rename column to match with network data
species_decline_results_population <- species_decline_results_population %>%
  dplyr::rename(insect_OTU = removed_OTU)

#Merge network metric data with results from species removal modelling
species_decline_network_role <- merge(x=insect_metrics_df, y=species_decline_results_population, by= "insect_OTU",all.x=TRUE)

#Remove 'other' as this is a meaningless OTU
species_decline_network_role <- species_decline_network_role %>%
  filter(insect_OTU != "Other")

#Remove irrelevant nutrients (i.e. not pollinator-dependent)
species_decline_network_role_subset <- select(species_decline_network_role, -propdecl_pollen_mean, -propdecl_VitB12_mean, -propdecl_Energy_mean, -propdecl_Fat_mean, -propdecl_Protein_mean,
                                       -propdecl_VitB1_mean, -propdecl_VitB2_mean, -propdecl_VitB3_mean, -propdecl_VitB6_mean)

species_decline_network_role_subset <- species_decline_network_role_subset %>%
  dplyr::mutate(mean_intake_decline = propdecl_Calcium_mean + propdecl_Iron_mean + propdecl_VitC_mean + propdecl_Folate_mean + propdecl_VitA_mean + propdecl_VitE_mean)

names(species_decline_network_role_subset)

#Convert to long format
species_long <- species_decline_network_role_subset %>%
  pivot_longer(cols = starts_with("propdecl_"),       
    names_to = "nutrient",                 
    values_to = "value"  )                  
  
 species_long <- species_long %>%
  mutate(nutrient = nutrient %>%
           str_remove("^propdecl_") %>%
           str_remove("_mean$"))   

# write.csv(species_decline_network_role_filtered, "output_data/Species_nutrient_declines_network_role.csv")
 
 ###################################################################################################################################
 #####  Investigate relationships between network metrics and nutritional importance    ############################################
 ####################################################################################################################################
 
#Remove species where intake decline is NA or 0
 species_decline_network_role_filtered <- species_decline_network_role_subset %>%
   filter(!is.na(mean_intake_decline) & mean_intake_decline > 0)

 species_decline_network_role_filtered$log_mean_intake_decline <- log(species_decline_network_role_filtered$mean_intake_decline)
 species_decline_network_role_filtered$sqrt_mean_intake_decline <- sqrt(species_decline_network_role_filtered$mean_intake_decline)
 
 hist(species_decline_network_role_filtered$sqrt_mean_intake_decline, breaks = 50)
 
 #List the network metrics I want to calculate
 metrics <- c("degree", "normalised.degree", "species.strength", "interaction.push.pull",
   "nestedrank", "PDI", "resource.range", "species.specificity.index", "PSI",
   "node.specialisation.index.NSI", "betweenness", "weighted.betweenness",
   "closeness", "weighted.closeness", "Fisher.alpha", "partner.diversity",
   "effective.partners", "proportional.generality", "proportional.similarity", "d")
 
  df_log <- species_decline_network_role_filtered %>%
   mutate(across(all_of(metrics), ~ log1p(.), .names = "log_{.col}"))
  
  log_metrics <- paste0("log_", metrics)
 
 #Fit a model for each metric and extract AIC and adjusted R²
 results_log <- lapply(log_metrics, function(metric) {
   formula <- as.formula(paste("log_mean_intake_decline ~", metric))
   model <- lm(formula, data = df_log)
   list(
     metric = metric,
     AIC = AIC(model),
     adj_r2 = summary(model)$adj.r.squared
   )
 })
 
 results_log_df <- do.call(rbind, lapply(results_log, as.data.frame)) %>%
   arrange(AIC)
 
 #####################################################
 ## Check model assumptions & combine all results into a table
 #####################################################
 
 results_detailed <- lapply(log_metrics, function(metric) {
   formula <- as.formula(paste("log_mean_intake_decline ~", metric))
   model <- lm(formula, data = df_log)
   tidy_model <- tidy(model)  # coefficient table
   glance_model <- glance(model)  # model stats
   
   # Assumption tests
   shapiro_p <- shapiro.test(resid(model))$p.value
   bp_p <- tryCatch(car::ncvTest(model)$p, error = function(e) NA)
   
   # Extract slope row only (term != intercept)
   slope_row <- tidy_model[tidy_model$term != "(Intercept)", ]
   
   data.frame(
     metric = metric,
     estimate = slope_row$estimate,
     std_error = slope_row$std.error,
     t_value = slope_row$statistic,
     p_value = slope_row$p.value,
     adj_r2 = glance_model$adj.r.squared,
     AIC = AIC(model),
     shapiro_p = shapiro_p,
     bp_p = bp_p,
     intercept = tidy_model$estimate[tidy_model$term == "(Intercept)"],
     n = nobs(model),
     assumption_ok = ifelse(shapiro_p > 0.05 & bp_p > 0.05, TRUE, FALSE)
   )
 })
 
 results_table <- do.call(rbind, results_detailed)
 
 results_table <- results_table %>%
   select(metric,              # Predictor name
     adj_r2,              # Adjusted R²
     AIC,                 # AIC
     estimate,            # Slope
     std_error,           # Standard error of slope
     t_value,             # t statistic
     p_value,             # p-value
     intercept,           # Model intercept
     shapiro_p,           # Residual normality p-value
     bp_p,                # Homoscedasticity p-value
     n)
     
 
 # Sort by AIC (ascending)
 results_table <- results_table %>%
   arrange(AIC)

 
 write.csv(results_table, "output_data/Species_network_metrics_model_summary.csv", row.names = FALSE)
 

 #####################################################
 ## Plot the best models
 #####################################################

 
p1 <- ggplot(species_decline_network_role_filtered, aes(x = degree, y = mean_intake_decline)) +
   geom_point(alpha = 0.7) +
   geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue", fill = "lightblue") +
   scale_x_log10() +
   scale_y_log10(labels = label_number(accuracy = 0.001), limits = c(1e-4, NA))+
   labs(
     x = "Degree (log scale)",
     y = "Proportional decline in nutrient intake (log scale)",
     title = "A. Species degree"
   ) +
   theme_bw()
 
 p2 <- ggplot(species_decline_network_role_filtered, aes(x = nestedrank, y = mean_intake_decline)) +
   geom_point(alpha = 0.7) +
   geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue", fill = "lightblue") +
   scale_x_log10() +
   scale_y_log10(labels = label_number(accuracy = 0.001), limits = c(1e-4, NA))+
   labs(
     x = "Nested rank (log scale)",
     y = "",
     title = "B. Nested rank"
   ) +
   theme_bw()
 
 p3 <- ggplot(species_decline_network_role_filtered, aes(x = weighted.closeness, y = mean_intake_decline)) +
   geom_point(alpha = 0.7) +
   geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue", fill = "lightblue") +
   scale_x_log10() +
   scale_y_log10(labels = label_number(accuracy = 0.001), limits = c(1e-4, NA))+
   labs(
     x = "Weighted closeness (log scale)",
     y = "Proportional decline in nutrient intake (log scale)",
     title = "C. Weighted closeness"
   ) +
   theme_bw()
 

 
#Save as multi-panel plot

 design <- "
AB
CD
"
 
 multi_panel_plot <- p1 + p2 + p3 + plot_spacer() +
   plot_layout(design = design)


#Save plot
ggsave(plot=multi_panel_plot, filename="plots/Network_role_nutrient_decline.svg", width=8, height=8, dpi=600, bg="white")
ggsave(plot=multi_panel_plot, filename="plots/Network_role_nutrient_decline.png", width=8, height=8, dpi=600, bg="white")

 
##############################################################################################################################################################################
#########################################################          END OF SCRIPT 04    #######################################################################################
##############################################################################################################################################################################
