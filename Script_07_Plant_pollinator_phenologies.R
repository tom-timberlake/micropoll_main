##################################################################################################################################################################
##############      MICRO-POLL SCRIPT 7 - INVESTIGATING PLANT AND POLLINATOR PHENOLOGY PATTERNS      ############################################
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
library(stringr)
library(ggridges)
library(patchwork)


#Import plant-pollinator interaction data from main "input_data" folder
plant_poll_data <- data.table(read_excel("input_data/MP_pollinator_visitation.xlsx", 
                                         sheet = "Visitation data"))



##################################################################################################################################################
################################   Comparing phenolgy patterns of crop visits vs wild plants    ############################################
##################################################################################################################################################


###################
### Apis spp.
###################

Apis_data <- plant_poll_data %>%
  # Keep only Apis
  filter(insect_genus == "Apis") %>%
  mutate(feeding = case_when(
    plant_eng_name == "Apple" ~ "Apple",
    plant_eng_name == "Broad leaf mustard" ~ "Mustard",
    plant_eng_name == "Jumli bean" ~ "Bean",
    plant_category == "wild" ~ "Wild plants",
    TRUE ~ NA_character_ ))  %>%
  filter(!is.na(feeding)) %>%
  mutate(survey_date = as.Date(survey_date),
         feeding = factor(feeding, levels = c("Bean", "Mustard", "Wild plants" , "Apple")))

min_d <- floor_date(min(Apis_data$survey_date, na.rm = TRUE), "month")
max_d <- ceiling_date(max(Apis_data$survey_date, na.rm = TRUE), "month")
month_seq <- seq(min_d, max_d, by = "1 month")

# Custom color palette
feeding_colors <- c("Apple" = "#8B0000",    
                    "Wild plants" = "#228B22",     
                    "Mustard" = "#EE8238", 
                    "Bean" = "#8B4513"  )  

apis_plot <- ggplot(Apis_data, aes(x = survey_date, y = feeding, fill = feeding)) +
  geom_vline(data = data.frame(x = month_seq),
             aes(xintercept = x), inherit.aes = FALSE,
             color = "grey80", linewidth = 0.5) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, show.legend = FALSE) +
    scale_fill_manual(values = feeding_colors) +
  scale_x_date(breaks = month_seq, labels = scales::label_date("%b")) +
  labs(
    title = "Apis spp.",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "italic"))
  


###################
### Bombus
###################

bombus_data <- plant_poll_data %>%
  # Keep only Bombus
  filter(insect_genus == "Bombus") %>%
  mutate(feeding = case_when(
    plant_eng_name == "Apple" ~ "Apple",
    plant_eng_name == "Broad leaf mustard" ~ "Mustard",
    plant_eng_name == "Jumli bean" ~ "Bean",
    plant_category == "wild" ~ "Wild plants",
    TRUE ~ NA_character_ ))  %>%
  filter(!is.na(feeding)) %>%
  mutate(survey_date = as.Date(survey_date),
         feeding = factor(feeding, levels = c("Bean", "Mustard", "Wild plants" , "Apple")))


bombus_plot <- ggplot(bombus_data, aes(x = survey_date, y = feeding, fill = feeding)) +
  geom_vline(data = data.frame(x = month_seq),
             aes(xintercept = x), inherit.aes = FALSE,
             color = "grey80", linewidth = 0.5) +
  geom_density_ridges(scale = 1.5, alpha = 0.8, show.legend = FALSE) +
  scale_x_date(breaks = month_seq, labels = scales::label_date("%b")) +
  scale_fill_manual(values = feeding_colors) +
  labs(title = "Bombus spp.",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 13)  +
  theme(plot.title = element_text(face = "italic"))
    

two_panel_phenology <- apis_plot + bombus_plot +
  plot_layout(ncol = 2, guides = "collect") +
  theme(legend.position = "bottom")  # remove or change if your plots have no legend

ggsave(plot=two_panel_phenology, filename="plots/Crop_wild_plant_visitation_phenology.svg", width=8, height=5, dpi=600, bg="white")
ggsave(plot=two_panel_phenology, filename="plots/Crop_wild_plant_visitation_phenology.png", width=8, height=5, dpi=600, bg="white")



##############################################################################################################################################################################
#########################################################          END OF SCRIPT 07   #######################################################################################
##############################################################################################################################################################################

