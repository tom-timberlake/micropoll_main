##########################################################################################################
######################################  MICRO-POLL SCRIPT 4  #############################################
####################  CROP–POLLINATOR NETWORK ROLES & NUTRITIONAL IMPORTANCE  ###########################
##########################################################################################################

# This script derives a small set of pollinator (OTU) descriptors that may explain why removing some
# pollinators causes larger declines in pollinator-dependent nutrient intake than removing others.
#
# Overview of the logic:
#   1) Build a full plant–pollinator interaction matrix from visit observations.
#   2) Define pollinator "abundance" as total number of visits recorded for that OTU in the survey.
#   3) Calculate several species-level network descriptors on the full web:
#        - specialisation (d′)
#        - interaction breadth (degree)
#        - network position (weighted closeness)
#        - Shannon diversity of interactions across plant partners
#   4) Many network metrics change mechanically with the number of visits per pollinator (sampling intensity).
#      To separate "structure beyond activity", each metric is compared to an abundance-constrained null model:
#      we preserve each pollinator's total number of visits (column sums) but randomise which plants they visit.
#      Observed values are expressed as z-scores relative to the null expectation.
#   5) Crop focus is treated similarly, but is defined as the fraction of each pollinator's visits that go to crops.
#   6) Model nutrient-intake decline using log(response) and log1p(abundance), plus the null-standardised predictors.

rm(list = ls())

############################################
# Load required packages and import data  ##
############################################

library(tidyverse)
library(readxl)
library(data.table)
library(bipartite)
library(broom)
library(scales)
library(corrplot)
library(vegan)

dir.create("plots", showWarnings = FALSE, recursive = TRUE)
dir.create("output_data", showWarnings = FALSE, recursive = TRUE)

# Visit-level interaction data
plant_poll_data <- data.table(
  read_excel("input_data/MP_pollinator_visitation.xlsx",
             sheet = "Visitation data")
)

# Nutritional impacts of removing each pollinator OTU
species_decline_results_population <- read.csv(
  "output_data/OTU_Removal_Results_Population.csv"
)

##############################################################
# Clean interaction data and build full and crop subsets     #
##############################################################

# Retain only records where pollinators have a consistent OTU identity.
plant_poll_data_clean_all <- plant_poll_data %>%
  filter(!is.na(insect_species)) %>%
  select(plant_sci_name, plant_category, insect_OTU)

# Crop-only subset used to quantify how much of each pollinator's activity is allocated to crops.
plant_poll_data_clean_crop <- plant_poll_data_clean_all %>%
  filter(plant_category == "crop")

############################################
# Construct interaction matrices (webs)   ##
############################################

# FULL web: all plants (crops + wild) × pollinators
interaction_matrix_all <- table(
  plant_poll_data_clean_all$plant_sci_name,
  plant_poll_data_clean_all$insect_OTU
)

# CROP web: crops only × pollinators (used only to compute crop visits and crop focus)
interaction_matrix_crop <- table(
  plant_poll_data_clean_crop$plant_sci_name,
  plant_poll_data_clean_crop$insect_OTU
)

#########################################################
# Calculate abundance (TOTAL visits) and crop focus     #
#########################################################

# Total abundance = total number of visits recorded per pollinator OTU (across crops + wild plants).
abund_total <- as.data.frame(table(plant_poll_data_clean_all$insect_OTU))
colnames(abund_total) <- c("insect_OTU", "abundance")

# Crop visits are needed to calculate crop_focus (proportion of activity directed to crops).
abund_crop <- as.data.frame(table(plant_poll_data_clean_crop$insect_OTU))
colnames(abund_crop) <- c("insect_OTU", "abundance_crop")

# Crop focus = fraction of visits that land on crops.
abundance_table <- abund_total %>%
  full_join(abund_crop, by = "insect_OTU") %>%
  mutate(
    abundance      = replace_na(abundance, 0),
    abundance_crop = replace_na(abundance_crop, 0),
    crop_focus     = ifelse(abundance > 0, abundance_crop / abundance, NA_real_)
  )

#########################################################
# Calculate observed species-level metrics (full web)   #
#########################################################

# For each pollinator OTU, compute:
#   - d_all:     specialisation (Blüthgen's d′) from bipartite
#   - degree_all: number of plant partners (unweighted breadth) from bipartite
#   - closeness_all: weighted closeness centrality from bipartite
#   - diversity_all: Shannon diversity of interactions across plant partners (from counts)
#
# Shannon diversity is computed using vegan::diversity on the interaction counts because
# the 'diversity' index is not available in some bipartite versions.

mat_all <- as.matrix(interaction_matrix_all)

diversity_all_vec <- vegan::diversity(t(mat_all), index = "shannon")

insect_metrics_all_df <- as.data.frame(
  specieslevel(
    interaction_matrix_all,
    level = "higher",
    index = c("d", "degree", "closeness")
  )
)
insect_metrics_all_df$insect_OTU <- rownames(insect_metrics_all_df)

metrics_all_obs <- insect_metrics_all_df %>%
  transmute(
    insect_OTU,
    d_all          = d,
    degree_all     = degree,
    closeness_all  = weighted.closeness,
    diversity_all  = diversity_all_vec[insect_OTU]
  )

# Combine observed metrics with abundance and crop focus
insect_predictors <- metrics_all_obs %>%
  left_join(abundance_table, by = "insect_OTU")

#########################################################
# Null models (full web) and null-standardised metrics  #
#########################################################

# Many metrics increase or decrease simply because an OTU has more total visits.
# The null model keeps each OTU's total number of visits fixed (column sums) but randomises
# which plant species receive those visits. Observed values are then expressed as z-scores
# relative to this null expectation.

# Identify which plant rows correspond to crops in the full matrix (for crop_focus under the null).
crop_plants <- unique(plant_poll_data_clean_crop$plant_sci_name)
crop_rows <- rownames(mat_all) %in% crop_plants

set.seed(1)
Nnull <- 999

# Null model with fixed column totals (pollinator totals), randomised allocation among plant rows.
nm <- vegan::nullmodel(mat_all, method = "c0_ind")

otus <- colnames(mat_all)

# Storage for null distributions
null_crop_focus_mat <- matrix(NA_real_, nrow = length(otus), ncol = Nnull,
                              dimnames = list(otus, paste0("sim_", seq_len(Nnull))))
null_d_mat <- matrix(NA_real_, nrow = length(otus), ncol = Nnull,
                     dimnames = list(otus, paste0("sim_", seq_len(Nnull))))
null_degree_mat <- matrix(NA_real_, nrow = length(otus), ncol = Nnull,
                          dimnames = list(otus, paste0("sim_", seq_len(Nnull))))
null_close_mat <- matrix(NA_real_, nrow = length(otus), ncol = Nnull,
                         dimnames = list(otus, paste0("sim_", seq_len(Nnull))))
null_diversity_mat <- matrix(NA_real_, nrow = length(otus), ncol = Nnull,
                             dimnames = list(otus, paste0("sim_", seq_len(Nnull))))

pb <- txtProgressBar(min = 0, max = Nnull, style = 3)

for (i in seq_len(Nnull)) {
  
  # vegan::simulate() returns a 3D array even for nsim = 1, so extract the first slice.
  mat_null <- simulate(nm, nsim = 1)
  mat_null <- mat_null[, , 1]
  
  # (A) Crop focus under the null: fraction of each OTU's null visits that go to crop rows.
  col_tot  <- colSums(mat_null)
  crop_tot <- colSums(mat_null[crop_rows, , drop = FALSE])
  null_crop_focus_mat[, i] <- ifelse(col_tot > 0, crop_tot / col_tot, NA_real_)
  
  # (B) Network metrics under the null using the same definitions as the observed metrics.
  null_df <- as.data.frame(
    specieslevel(mat_null, level = "higher", index = c("d", "degree", "closeness"))
  )
  null_df$insect_OTU <- rownames(null_df)
  
  null_d_mat[null_df$insect_OTU, i]      <- null_df$d
  null_degree_mat[null_df$insect_OTU, i] <- null_df$degree
  null_close_mat[null_df$insect_OTU, i]  <- null_df$weighted.closeness
  
  # (C) Shannon diversity under the null from interaction counts.
  null_diversity_mat[, i] <- vegan::diversity(t(mat_null), index = "shannon")
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Null means and SDs
null_mean_crop_focus <- rowMeans(null_crop_focus_mat, na.rm = TRUE)
null_sd_crop_focus   <- apply(null_crop_focus_mat, 1, sd, na.rm = TRUE)

null_mean_d_all <- rowMeans(null_d_mat, na.rm = TRUE)
null_sd_d_all   <- apply(null_d_mat, 1, sd, na.rm = TRUE)

null_mean_degree_all <- rowMeans(null_degree_mat, na.rm = TRUE)
null_sd_degree_all   <- apply(null_degree_mat, 1, sd, na.rm = TRUE)

null_mean_close_all <- rowMeans(null_close_mat, na.rm = TRUE)
null_sd_close_all   <- apply(null_close_mat, 1, sd, na.rm = TRUE)

null_mean_div_all <- rowMeans(null_diversity_mat, na.rm = TRUE)
null_sd_div_all   <- apply(null_diversity_mat, 1, sd, na.rm = TRUE)

# Attach null summaries and compute z-scores
insect_predictors <- insect_predictors %>%
  mutate(
    null_mean_crop_focus = null_mean_crop_focus[insect_OTU],
    null_sd_crop_focus   = null_sd_crop_focus[insect_OTU],
    z_crop_focus = ifelse(
      !is.na(crop_focus) & !is.na(null_sd_crop_focus) & null_sd_crop_focus > 0,
      (crop_focus - null_mean_crop_focus) / null_sd_crop_focus,
      NA_real_
    ),
    
    null_mean_d_all = null_mean_d_all[insect_OTU],
    null_sd_d_all   = null_sd_d_all[insect_OTU],
    z_d_all = ifelse(
      !is.na(d_all) & !is.na(null_sd_d_all) & null_sd_d_all > 0,
      (d_all - null_mean_d_all) / null_sd_d_all,
      NA_real_
    ),
    
    null_mean_degree_all = null_mean_degree_all[insect_OTU],
    null_sd_degree_all   = null_sd_degree_all[insect_OTU],
    z_degree_all = ifelse(
      !is.na(degree_all) & !is.na(null_sd_degree_all) & null_sd_degree_all > 0,
      (degree_all - null_mean_degree_all) / null_sd_degree_all,
      NA_real_
    ),
    
    null_mean_closeness_all = null_mean_close_all[insect_OTU],
    null_sd_closeness_all   = null_sd_close_all[insect_OTU],
    z_closeness_all = ifelse(
      !is.na(closeness_all) & !is.na(null_sd_closeness_all) & null_sd_closeness_all > 0,
      (closeness_all - null_mean_closeness_all) / null_sd_closeness_all,
      NA_real_
    ),
    
    null_mean_diversity_all = null_mean_div_all[insect_OTU],
    null_sd_diversity_all   = null_sd_div_all[insect_OTU],
    z_diversity_all = ifelse(
      !is.na(diversity_all) & !is.na(null_sd_diversity_all) & null_sd_diversity_all > 0,
      (diversity_all - null_mean_diversity_all) / null_sd_diversity_all,
      NA_real_
    )
  )

#########################################################
# Merge predictors with nutritional impact results      #
#########################################################

species_decline_results_population_filter <- species_decline_results_population %>%
  rename(insect_OTU = removed_OTU)

species_decline_network <- insect_predictors %>%
  left_join(species_decline_results_population_filter, by = "insect_OTU") %>%
  filter(insect_OTU != "Other")

# Drop nutrients we are not using in the pooled response
species_decline_network <- species_decline_network %>%
  select(
    -propdecl_pollen_mean,
    -propdecl_VitB12_mean, -propdecl_Energy_mean, -propdecl_Fat_mean, -propdecl_Protein_mean,
    -propdecl_VitB1_mean, -propdecl_VitB2_mean, -propdecl_VitB3_mean, -propdecl_VitB6_mean
  )

# Pooled nutritional importance (sum across six pollinator-dependent nutrients)
species_decline_network <- species_decline_network %>%
  mutate(
    mean_intake_decline =
      propdecl_Calcium_mean +
      propdecl_Iron_mean +
      propdecl_VitC_mean +
      propdecl_Folate_mean +
      propdecl_VitA_mean +
      propdecl_VitE_mean
  )

###############################################################
# Prepare modelling dataset                                  #
###############################################################

df_model <- species_decline_network %>%
  filter(!is.na(mean_intake_decline),
         mean_intake_decline > 0)

# Transformations:
# - log(response) stabilises variance and is safe because mean_intake_decline > 0
# - log1p(abundance) accommodates zeros and reduces skew
# - all network predictors are already on a comparable scale as null-referenced z-scores
df_model <- df_model %>%
  mutate(
    log_mean_intake_decline = log(mean_intake_decline),
    log_abundance           = log1p(abundance)
  )

###############################################################
# Final predictor set                                         #
###############################################################

# Candidate predictors (all null-standardised):
#   - z_d_all          : specialisation beyond null expectation
#   - z_crop_focus     : crop allocation beyond null expectation
#   - z_degree_all     : breadth beyond null expectation
#   - z_closeness_all  : centrality beyond null expectation
#   - z_diversity_all  : Shannon diversity beyond null expectation
candidate_predictors <- c(
  "z_d_all",
  "z_crop_focus",
  "z_degree_all",
  "z_closeness_all",
  "z_diversity_all"
)

missing_predictors <- setdiff(candidate_predictors, colnames(df_model))
if (length(missing_predictors) > 0) {
  stop("Missing predictors in df_model: ", paste(missing_predictors, collapse = ", "))
}

df_log <- df_model %>%
  select(
    insect_OTU,
    mean_intake_decline,
    log_mean_intake_decline,
    abundance,
    log_abundance,
    all_of(candidate_predictors)
  ) %>%
  filter(
    complete.cases(
      select(., log_mean_intake_decline, log_abundance, all_of(candidate_predictors))
    )
  )

#############################################
# Inspect correlation among predictors      #
#############################################

cor_vars <- df_log %>%
  select(log_mean_intake_decline, log_abundance, all_of(candidate_predictors))

cor_matrix <- cor(cor_vars, use = "complete.obs")
print(cor_matrix)

png("plots/Species_metric_correlations_abun_all.png",
    width = 7, height = 7, units = "in", res = 600)
corrplot(cor_matrix,
         method = "ellipse",
         type   = "upper",
         tl.cex = 0.8)
dev.off()

svg("plots/Species_metric_correlations_abun_all.svg",
    width = 7, height = 7)
corrplot(cor_matrix,
         method = "ellipse",
         type   = "upper",
         tl.cex = 0.8)
dev.off()

######################################################
# Regression models: abundance + candidate predictors #
######################################################

# Baseline model: nutritional importance explained by total abundance alone.
M0 <- lm(log_mean_intake_decline ~ log_abundance, data = df_log)
AIC_M0   <- AIC(M0)
adjR2_M0 <- summary(M0)$adj.r.squared
summary(M0)

# One-at-a-time models: abundance + one additional predictor
metric_results <- lapply(candidate_predictors, function(pred) {
  
  form <- as.formula(paste("log_mean_intake_decline ~ log_abundance +", pred))
  mod  <- lm(form, data = df_log)
  tid  <- tidy(mod)
  
  slope_pred <- tid[tid$term == pred, ]
  
  data.frame(
    predictor              = pred,
    coef_predictor         = slope_pred$estimate,
    p_predictor            = slope_pred$p.value,
    adjR2_model            = summary(mod)$adj.r.squared,
    AIC_model              = AIC(mod),
    delta_AIC_vs_abundance = AIC(mod) - AIC_M0,
    adjR2_gain             = summary(mod)$adj.r.squared - adjR2_M0
  )
})

metric_results_df <- bind_rows(metric_results) %>%
  arrange(AIC_model)

metric_results_df

# Full model: abundance + all selected predictors
form_all <- as.formula(paste(
  "log_mean_intake_decline ~ log_abundance +",
  paste(candidate_predictors, collapse = " + ")
))

M_all <- lm(form_all, data = df_log)
summary(M_all)

AIC_M_all   <- AIC(M_all)
adjR2_M_all <- summary(M_all)$adj.r.squared

############################################################
# Combine model performances into a single comparison table #
############################################################

baseline_row <- data.frame(
  model                   = "abundance_only",
  model_type              = "baseline",
  predictor               = NA_character_,
  n_predictors            = 1,
  AIC                     = AIC_M0,
  adjR2                   = adjR2_M0,
  delta_AIC_vs_abundance  = 0,
  adjR2_gain              = 0,
  coef_predictor          = NA_real_,
  p_predictor             = NA_real_
)

individual_rows <- metric_results_df %>%
  mutate(
    model        = paste0("abundance_plus_", predictor),
    model_type   = "abundance + single_predictor",
    n_predictors = 2,
    AIC          = AIC_model,
    adjR2        = adjR2_model
  ) %>%
  select(
    model, model_type, predictor, n_predictors,
    AIC, adjR2,
    delta_AIC_vs_abundance, adjR2_gain,
    coef_predictor, p_predictor
  )

full_row <- data.frame(
  model                   = "abundance_plus_all_predictors",
  model_type              = "abundance + all_predictors",
  predictor               = "all",
  n_predictors            = 1 + length(candidate_predictors),
  AIC                     = AIC_M_all,
  adjR2                   = adjR2_M_all,
  delta_AIC_vs_abundance  = AIC_M_all - AIC_M0,
  adjR2_gain              = adjR2_M_all - adjR2_M0,
  coef_predictor          = NA_real_,
  p_predictor             = NA_real_
)

model_comparison_table <- bind_rows(
  baseline_row,
  individual_rows,
  full_row
) %>%
  arrange(AIC)

model_comparison_table

write.csv(
  model_comparison_table,
  "output_data/Network_role_all_model_comparisons_abun_all.csv",
  row.names = FALSE
)

##########################################
# Plot abundance vs nutritional impacts  #
##########################################

p_abundance <- ggplot(df_log,
                      aes(x = abundance,  
                          y = mean_intake_decline)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE) +
  scale_x_log10(labels = label_number(accuracy = 1)) +
  scale_y_log10(labels = label_number(accuracy = 0.001)) +
  labs(
    x = "Pollinator abundance (log10 scale)",
    y = "Proportional decline in nutrient intake (log10 scale)",
    title = ""
  ) +
  theme_bw()

ggsave(
  plot     = p_abundance,
  filename = "plots/Abundance_nutritional_importance.png",
  width    = 5,
  height   = 5,
  dpi      = 600,
  bg       = "white"
)

ggsave(
  plot     = p_abundance,
  filename = "plots/Abundance_nutritional_importance.svg",
  width    = 5,
  height   = 5,
  dpi      = 600,
  bg       = "white"
)

##########################################################################################################
# End of script
##########################################################################################################

