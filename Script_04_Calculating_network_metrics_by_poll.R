##########################################################################################################
######################################  MICRO-POLL SCRIPT 4  #############################################
####################  CROP–POLLINATOR NETWORK ROLES & NUTRITIONAL IMPORTANCE  ###########################
##########################################################################################################

# The purpose of this script is to calculate a small set of species-level network metrics
# (for pollinator taxa in crop–pollinator networks) that represent key network-role
# dimensions, and test which features of pollinator species (abundance + these metrics)
# best predict their nutritional importance when removed.
#
# Final metric set:
#   - degree            -> partner range / connectedness
#   - d                 -> specialisation (selectivity)
#   - species_strength  -> importance of the pollinator to crops
#
# This analysis relates to manuscript Question 3.


rm(list = ls())

############################################
# Load required packages and import data  ##
############################################

library(tidyverse)
library(readxl)
library(data.table)
library(bipartite)
library(broom)       # for tidy regression output
library(patchwork)   # for multi-panel plots
library(scales)      # for axis labels on log scales
library(corrplot)    # for correlation matrix visualisation

# Import plant–pollinator interaction data (visit-level observations)
plant_poll_data <- data.table(
  read_excel("input_data/MP_pollinator_visitation.xlsx",
             sheet = "Visitation data")
)

# Import results of species-decline modelling (nutritional impacts of removing each pollinator taxon)
species_decline_results_population <- read.csv(
  "output_data/OTU_Removal_Results_Population.csv"
)

#########################################################
# Filter interactions to crops and calculate abundance ##
#########################################################

# I first restrict to crop–pollinator interactions only, as my goal is to
# link crop visitation to human nutrition.
crop_poll_data <- plant_poll_data %>%
  filter(plant_category == "crop")

# I retain only those interactions where the insect has been identified to species,
# so that network metrics are comparable and ecologically interpretable.
plant_poll_data_clean <- crop_poll_data %>%
  filter(!is.na(insect_species))

# I then keep just the plant and pollinator identifiers needed to build the interaction matrix.
plant_poll_data_simple <- plant_poll_data_clean %>%
  select(plant_sci_name, insect_OTU)

# I treat the number of crop visits per pollinator OTU as a simple abundance measure.
abundance_table <- as.data.frame(table(plant_poll_data_simple$insect_OTU))
colnames(abundance_table) <- c("insect_OTU", "abundance")

# I now build a plant (rows) × pollinator (columns) interaction matrix, which is the
# basic quantitative web object required by bipartite.
interaction_matrix <- table(plant_poll_data_simple$plant_sci_name,
                            plant_poll_data_simple$insect_OTU)

#########################################################
# Calculate species-level network metrics for insects  ##
#########################################################

# The aim here is to derive a small set of theory-based, species-level metrics that capture
# complementary aspects of a pollinator's role in the interaction network:
#   - degree           : number of crop partners (binary connectedness)
#   - d                : Blüthgen’s specialisation index (selectivity)
#   - species_strength : importance of the pollinator to partner crops

# I first obtain the full species-level output for pollinators
# (the "higher" level = columns = pollinators).
insect_metrics_full <- specieslevel(interaction_matrix, level = "higher")

# I convert it to a data frame for easier manipulation.
insect_metrics_df <- as.data.frame(insect_metrics_full)

# I add the pollinator OTU identifiers as an explicit column.
insect_metrics_df$insect_OTU <- rownames(insect_metrics_df)

# Different versions of bipartite sometimes use slightly different column names, so
# I define a small helper function that will look for a set of candidate names and
# standardise them to a single, consistent name in my workflow.
pick_col <- function(df, candidates, new_name) {
  hit <- intersect(candidates, colnames(df))
  if (length(hit) == 0) {
    stop("None of the candidate columns found for metric '", new_name,
         "'. Candidates were: ", paste(candidates, collapse = ", "))
  }
  df %>%
    dplyr::rename(!!new_name := dplyr::all_of(hit[1]))
}

# I now extract and standardise the three key metrics of interest:
#   - degree            (partner range)
#   - d                 (specialisation)
#   - species_strength  (importance / dependence)
insect_metrics_key <- insect_metrics_df %>%
  pick_col(c("degree", "Degree"), "degree") %>%
  pick_col(c("d"),                "d") %>%
  pick_col(c("species.strength",
             "species strength",
             "strength"),
           "species_strength")

# I now merge these network-role metrics with the abundance information for each pollinator OTU.
insect_metrics_merged <- insect_metrics_key %>%
  left_join(abundance_table, by = "insect_OTU")

#########################################################
# Merge network roles with nutritional impact results  ##
#########################################################

# I rename the OTU column in the decline results to ensure consistency with the
# interaction/network data.
species_decline_results_population_filter <- species_decline_results_population %>%
  rename(insect_OTU = removed_OTU)

# I then attach the network-role metrics and abundance to the nutritional impact
# results for each pollinator species.
species_decline_network_role <- insect_metrics_merged %>%
  left_join(species_decline_results_population_filter, by = "insect_OTU")

# I remove "Other" as this is not a meaningful OTU for species-level network metrics.
species_decline_network_role <- species_decline_network_role %>%
  filter(insect_OTU != "Other")

# For this analysis I restrict attention to nutrients that are meaningfully
# pollinator-dependent, dropping the others here.
species_decline_network_role_subset <- species_decline_network_role %>%
  select(
    -propdecl_pollen_mean,
    -propdecl_VitB12_mean, -propdecl_Energy_mean, -propdecl_Fat_mean, -propdecl_Protein_mean,
    -propdecl_VitB1_mean, -propdecl_VitB2_mean, -propdecl_VitB3_mean, -propdecl_VitB6_mean
  )

# I summarise the overall nutritional importance of each pollinator as the sum of
# proportional declines in intake across six key pollinator-dependent nutrients.
species_decline_network_role_subset <- species_decline_network_role_subset %>%
  mutate(
    mean_intake_decline =
      propdecl_Calcium_mean +
      propdecl_Iron_mean +
      propdecl_VitC_mean +
      propdecl_Folate_mean +
      propdecl_VitA_mean +
      propdecl_VitE_mean
  )

# Optionally, I also keep a long-format version of nutrient-specific declines, which
# may be useful in other analyses (not used directly in the regression models below).
species_long <- species_decline_network_role_subset %>%
  pivot_longer(
    cols = starts_with("propdecl_"),
    names_to = "nutrient",
    values_to = "value"
  ) %>%
  mutate(
    nutrient = nutrient %>%
      str_remove("^propdecl_") %>%
      str_remove("_mean$")
  )

###############################################################
# Prepare data for analysis: filter and transform variables  ##
###############################################################

# I retain only those pollinator species that have a non-zero, non-missing
# overall nutritional impact.
species_decline_network_role_filtered <- species_decline_network_role_subset %>%
  filter(!is.na(mean_intake_decline),
         mean_intake_decline > 0)

# I log-transform both the response (nutritional impact) and abundance to
# stabilise variance and reduce skew.
species_decline_network_role_filtered <- species_decline_network_role_filtered %>%
  mutate(
    log_mean_intake_decline = log(mean_intake_decline),
    log_abundance           = log1p(abundance)  # log(1 + abundance) safely handles zeros
  )

###############################################################
# Define the key network-role metrics to be tested           ##
###############################################################

# Here I define the core network-role metrics to be tested. These were chosen
# a priori to represent distinct, theory-based dimensions of species role:
#   1) degree           -> partner range / connectedness
#   2) d                -> specialisation (selectivity)
#   3) species_strength -> importance (plant dependence on this pollinator)
metrics_key <- c("degree",
                 "d",
                 "species_strength")

# As a basic safeguard, I check that all of these metrics are present.
missing_metrics <- setdiff(metrics_key,
                           colnames(species_decline_network_role_filtered))
if (length(missing_metrics) > 0) {
  stop("The following metrics are missing from the data: ",
       paste(missing_metrics, collapse = ", "))
}

# I now construct a modelling data frame with the response, abundance, and
# the three key network-role metrics.
df_log <- species_decline_network_role_filtered %>%
  select(
    insect_OTU,
    mean_intake_decline,
    log_mean_intake_decline,
    abundance,
    log_abundance,
    all_of(metrics_key)
  )

# I log-transform the network metrics (using log1p to handle zeros and
# reduce the influence of extreme values).
df_log <- df_log %>%
  mutate(
    across(
      all_of(metrics_key),
      ~ log1p(.),
      .names = "log_{.col}"
    )
  )

# I store the names of the log-transformed metrics for later use in models.
log_metrics <- paste0("log_", metrics_key)

# I now drop any rows with missing values in the response, abundance, or
# any of the log-transformed metrics to ensure complete-case analysis.
df_log <- df_log %>%
  filter(
    complete.cases(
      select(., log_mean_intake_decline,
             log_abundance,
             all_of(log_metrics))
    )
  )

#############################################
# Inspect correlation among key predictors ##
#############################################

# Before fitting models, I check how strongly the chosen predictors are
# correlated with one another and with abundance, to assess redundancy.

# I assemble a data frame containing log-abundance and all log metrics.
cor_vars <- df_log %>%
  select(log_abundance, all_of(log_metrics))

# I compute the correlation matrix.
cor_matrix <- cor(cor_vars, use = "complete.obs")

# I print the correlations to the console.
print(cor_matrix)

# I also visualise the correlation matrix using corrplot to identify
# any strong collinearities at a glance.
corr_plot_metrics <- corrplot(cor_matrix,
                              method = "ellipse",
                              type   = "upper",
                              tl.cex = 0.8)
         

# Save PNG
png("plots/Species_metric_correlations.png",
    width = 7, height = 7, units = "in", res = 600)
corrplot(cor_matrix,
         method = "ellipse",
         type   = "upper",
         tl.cex = 0.8)
dev.off()

# Save SVG
svg("plots/Species_metric_correlations.svg",
    width = 7, height = 7)
corrplot(cor_matrix,
         method = "ellipse",
         type   = "upper",
         tl.cex = 0.8)
dev.off()
  
  

######################################################
# Regression models: abundance + key network metrics ##
######################################################

# I begin with a simple baseline model in which nutritional impact is
# predicted by abundance alone.
M0 <- lm(log_mean_intake_decline ~ log_abundance, data = df_log)
AIC_M0   <- AIC(M0)
adjR2_M0 <- summary(M0)$adj.r.squared
summary(M0)

# I then test each network-role metric individually, always in addition
# to abundance (i.e. abundance + single metric). This allows me to ask
# whether each metric explains additional variance beyond abundance alone.
metric_results <- lapply(log_metrics, function(met) {
  form <- as.formula(paste("log_mean_intake_decline ~ log_abundance +", met))
  mod  <- lm(form, data = df_log)
  tid  <- tidy(mod)
  slope_metric <- tid[tid$term == met, ]
  
  data.frame(
    metric                 = met,
    coef_metric            = slope_metric$estimate,
    p_metric               = slope_metric$p.value,
    adjR2_model            = summary(mod)$adj.r.squared,
    AIC_model              = AIC(mod),
    delta_AIC_vs_abundance = AIC(mod) - AIC_M0,
    adjR2_gain             = summary(mod)$adj.r.squared - adjR2_M0
  )
})

metric_results_df <- bind_rows(metric_results) %>%
  arrange(AIC_model)

metric_results_df


# I also fit a full model that includes abundance plus all
# chosen network-role metrics simultaneously.
form_all <- as.formula(paste(
  "log_mean_intake_decline ~ log_abundance +",
  paste(log_metrics, collapse = " + ")
))

M_all <- lm(form_all, data = df_log)
summary(M_all)
AIC_M_all <- AIC(M_all)

# I summarise the comparison between the abundance-only model and the
# full model in terms of AIC and adjusted R².
full_model_comparison <- data.frame(
  model = c("abundance_only", "abundance_plus_all_metrics"),
  AIC   = c(AIC_M0, AIC_M_all),
  adjR2 = c(adjR2_M0, summary(M_all)$adj.r.squared)
)

full_model_comparison


############################################################
# Combine all model performances into a single comparison ##
############################################################

# I now collate the performance of:
#   - the abundance-only baseline,
#   - each abundance + single-metric model,
#   - and the abundance + all-metrics model
# into a single comparison table.

# 1. Baseline row: abundance-only model
baseline_row <- data.frame(
  model                   = "abundance_only",
  model_type              = "baseline",
  metric                  = NA_character_,
  n_predictors            = 1,  # just log_abundance
  AIC                     = AIC_M0,
  adjR2                   = adjR2_M0,
  delta_AIC_vs_abundance  = 0,
  adjR2_gain              = 0,
  coef_metric             = NA_real_,
  p_metric                = NA_real_
)

# 2. Rows for models with abundance + each single metric
individual_rows <- metric_results_df %>%
  mutate(
    model        = paste0("abundance_plus_", metric),
    model_type   = "abundance + single_metric",
    n_predictors = 2,  # log_abundance + 1 network metric
    AIC          = AIC_model,
    adjR2        = adjR2_model
  ) %>%
  select(
    model, model_type, metric, n_predictors,
    AIC, adjR2,
    delta_AIC_vs_abundance, adjR2_gain,
    coef_metric, p_metric
  )

# 3. Row for the model with abundance + all metrics together
adjR2_M_all <- summary(M_all)$adj.r.squared

full_row <- data.frame(
  model                   = "abundance_plus_all_metrics",
  model_type              = "abundance + all_metrics",
  metric                  = "all",
  n_predictors            = 1 + length(log_metrics),  # abundance + all metrics
  AIC                     = AIC_M_all,
  adjR2                   = adjR2_M_all,
  delta_AIC_vs_abundance  = AIC_M_all - AIC_M0,
  adjR2_gain              = adjR2_M_all - adjR2_M0,
  coef_metric             = NA_real_,
  p_metric                = NA_real_
)

# 4. Bind everything into one table and order by AIC (best model first)
model_comparison_table <- bind_rows(
  baseline_row,
  individual_rows,
  full_row
) %>%
  arrange(AIC)

# I inspect this combined model comparison in the console and save it.
model_comparison_table

write.csv(
  model_comparison_table,
  "output_data/Network_role_all_model_comparisons.csv",
  row.names = FALSE
)

##########################################
# Plot abundance vs nutritional impacts ##
##########################################

# Because the results show that abundance is the only variable that predicts
# nutritional importance, and none of the network metrics add explanatory power,
# we produce ONLY a single diagnostic plot:
#     abundance → nutritional importance
#
# All partial-effect plots for network metrics have been removed because
# no metrics significantly improved model fit beyond abundance.

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
    x = "Pollinator abundance (log scale)",
    y = "Proportional decline in nutrient intake (log scale)",
    title = ""
  ) +
  theme_bw()

# Save the abundance-only plot
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
