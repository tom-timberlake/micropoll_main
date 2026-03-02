# Reproducible R code for the manuscript entitled:  
**‘Pollinators support the nutrition and income of vulnerable communities’**

**Author:** Thomas P. Timberlake
**Manuscript accepted:** 23/02/2026
**Last updated:** 02/03/2026  
**Version:** 6.0

---

## Overview

This project assesses nutrient intake and dietary dependence on pollinator‐dependent crops, explores the links between crop, pollinator, and wild plant interactions, simulates the impacts of pollinator changes on nutritional and economic outcomes, investigates the role of network structure in shaping nutritional impacts, and finally estimates the probability of nutrient adequacy among study participants. Socioeconomic data provide additional context to the findings.

This README provides a structured outline of all steps followed in the project—from raw dietary and interaction data processing through to simulating pollinator change scenarios, network analyses, adequacy evaluations, and socioeconomic comparisons. All outputs are designed for reproducible analysis and are saved in common directories for further review or manuscript submission.

**For best results:**  
- Run each script in the order provided.  
- If you encounter any dependency or file path issues, verify that your working directory is set correctly and that all required input files are present in the `/input_data` folder.

### Required Input Files
- All raw data files are available in the `/input_data` folder.

### Output Files
- Processed datasets are saved in the `/output_data` directory.  
- Plots are saved under `/plots`.

---

## Required Packages

The scripts use a variety of packages for data import, manipulation, visualization, modelling, and analysis. Some of the main packages include:

- **tidyverse, dplyr, tidyr, reshape2:** Data manipulation and reshaping  
- **readxl, data.table, openxlsx:** Importing Excel and CSV files  
- **skimr:** Data exploration and summaries  
- **ggplot2, cowplot, gridExtra, ggridges:** Plotting and visualization  
- **networkD3, webshot2:** Interactive network diagrams (Sankey plots) and image export  
- **bipartite:** Pollination network analyses and species-level metrics  
- **lme4, emmeans:** Mixed-effects modelling and post hoc comparisons  
- **car, robustbase, broom:** Model diagnostics, robust regressions, tidy outputs  
- **scales, viridis:** Improved scaling and colour palettes
- **bipartite, vegan:** Construction and analysis of quantitative plant–pollinator networks, including calculation of species-level network metrics and abundance-constrained null model simulations used to standardise metrics and quantify interaction structure beyond sampling effects.

---

## Running the Workflow

1. **Set Up Environment:**  
   - Ensure that all required packages are installed (or install them using `install.packages("packageName")`).  
   - Set your working directory so that the `/input_data` folder is in the expected location relative to the scripts.

2. **Execution:**  
   - Run each script sequentially:  
     `Script 1 → Script 2A → Script 2B → Script 3A → Script 3B → Script 4 → Script 5 → Script 6 → Script 7`  
   - This will generate processed data, intermediate outputs, plots, and final summaries.

3. **Reproducibility:**  
   - Each script begins by clearing the workspace and loading required libraries.  
   - Intermediate outputs from earlier scripts (saved in `/output_data`) serve as inputs for later scripts.

---

## Script Summaries

### **Script 1: Importing Dietary Data, Calculating Intakes & Pollinator Dependence**

**Purpose:**  
Prepares raw dietary data by calculating ingredient-specific nutrient intakes, adjusting for nutrient retention during cooking, and quantifying pollinator dependence of nutrient intake.

**Key Steps:**  
- Import dietary intake and nutrient composition data.  
- Apply retention factors and calculate nutrient intakes per ingredient.  
- Aggregate intakes at daily and respondent levels.  
- Calculate pollinator-dependent contributions to nutrient intakes.  
- Estimate nutrient intakes derived from local versus imported sources.  
- Produce summary plots and export cleaned datasets for downstream analysis.

---

### **Script 2A: Linking Crops, Pollinators & Micronutrients**

**Purpose:**  
Merges crop nutrient profiles with plant–pollinator interaction data and pollen transport capacities to determine the contribution scores of crops, insect pollinators, and wild plants to nutrient intakes.

**Key Steps:**  
- Import crop nutrient data (from Script 1 outputs), plant–pollinator visitation data, and pollen capacity data.  
- Clean and reshape crop nutrient data; calculate proportional nutrient contributions per crop.  
- Summarize plant–pollinator interactions and merge these with pollen transport data.  
- Calculate a “Pollinator Contribution (PC)” score for each insect based on crop contribution, pollen transport proportion, and crop pollinator dependence.  
- Summarize contributions into tables for crops, pollinators, and wild plants.  
- Export results (as an Excel workbook) and prepare data for network visualization.  
- Create a Sankey diagram linking nutrients, crops, pollinators, and wild plant resources.

---

### **Script 2B: Linking Crops, Pollinators & Micronutrients at Village Level**

**Purpose:**  
Performs the same analysis as Script 2A but disaggregated by village.

**Key Steps:**  
- Import village-level crop nutrient data, plant–pollinator, and pollen capacity data.  
- Clean, reshape, and compute nutrient contributions grouped by village.  
- Generate comparative plots of crop, pollinator, and wild plant contributions across villages.  
- Export summary tables and plots to compare importance scores by village.

---

### **Script 3A: Modelling Continuous Pollinator Decline and Species Removals**

**Purpose:**  
Simulates continuous rates of pollinator decline and quantifies impacts on nutrient intakes (with calorific substitution by rice), while also assessing individual pollinator importance via species removals.

**Key Steps:**  
- Import interaction, pollen capacity, and dietary data.  
- Simulate 100 replicate runs of pollinator decline scenarios (0–100% decline).  
- Apply caloric replacement with rice to maintain energy balance.  
- Summarize results at population, village, and respondent-category levels.  
- Plot pollination service decline vs. nutrient intake decline for key micronutrients.  
- Conduct species-by-species removal simulations to identify nutritional importance of taxa.  
- Export removal results for use in Script 4.

---

### **Script 3B: Modelling Three Discrete Scenarios of Pollinator Change**

**Purpose:**  
Model nutritional and economic outcomes under three scenarios: complete pollinator loss, partial decline (to 2030), and pollination enhancement.

**Key Steps:**  
- Import plant–pollinator, pollen transport, dietary intake, and income data.  
- Define three scenarios:  
  1. **No pollinators (no_poll)** – complete loss of pollination services.  
  2. **Pollinator decline (poll_decl)** – projected decline by 2030.  
  3. **Pollinator enhancement (poll_incr)** – removal of pollination deficits.  
- Run 100 replicate stochastic simulations per scenario.  
- Apply caloric substitution to maintain energy balance.  
- Generate participant-level datasets and proportional change summaries.  
- Link to farmer income data and assess economic impacts.  
- Model differences across villages and respondent categories.

**Outputs:**  
Scenario-specific datasets, nutrient and income change summaries, and plots.  
**Sensitivity analyses** included for robustness checks.

---

### **Script 4: Calculating Crop–Pollinator Network Metrics**

**Purpose:**  
Quantify key species-level crop–pollinator network roles and pollinator abundance, and assess their ability to predict nutritional importance.

**Key Steps:**  
- Import plant–pollinator visitation data and pollinator species-removal results generated in Script 3A.
- Construct a quantitative plant × pollinator interaction matrix including both crop and wild plant species.
- Calculate observed species-level network metrics for each pollinator OTU using the bipartite package, including:
   1. Blüthgen’s d′ specialisation,
   2. Interaction breadth (degree),
   3. Weighted closeness centrality,
   4. Shannon diversity of interactions across plant partners.
- Quantify pollinator abundance as total recorded visit frequency and calculate v. crop focus as the proportion of visits directed to crop species.
- Generate abundance-constrained null models using the vegan package (nullmodel, method = c0_ind) to randomise plant associations while preserving total visits per pollinator.
- Convert observed network metrics and crop focus to Z-scores relative to null model expectations, isolating interaction structure beyond sampling intensity.
- Summarise nutritional importance for each pollinator as the combined proportional intake decline across six pollinator-dependent nutrients (Calcium, Iron, Vitamin A, Vitamin C, Vitamin E, and Folate).
- Prepare a modelling dataset by log-transforming nutritional importance and pollinator abundance and retaining null-standardised predictors.
- Assess collinearity among predictors using correlation matrices.
- Fit linear models predicting nutritional importance using:
   > abundance alone,
   > abundance plus individual network metrics,
   > abundance plus all network metrics combined.
- Compare models using AIC and adjusted R² to evaluate whether network structure improves prediction beyond abundance alone.
- Export model comparison tables and produce visualisations of predictor correlations and abundance–nutritional importance relationships.



**Outputs:**  
- output_data/Network_role_all_model_comparisons_abun_all.csv – model comparison table summarising abundance-only, single-metric, and full models.
- plots/Species_metric_correlations_abun_all.(png|svg) – correlation matrix of abundance, nutritional importance, and null-standardised network metrics.
- plots/Abundance_nutritional_importance.(png|svg) – relationship between pollinator abundance and nutritional importance.


---

### **Script 5: Calculating Probability of Adequacy from Nutrient Intake Data**

**Purpose:**  
Estimate the probability of nutrient adequacy (PA) for individuals under different pollinator scenarios and summarize group-level adequacy.

**Key Steps:**  
- Import scenario-level nutrient intake datasets and EAR reference data.  
- Compute usual intakes and assign respondents to EAR categories.  
- Calculate Z-scores and transform to PA scores using `pnorm`.  
- Handle iron using sex- and age-specific cut-offs.  
- Compute Mean Probability of Adequacy (MPA) across 11 nutrients.  
- Visualize PA changes using Cleveland plots and estimate prevalence of new deficiencies.

---

### **Script 6: Summarising Socioeconomic Context of the Study Population**

**Purpose:**  
Provide descriptive statistics and contextual comparisons on socioeconomic variables.

**Key Steps:**  
- Import and merge registration, census, and farmer questionnaire data.  
- Derive new household-level variables (e.g., crops grown, schooling, income/ha).  
- Summarize and visualize demographic, livelihood, and pollination knowledge indicators.  
- Compare Jumla data with global smallholder datasets and visualize cross-country differences.  
- Estimate and visualize the proportion of self-consumed vs. sold crop production.

---

### **Script 7: Investigating Plant and Pollinator Phenology Patterns**

**Purpose:**  
Visualize seasonal visitation patterns of key pollinators (Apis, Bombus) to crops and wild plants to explore potential resource competition.

**Key Steps:**  
- Import visitation data from `input_data/MP_pollinator_visitation.xlsx` (sheet: “Visitation data”).  
- Create feeding category variable (Bean, Mustard, Wild plants, Apple).  
- Filter data for Apis and Bombus, and generate monthly visitation summaries.  
- Produce density ridge plots showing monthly visitation peaks by feeding category.

---

## Final Remarks

This README provides a high-level guide to the full project workflow—from processing raw data and calculating nutrient intakes to simulating pollinator scenarios and evaluating nutrient adequacy, alongside socioeconomic context analyses. For detailed R commands and further explanations, please refer to the commented source code in each individual script.

---

**End of README**

