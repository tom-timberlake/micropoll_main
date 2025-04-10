# Reproducible R code for the manuscript entitled:  
**‘Pollination deficits limit the health and wealth of impoverished communities’**

**Author:** Thomas P. Timberlake  
**Last updated:** 10/04/2025  
**Version:** 2.0

---

## Overview

This project assesses nutrient intake and dietary dependence on pollinator‐dependent crops, explores the links between crop, pollinator, and wild plant interactions, simulates the impacts of pollinator changes on nutritional and economic outcomes, and finally estimates the probability of nutrient adequacy among study participants.

This README provides a structured outline of all steps followed in the project—from raw dietary and interaction data processing through to simulating pollinator change scenarios and final adequacy evaluations, accompanied by socioeconomic contextual summaries. All outputs are designed for reproducible analysis and are saved in common directories for further review or manuscript submission.

**For best results:**  
- Run each script in the order provided.  
- If you encounter any dependency or file path issues, please verify that your working directory is set correctly and that all required input files are present in the `/input_data` folder.

### Required Input Files
- All raw data files are available in the `/input_data` folder.

### Output Files
- Processed datasets are saved in the `/output_data` directory.
- Plots are saved under `/plots`.

---

## Required Packages

The scripts use a variety of packages for data import, manipulation, visualization, modelling, and analysis. Some of the main packages include:

- **tidyverse, dplyr, tidyr, reshape2:** For data manipulation and reshaping.
- **readxl, data.table, openxlsx:** For importing Excel and CSV files.
- **skimr:** For data exploration and summaries.
- **ggplot2, cowplot, gridExtra, ggridges:** For plotting and visualization.
- **networkD3, webshot2:** For generating interactive network (Sankey) diagrams and exporting them as images.
- **lme4, emmeans:** For mixed-effects modelling and post hoc comparisons.
- **car:** For transformation functions and additional statistical utilities.

---

## Running the Workflow

### 1. Set Up Environment
- Ensure that all required packages are installed (or install them using `install.packages("packageName")`).
- Set your working directory so that the `/input_data` folder is in the expected location relative to the scripts.

### 2. Execution
- Run each script sequentially:
  1. **Script 1:** Importing Dietary Data, Calculating Intakes & Pollinator Dependence.
  2. **Script 2:** Linking Crops, Pollinators & Micronutrients to Calculate & Plot Contributions.
  3. **Script 2B:** Linking Crops, Pollinators & Micronutrients at Village Level.
  4. **Script 3:** Simulating Impacts of Pollinator Change on Nutrient Intake & Farming Income.
  5. **Script 4:** Calculating Probability of Adequacy from Nutrient Intake Data.
  6. **Script 5:** Summarising Basic Information About the Population to Provide Socioeconomic Context.

### 3. Reproducibility
- Each script starts by clearing the workspace and loading required libraries.
- Intermediate outputs from earlier scripts (often saved in `/output_data`) serve as inputs to later scripts.

---

## Script Summaries

### **Script 1: Importing Dietary Data, Calculating Intakes & Pollinator Dependence**
- **Purpose:**  
  Prepares raw dietary data by calculating ingredient-specific nutrient intakes, adjusting for nutrient retention during cooking, and calculating daily totals and average intakes per respondent.
- **Key Steps:**  
  - Import dietary intake and related nutrient information.
  - Replace missing retention factors with defaults (1 or 0 where required).
  - Calculate nutrient intakes by multiplying food quantities by nutrient density (and applying retention factors for micronutrients).
  - Sum ingredient intakes at the daily level and compute means by respondent and category.
  - Calculate the pollinator-dependent proportion of each nutrient by adjusting ingredient values based on their pollinator dependence characteristics.
  - Generate summary plots comparing local versus imported food contributions and export cleaned datasets for subsequent analysis.

---

### **Script 2: Linking Crops, Pollinators & Micronutrients to Calculate & Plot Contributions**
- **Purpose:**  
  Merges crop nutrient profiles with plant-pollinator interaction data and pollen transport capacities to determine the contribution scores of crops, insect pollinators, and wild plants to nutrient intakes.
- **Key Steps:**  
  - Import crop nutrient data (from Script 1 outputs), plant–pollinator visitation data, and pollen capacity data.
  - Reshape and clean the crop nutrient data and calculate the proportional nutrient contributions per crop.
  - Summarize plant–pollinator interactions and merge these with pollen transport data.
  - Calculate a “Pollinator Contribution (PC)” score for each insect, based on the product of crop contribution, pollen transport proportion, and crop pollinator dependence.
  - Summarize contributions into tables (and reshape data as needed) for crops, pollinators, and wild plants.
  - Export the summary results (as an Excel workbook) and prepare data for network visualization.
  - Create a Sankey diagram that visually links nutrients, crops, pollinators, and wild plant resources.

---

### **Script 2B: Linking Crops, Pollinators & Micronutrients at Village Level**
- **Purpose:**  
  Similar to Script 2, this script performs the analysis at a village (subgroup) level rather than at a meta-network level.
- **Key Steps:**  
  - Import village-level crop nutrient data along with plant–pollinator and pollen capacity data.
  - Repeat steps to clean, reshape, and compute nutrient contributions with the added grouping by village.
  - Generate plots that illustrate differences in crop, pollinator, and wild plant contributions to nutrient intakes across villages.
  - Export summary tables and key plots to compare importance scores among villages.

---

### **Script 3: Simulating Impacts of Pollinator Change on Nutrient Intake & Farming Income**
- **Purpose:**  
  Simulates the effects of changes in local pollinator populations on both dietary nutrient intakes and farming incomes.
- **Key Steps:**  
  - Import nutrient intake data (from Script 1 outputs), pollinator yield change data (from external sources), and farmer questionnaire data.
  - Merge yield change data with dietary data.
  - For a “pollinator loss” scenario, set pollinator-dependent contributions of imported foods to zero and subtract the pollinator-dependent component from nutrient intakes.
  - Adjust yields for pollinator enhancement scenarios by applying multipliers.
  - Calculate new nutrient intake profiles under yield enhancement and decline scenarios.
  - Determine compensatory dietary changes (e.g., additional rice consumption, with an option to simulate replacement by potatoes).
  - Export modified intake datasets that reflect each scenario for downstream analyses.

---

### **Script 4: Calculating Probability of Adequacy from Nutrient Intake Data**
- **Purpose:**  
  Estimates the probability of nutrient adequacy (PA) for individual respondents based on their daily nutrient intakes and compares these under different pollinator scenarios.
- **Key Steps:**  
  - Import processed daily nutrient intake datasets for the original, pollinator loss, and pollinator enhancement scenarios.
  - Import the Estimated Average Requirements (EAR) data.
  - For each respondent, compute the mean daily nutrient intake and combine with demographic data.
  - Categorize respondents into groups that match the EAR data (e.g., by age, sex, pregnancy status).
  - Merge EAR values with intake data and calculate Z-scores to assess the deviation of intake from the EAR.
  - Transform Z-scores into cumulative probability values (PA scores) using the normal cumulative distribution function (`pnorm`). Iron is handled separately with a bespoke categorization.
  - Summarize individual PA scores and compute an overall Mean Probability of Adequacy (MPA) per respondent.
  - Save individual-level PA summaries for each scenario, then merge and plot changes in PA by nutrient and respondent category.

---

### **Script 5: Summarising Basic Information About the Population to Provide Socioeconomic Context**
- **Purpose:**  
  Provides an overview of key socioeconomic characteristics of the study population and compares these with global smallholder data.
- **Key Steps:**  
  - Import household registration, census data, and farmer questionnaire data.
  - Merge datasets to obtain a comprehensive household dataset.
  - Compute summary variables such as household size, water collection time, landholding, schooling years, and total crop count.
  - Generate summary statistics (means, medians, standard deviations) of these features.
  - Reshape and export a summary of the study population characteristics.
  - Additionally, import comparison datasets (e.g., FAO smallholder profiles and Ellis et al. data) and create comparison plots to contextualize the socioeconomic and pollinator-dependence profiles of the study area.

---

## Final Remarks

This README provides a high-level guide to the full project workflow—from processing raw data and calculating nutrient intakes to simulating pollinator scenarios and evaluating nutrient adequacy, alongside socioeconomic context analyses. For detailed R commands and further explanations, please refer to the commented source code in each individual script.
