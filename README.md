Title: Reproducible R code for the manuscript entitled: ‘Pollination deficits limit the health and wealth of impoverished communities’
Author: Thomas P Timberlake
Last updated: 10/04/2025
Version: 2.0

Overview
This project assesses nutrient intake and dietary dependence on pollinator‐dependent crops, explores the links between crop, pollinator, and wild plant interactions, simulates the impacts of pollinator changes on nutritional and economic outcomes, and finally estimates the probability of nutrient adequacy among study participants.

This README provides a structured outline of all steps followed in the project—from raw dietary and interaction data processing through to simulating pollinator change scenarios and final adequacy evaluations, accompanied by socioeconomic contextual summaries. 
All outputs are designed for reproducible analysis and are saved in common directories for further review or manuscript submission.

For best results, run each script in the order provided. Should you encounter any dependency or file path issues, verify that your working directory is set correctly and that all required input files are present in the /input_data folder.

Required Input Files: All raw data files are available in the /input_data folder.
Output Files: Processed datasets are saved in the /output_data directory, and plots are saved under /plots.

Required Packages
The scripts use a variety of packages for data import, manipulation, visualization, modelling, and analysis. Some of the main packages include:
•	tidyverse, dplyr, tidyr, reshape2: For data manipulation and reshaping.
•	readxl, data.table, openxlsx: For importing Excel and CSV files.
•	skimr: For data exploration and summaries.
•	ggplot2, cowplot, gridExtra, ggridges: For plotting and visualization.
•	networkD3, webshot2: For generating interactive network (Sankey) diagrams and exporting them as images.
•	lme4, emmeans: For mixed-effects modelling and post hoc comparisons.
•	car: For transformation functions and additional statistical utilities.

Running the Workflow
1.	Set Up Environment:
– Ensure that all required packages are installed (or install them using install.packages("packageName")).
– Set your working directory so that the /input_data folder is in the expected location relative to the scripts.
2.	Execution:
– Run each script sequentially (i.e., Script 1, then Script 2, Script 2B, Script 3, Script 4, and Script 5) to generate processed data, intermediate outputs, plots, and final summaries.
3.	Reproducibility:
– Each script starts by clearing the workspace and loading required libraries.
– Intermediate outputs from earlier scripts (often saved in /output_data) serve as inputs to later scripts.

Script Summaries

Script 1: Importing Dietary Data, Calculating Intakes & Pollinator Dependence
•	Purpose: Prepares raw dietary data by calculating ingredient-specific nutrient intakes, adjusting for nutrient retention during cooking, and calculating daily totals and average intakes per respondent.
•	Key Steps:
o	Import dietary intake and related nutrient information.
o	Replace missing retention factors with defaults (1 or 0 where required).
o	Calculate nutrient intakes by multiplying food quantities by nutrient density (and applying retention factors for micronutrients).
o	Sum ingredient intakes at the daily level and compute means by respondent and category.
o	Calculate the pollinator-dependent proportion of each nutrient by adjusting ingredient values based on their pollinator dependence characteristics.
o	Generate summary plots comparing local versus imported food contributions and export cleaned datasets for subsequent analysis.

Script 2: Linking Crops, Pollinators & Micronutrients to Calculate & Plot Contributions
•	Purpose: Merges crop nutrient profiles with plant-pollinator interaction data and pollen transport capacities to determine the contribution scores of crops, insect pollinators, and wild plants to nutrient intakes.
•	Key Steps:
o	Import crop nutrient data (from Script 1 outputs), plant–pollinator visitation data, and pollen capacity data.
o	Reshape and clean the crop nutrient data and calculate the proportional nutrient contributions per crop.
o	Summarize plant–pollinator interactions and merge these with pollen transport data.
o	Calculate a “Pollinator Contribution (PC)” score for each insect, based on the product of crop contribution, pollen transport proportion, and crop pollinator dependence.
o	Summarize contributions into tables (and reshape data as needed) for crops, pollinators, and wild plants.
o	Export the summary results (as an Excel workbook) and prepare data for network visualization.
o	Create a Sankey diagram that visually links nutrients, crops, pollinators, and wild plant resources.

Script 2B: Linking Crops, Pollinators & Micronutrients at Village Level
•	Purpose: Similar to Script 2, this script performs the analysis at a village (subgroup) level rather than a meta-network level.
•	Key Steps:
o	Import village-level crop nutrient data along with plant–pollinator and pollen capacity data.
o	Repeat steps to clean, reshape, and compute nutrient contributions with the added grouping by village.
o	Generate plots that illustrate differences in crop, pollinator, and wild plant contributions to nutrient intakes across villages.
o	Export summary tables and key plots to compare importance scores among villages.

Script 3: Simulating Impacts of Pollinator Change on Nutrient Intake & Farming Income
•	Purpose: Simulates the effects of changes in local pollinator populations on both dietary nutrient intakes and farming incomes.
•	Key Steps:
o	Import nutrient intake data (from Script 1 outputs), pollinator yield change data (from external data), and farmer questionnaire data.
o	Merge yield change data with dietary data.
o	For a “pollinator loss” scenario, set pollinator-dependent contributions of imported foods to zero and subtract the pollinator-dependent component from nutrient intakes.
o	Adjust yields for pollinator enhancement scenarios by applying multipliers.
o	Calculate new nutrient intake profiles under yield enhancement and decline scenarios.
o	Determine compensatory dietary changes (e.g., additional rice consumption, with an option to simulate replacement by potatoes).
o	Export modified intake datasets that reflect each scenario for downstream analyses.

Script 4: Calculating Probability of Adequacy from Nutrient Intake Data
•	Purpose: Estimates the probability of nutrient adequacy (PA) for individual respondents based on their daily nutrient intakes and compares these under different pollinator scenarios.
•	Key Steps:
o	Import processed daily nutrient intake datasets for the original, pollinator loss, and pollinator enhancement scenarios.
o	Import the Estimated Average Requirements (EAR) data.
o	For each respondent, compute the mean daily nutrient intake and combine with demographic data.
o	Categorize respondents into groups that match the EAR data (e.g., by age, sex, pregnancy status).
o	Merge EAR values with intake data and calculate Z-scores to assess the deviation of intake from EAR.
o	Transform Z-scores into cumulative probability values (PA scores) using the normal cumulative distribution function (pnorm). Iron is handled separately with a bespoke categorization.
o	Summarize individual PA scores and compute an overall Mean Probability of Adequacy (MPA) per respondent.
o	Save individual-level PA summaries for each scenario, then merge and plot changes in PA by nutrient and respondent category.

Script 5: Summarising Basic Information About the Population to Provide Socioeconomic Context
•	Purpose: Provides an overview of key socioeconomic characteristics of the study population and compares these with global smallholder data.
•	Key Steps:
o	Import household registration, census data, and farmer questionnaire data.
o	Merge datasets to obtain a comprehensive household dataset.
o	Compute summary variables such as household size, water collection time, landholding, schooling years, and total crop count.
o	Generate summary statistics (means, medians, standard deviations) of these features.
o	Reshape and export a summary of the study population characteristics.
o	Additionally, import comparison datasets (e.g., FAO smallholder profiles and Ellis et al. data) and create comparison plots to contextualize the socioeconomic and pollinator-dependence profiles of the study area.
