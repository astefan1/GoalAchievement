# README for code submitted with the Stage II Registered Report

## Design analysis

All scripts relating to the design analysis conducted prior to data collection can be found in the folder 📂`design_analysis`. To reproduce the design analyses, follow the steps outlined below.

### H1 (physical activity)

* Run the script 'design_analysis_H1.R'. This file makes use of functions from two other scripts:
    * `generateData_ANCOVA_H1.R` defines a function to generate many datasets from an ANCOVA model (for H1, where Implementation Intentions and Mental Contrasting have the same assumed effect size)
    * `conductDA_ANCOVA.R` defines a function that applies the ANCOVA model to a list of generated datasets

* Run the script `resultsDA_ANCOVA.R` to summarize design analysis results across multiple sample sizes. This script makes use of functions from the script `analyzeDA_H1.R`

### H2 (automaticity)

* Run the script `design_analysis_H2.R`. This file makes use of functions from five other scripts and contains the entire design analysis for H2, including plots and tables for all conditions:
    * `generateData_ANCOVA_H1.R`
    * `generateData_ANCOVA_H2.R`
    * `conductDA_ANCOVA.R`
    * `analyzeDA_H1.R`
    * `analyzeDA_H2.R`
    
### H3-1 (goal commitment measured through direct measures) and H3-2 (goal commitment measured through indirect measures)

* Run the script `design_analysis_H3.R`. This file makes use of functions from four other scripts and contains the entire design analysis for H2, including plots and tables for all conditions:
    * `generateData_ANCOVA_H1.R`
    * `generateData_ANCOVA_H2.R`
    * `conductDA_ANCOVA.R`
    * `analyzeDA_H1.R`
    * `analyzeDA_H2.R`
    * `analyzeDA_H3.R`

### Prior predictives
* `prior_predictives.R` contains a script to produce prior predictive plots like the one in the manuscript and the supplementary materials for all hypotheses. The script makes use of the following other script:
    * `generateData_ANCOVA_H1.R`

## Preregistered analyses

The folder 📂`preregistered_analyses` contains two scripts that were submitted together with the Stage 2 Registered Report (i.e., prior to data collection)to describe the planned analyses.
    
* The script ` 'generateData_Preregistration.R` generates mock data to conduct the analyses.
* The script `preregistered_analyses.R` contains an overview of all planned analyses (conducted based on the mock data)

## Data analysis

The folder 📂`data_analysis` contains the scripts necessary to reproduce all results reported in the manuscript. Generally, it follows the logic of the preregistered analyses described in the respective folder. However, analyses are distributed across multiple scripts to make them easier identifiable.

   * The script ` 'generateData_Preregistration.R` generates mock data to conduct the analyses.