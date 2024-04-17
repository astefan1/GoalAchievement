# README for code submitted with the Stage II Registered Report

## Design analysis

### H1 (physical activity)

* Run the script 'design_analysis_H1.R'. This file makes use of functions from two other scripts:
    * 'generateData_ANCOVA_H1.R' defines a function to generate many datasets from an ANCOVA model (for H1, where Implementation Intentions and Mental Contrasting have the same assumed effect size)
    * 'conductDA_ANCOVA.R' defines a function that applies the ANCOVA model to a list of generated datasets

* Run the script 'resultsDA_ANCOVA.R' to summarize design analysis results across multiple sample sizes. This script makes use of functions from the script 'analyzeDA_H1.R'

### H2 (automaticity)

* Run the script 'design_analysis_H2.R'. This file makes use of functions from five other scripts and contains the entire design analysis for H2, including plots and tables for all conditions:
    * "generateData_ANCOVA_H1.R"
    * "generateData_ANCOVA_H2.R"
    * "conductDA_ANCOVA.R"
    * "analyzeDA_H1.R"
    * "analyzeDA_H2.R"
    
### H3-1 (goal commitment measured through direct measures) and H3-2 (goal commitment measured through indirect measures)

* Run the script 'design_analysis_H3.R'. This file makes use of functions from four other scripts and contains the entire design analysis for H2, including plots and tables for all conditions:
    * "generateData_ANCOVA_H1.R"
    * "generateData_ANCOVA_H2.R"
    * "conductDA_ANCOVA.R"
    * "analyzeDA_H1.R"
    * "analyzeDA_H2.R"
    * "analyzeDA_H3.R"

## Prior predictives
* 'prior_predictives.R' contains a script to produce prior predictive plots like the one in the manuscript and the supplementary materials for all hypotheses. The script makes use of the following other script:
    * "generateData_ANCOVA_H1.R"

## Preregistered analyses
* An overview of analyses that will be conducted on the collected data can be found in "preregistered_analyses.R'. This script uses simulated data created with the script 'generateData_Preregistration.R'.