# README for code submitted with the Stage II Registered Report

## Design analysis
* The design analyses under the different conditions are conducted in the script 'design_analysis.R'. This script makes use of two functions that are defined in the following files:

    * 'generateData_ANCOVA.R' defines a function to generate many datasets from an ANCOVA model
    * 'conductDA_ANCOVA.R' defines a function that applies the ANCOVA model to a list of generated datasets

* 'analyzeDA_ANCOVA.R' contains functions for analyzing the results of the design analysis. Mostly for plotting and tables.

* 'resultsDA_ANCOVA.R' applies the functions for analyzing design analysis results to the DA results created from this project.

* 'zzz_generateData_ANOVA.R' is a function to create data under the ANOVA model, but is currently not used in the design analysis

## Prior predictives
* 'prior_predictives.R' contains a script to produce prior predictive plots like the one in the manuscript and the supplementary materials from the ANCOVA model

## Preregistered analyses
* An overview of analyses that will be conducted on the collected data can be found in "preregistered_analyses.R'. This script uses simulated data created with the script 'generateData_Preregistration.R'.