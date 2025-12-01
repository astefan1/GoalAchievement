# README for code submitted with the Stage III Registered Report

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
    
* The script ` generateData_Preregistration.R` generates mock data to conduct the analyses.
* The script `preregistered_analyses.R` contains an overview of all planned analyses (conducted based on the mock data)

## Data analysis

The folder 📂`data_analysis` contains the scripts necessary to reproduce all results reported in the manuscript. Generally, it follows the logic of the preregistered analyses described in the respective folder. However, analyses are distributed across multiple scripts to make them easier identifiable.

   * ` preprocessing.R` contains all steps of data preprocessing and computes descriptive statistics participant demographics
   * ` analysis_scales_reliability.R` computes Bayesian estimates of McDonald's Omega for all scales (these are reported in the Methods section)
   * ` analysis_dropout_rates.R` contains all analyses of treatment-dependent attrition, exclusion, and protocol adherence 
   * ` analysis_H1.R`, ` analysis_H2.R`, ` analysis_H3-1.R`, ` analysis_H3-2.R` contain all confirmatory preregistered analyses for Hypothesis 1-3, respectively, as well as the corresponding robustness checks
   * Scripts with the prefix `analysis_expl_mod_` contain preregistered exploratory moderation analyses. The word in the suffix indicates the moderating variable. For example, `analysis_expl_mod_attitudes.R` contains analyses where attitudes act as the moderator.
   * Scripts with the prefix `analysis_expl_med_` contain preregistered exploratory mediation analyses. The word in the suffix indicates the mediator. For example, `analysis_expl_med_automaticity.R` contains all analyses where automaticity is the mediator.
   * Scripts with the prefix `analysis_expl_noPrereg` contain non-preregistered exploratory hypothesis tests (`_DVincrease` contains a script to compute paired t-tests for pre-post comparisons; `_baselineDiffs` contains one-way ANOVAs testing for baseline differences in outcome variables)
   * The scripts `analysis_descriptives.R` and `figure_descriptives.R` refer to analyses of descriptive statistics reported in the supplementary materials and Figure 3 in the manuscript

__Reproducibility__: Bayesian analyses rely on Hamiltonian Monte Carlo sampling. We set a seed to make the random process reproducible on our machine, but since compiling is idiosyncratic on each machine, the seed will not guarantee reproducibility on other machines. This means that all Bayesian analyses are only reproducible with Monte Carlo error, i.e., Bayes factors and posterior estimates may deviate slightly from reported results if analyses are re-executed on a different machine. To make all analyses that depend on fitted model objects reproducible and allow others to investigate our posterior samples, we share the fitted model objects on the OSF.

## Data

The raw study data can be found on the highest level in the file `Dataset MCIIAP.xlsx`. The preprocessed data as well as additional data for analyses of attrition and exclusion can be found in the  📂`data_analysis`.

## Figures

Figures are saved in the 📂`figures` directory in the 📂`data_analysis` folder.