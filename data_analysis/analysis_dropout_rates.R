# ==============================================================================
# Analysis of attrition
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(readxl)         # Read in Excel spreadsheets
library(multibridge)    # Bayesian multinomial test
library(brms)           # Bayesian regression

#############################  Load data #######################################

GAData <- read.csv("GAData_preprocessed.csv")
GAIncompletePhase1 <- read_excel("MCIIAP_attrition.xlsx")
GAExcludedPhase1 <- read_excel("MCIIAP_exclusion.xlsx")

###################  Conduct preregistered analysis ############################

# In a first step, we will use Bayesian multinomial tests (Sarafoglou et al., 
# 2021) to assess study completion in each of the four study conditions. 
# Specifically, we will compare both drop-out rates (i.e., rates of 
# participants who voluntarily discontinued the study) and participant 
# exclusion rates (i.e., participants who were excluded due to failing attention 
# or quality checks) between conditions.

####  Drop-Out ####

# Preregistered multinomial tests for Phase 1
GAIncompletePhase1$treatment[is.na(GAIncompletePhase1$treatment)] <- "notreat"
table(GAIncompletePhase1$treatment)
nrow(GAIncompletePhase1)-sum(GAIncompletePhase1$treatment == "notreat")
ss_per_group1 <- as.vector(table(GAIncompletePhase1$treatment[GAIncompletePhase1$treatment != "notreat"]))

mult_bf_equality(x = ss_per_group1, a = rep(1, 4))

# Preregistered multinomial tests for Phase 2
ss_per_group2 <- as.vector(table(GAData$treatment[GAData$NoDataInPhase2 == TRUE]))
table(GAData$treatment[GAData$NoDataInPhase2 == TRUE])
mult_bf_equality(x = ss_per_group2, a = rep(1, 4))

# Preregistered multinomial tests across study phases
ss_per_group_both <- ss_per_group1 + ss_per_group2
mult_bf_equality(x = ss_per_group_both, a = rep(1, 4))

#### Participant exclusion ####

# Preregistered multinomial test
table(GAExcludedPhase1$treatment)
ss_per_group <- as.vector(table(GAExcludedPhase1$treatment))
mult_bf_equality(x = ss_per_group, a = rep(1, 4))

#### Protocol violation ####

# Preregistered multinomial test for protocol violation
ss_per_group <- as.vector(table(GAData$treatment[GAData$PerProtocolAnalyses == "No"]))
table(GAData$treatment[GAData$PerProtocolAnalyses == "No"])
mult_bf_equality(x = ss_per_group, a = rep(1, 3))

# Exploratory logistic regression
GAData_NoControl <- GAData[GAData$treatment != "control",]
GAData_NoControl$NoDataInPhase2_numeric <- as.numeric(GAData_NoControl$NoDataInPhase2)
GAData_NoControl$treatment_factor <- factor(GAData_NoControl$treatment)

mod_excl1 <- brm(PerProtocolAnalyses ~ treatment_factor,
                data = GAData_NoControl, 
                family = bernoulli(link = "logit"), 
                prior = prior(normal(0,1),class = b),
                sample_prior = TRUE,
                save_pars = save_pars(all = TRUE),
                iter = 12000,
                chains = 5,
                warmup = 2000,
                file = "../model_fits/mod_excl1")

mod_excl0 <- brm(PerProtocolAnalyses ~ 1,
                 data = GAData_NoControl, 
                 family = bernoulli(link = "logit"),
                 sample_prior = TRUE,
                 save_pars = save_pars(all = TRUE),
                 iter = 12000,
                 chains = 5,
                 warmup = 2000,
                 file = "../model_fits/mod_excl0")

bayes_factor(mod_excl1, mod_excl0)

summary(mod_excl1)

contrast1 <- hypothesis(mod_excl1, "treatment_factorimpInt = 0", )
contrast1
round(1/contrast1$hypothesis$Evid.Ratio, 2)

hypothesis(mod_excl1, "treatment_factormentCont = 0", )

contrast3 <- hypothesis(mod_excl1, "treatment_factormentCont-treatment_factorimpInt = 0", )
contrast3
round(1/contrast3$hypothesis$Evid.Ratio, 2)

