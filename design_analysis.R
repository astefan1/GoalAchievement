# ==============================================================================
# DESIGN ANALYSIS FOR ANCOVA MODEL
# ==============================================================================

#### Load functions & packages ####
library(brms)
library(MASS)
source("generateData_ANCOVA.R")
source("conductDA_ANCOVA.R")

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75 <- simANCOVA(iter = 1000, 
                         N = 350, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 0, 
                         effCombi = 0, 
                         retestReliability = 0.75)
                       
DA_H0_rr75 <- DA_ANCOVA(simData = DAT_H0_rr75, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75, file = "./generated_data/DA_H0_rr75.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60 <- simANCOVA(iter = 1000, 
                         N = 350, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 0, 
                         effCombi = 0, 
                         retestReliability = 0.6)

DA_H0_rr60 <- DA_ANCOVA(simData = DAT_H0_rr60, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60, file = "./generated_data/DA_H0_rr60.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75 <- simANCOVA(iter = 1000, 
                         N = 350, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 2.5, 
                         effCombi = 5, 
                         retestReliability = 0.75)

DA_H1_rr75 <- DA_ANCOVA(simData = DAT_H1_rr75, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75, file = "./generated_data/DA_H1_rr75.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60 <- simANCOVA(iter = 1000, 
                         N = 350, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 2.5, 
                         effCombi = 5, 
                         retestReliability = 0.6)

DA_H1_rr60 <- DA_ANCOVA(simData = DAT_H1_rr60, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60, file = "./generated_data/DA_H1_rr60.RData")


