# ==============================================================================
# DESIGN ANALYSIS FOR ANCOVA MODEL (HYPOTHESIS 1)
# ==============================================================================

#### Load functions & packages ####
library(brms)
library(MASS)
source("generateData_ANCOVA_H1.R")
source("conductDA_ANCOVA.R")

########################### N = 300 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N300 <- simANCOVA(iter = 1000, 
                              N = 300, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.75)

DA_H0_rr75_N300 <- DA_ANCOVA(simData = DAT_H0_rr75_N300, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N300, file = "./generated_data/DA_H0_rr75_N300.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N300 <- simANCOVA(iter = 1000, 
                              N = 300, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.6)

DA_H0_rr60_N300 <- DA_ANCOVA(simData = DAT_H0_rr60_N300, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N300, file = "./generated_data/DA_H0_rr60_N300.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N300 <- simANCOVA(iter = 1000, 
                              N = 300, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.75)

DA_H1_rr75_N300 <- DA_ANCOVA(simData = DAT_H1_rr75_N300, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N300, file = "./generated_data/DA_H1_rr75_N300.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N300 <- simANCOVA(iter = 1000, 
                              N = 300, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.6)

DA_H1_rr60_N300 <- DA_ANCOVA(simData = DAT_H1_rr60_N300, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N300, file = "./generated_data/DA_H1_rr60_N300.RData")

########################### N = 350 ############################################

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

########################### N = 400 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N400 <- simANCOVA(iter = 1000, 
                              N = 400, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.75)

DA_H0_rr75_N400 <- DA_ANCOVA(simData = DAT_H0_rr75_N400, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N400, file = "./generated_data/DA_H0_rr75_N400.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N400 <- simANCOVA(iter = 1000, 
                              N = 400, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.6)

DA_H0_rr60_N400 <- DA_ANCOVA(simData = DAT_H0_rr60_N400, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N400, file = "./generated_data/DA_H0_rr60_N400.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N400 <- simANCOVA(iter = 1000, 
                              N = 400, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.75)

DA_H1_rr75_N400 <- DA_ANCOVA(simData = DAT_H1_rr75_N400, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N400, file = "./generated_data/DA_H1_rr75_N400.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N400 <- simANCOVA(iter = 1000, 
                              N = 400, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.6)

DA_H1_rr60_N400 <- DA_ANCOVA(simData = DAT_H1_rr60_N400, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N400, file = "./generated_data/DA_H1_rr60_N400.RData")

########################### N = 450 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N450 <- simANCOVA(iter = 1000, 
                              N = 450, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.75)

DA_H0_rr75_N450 <- DA_ANCOVA(simData = DAT_H0_rr75_N450, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N450, file = "./generated_data/DA_H0_rr75_N450.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N450 <- simANCOVA(iter = 1000, 
                              N = 450, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.6)

DA_H0_rr60_N450 <- DA_ANCOVA(simData = DAT_H0_rr60_N450, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N450, file = "./generated_data/DA_H0_rr60_N450.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N450 <- simANCOVA(iter = 1000, 
                              N = 450, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.75)

DA_H1_rr75_N450 <- DA_ANCOVA(simData = DAT_H1_rr75_N450, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N450, file = "./generated_data/DA_H1_rr75_N450.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N450 <- simANCOVA(iter = 1000, 
                              N = 450, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.6)

DA_H1_rr60_N450 <- DA_ANCOVA(simData = DAT_H1_rr60_N450, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N450, file = "./generated_data/DA_H1_rr60_N450.RData")

########################### N = 500 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N500 <- simANCOVA(iter = 1000, 
                         N = 500, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 0, 
                         effCombi = 0, 
                         retestReliability = 0.75)

DA_H0_rr75_N500 <- DA_ANCOVA(simData = DAT_H0_rr75_N500, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N500, file = "./generated_data/DA_H0_rr75_N500.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N500 <- simANCOVA(iter = 1000, 
                         N = 500, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 0, 
                         effCombi = 0, 
                         retestReliability = 0.6)

DA_H0_rr60_N500 <- DA_ANCOVA(simData = DAT_H0_rr60_N500, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N500, file = "./generated_data/DA_H0_rr60_N500.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N500 <- simANCOVA(iter = 1000, 
                         N = 500, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 2.5, 
                         effCombi = 5, 
                         retestReliability = 0.75)

DA_H1_rr75_N500 <- DA_ANCOVA(simData = DAT_H1_rr75_N500, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N500, file = "./generated_data/DA_H1_rr75_N500.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N500 <- simANCOVA(iter = 1000, 
                         N = 500, 
                         meanControl = 13.4, 
                         sdWithinGroup = 7.2, 
                         effSingle = 2.5, 
                         effCombi = 5, 
                         retestReliability = 0.6)

DA_H1_rr60_N500 <- DA_ANCOVA(simData = DAT_H1_rr60_N500, 
                        priorCoef = "normal(0, 10)", 
                        priorIntercept = "normal(13.4, 15)", 
                        priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N500, file = "./generated_data/DA_H1_rr60_N500.RData")

########################### N = 550 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N550 <- simANCOVA(iter = 1000, 
                              N = 550, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.75)

DA_H0_rr75_N550 <- DA_ANCOVA(simData = DAT_H0_rr75_N550, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N550, file = "./generated_data/DA_H0_rr75_N550.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N550 <- simANCOVA(iter = 1000, 
                              N = 550, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.6)

DA_H0_rr60_N550 <- DA_ANCOVA(simData = DAT_H0_rr60_N550, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N550, file = "./generated_data/DA_H0_rr60_N550.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N550 <- simANCOVA(iter = 1000, 
                              N = 550, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.75)

DA_H1_rr75_N550 <- DA_ANCOVA(simData = DAT_H1_rr75_N550, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N550, file = "./generated_data/DA_H1_rr75_N550.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N550 <- simANCOVA(iter = 1000, 
                              N = 550, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.6)

DA_H1_rr60_N550 <- DA_ANCOVA(simData = DAT_H1_rr60_N550, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N550, file = "./generated_data/DA_H1_rr60_N550.RData")

########################### N = 600 ############################################

#### Design analysis H0: Retest reliability 0.75 ####
set.seed(1234)
DAT_H0_rr75_N600 <- simANCOVA(iter = 1000, 
                              N = 600, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.75)

DA_H0_rr75_N600 <- DA_ANCOVA(simData = DAT_H0_rr75_N600, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr75_N600, file = "./generated_data/DA_H0_rr75_N600.RData")

#### Design analysis H0: Retest reliability 0.6 ####
set.seed(1234)
DAT_H0_rr60_N600 <- simANCOVA(iter = 1000, 
                              N = 600, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 0, 
                              effCombi = 0, 
                              retestReliability = 0.6)

DA_H0_rr60_N600 <- DA_ANCOVA(simData = DAT_H0_rr60_N600, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H0_rr60_N600, file = "./generated_data/DA_H0_rr60_N600.RData")

#### Design analysis H1: Retest reliability 0.75 ####
set.seed(1234)
DAT_H1_rr75_N600 <- simANCOVA(iter = 1000, 
                              N = 600, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.75)

DA_H1_rr75_N600 <- DA_ANCOVA(simData = DAT_H1_rr75_N600, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr75_N600, file = "./generated_data/DA_H1_rr75_N600.RData")

#### Design analysis H1: Retest reliability 0.6 ####
set.seed(1234)
DAT_H1_rr60_N600 <- simANCOVA(iter = 1000, 
                              N = 600, 
                              meanControl = 13.4, 
                              sdWithinGroup = 7.2, 
                              effSingle = 2.5, 
                              effCombi = 5, 
                              retestReliability = 0.6)

DA_H1_rr60_N600 <- DA_ANCOVA(simData = DAT_H1_rr60_N600, 
                             priorCoef = "normal(0, 10)", 
                             priorIntercept = "normal(13.4, 15)", 
                             priorSigma = "student_t(3, 0, 7.5)")

save(DA_H1_rr60_N600, file = "./generated_data/DA_H1_rr60_N600.RData")

