# ==============================================================================
# DESIGN ANALYSIS FOR ANCOVA MODEL (HYPOTHESIS 2)
# ==============================================================================

#### Load functions & packages ####
library(brms)
library(MASS)
library(xtable)
source("generateData_ANCOVA_H1.R")
source("generateData_ANCOVA_H2.R")
source("conductDA_ANCOVA.R")
source("analyzeDA_H1.R")
source("analyzeDA_H2.R")

################## H0, retest reliability: 0.75, N = 500 #######################

set.seed(1234)
DAT_H0_rr75_N500_H2 <- simANCOVA(iter = 1000, 
                                 N = 500, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effSingle = 0, 
                                 effCombi = 0, 
                                 retestReliability = 0.75)
DA_H0_rr75_N500_H2 <- DA_ANCOVA(simData = DAT_H0_rr75_N500_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H0_rr75_N500_H2, file = "./generated_data/DA_H0_rr75_N500_H2.RData")

res_H0_rr75_H2_N500 <- analyze_DA_Effects_H2(DA_H0_rr75_N500_H2)
save(res_H0_rr75_H2_N500, file = "./generated_data/res_H0_rr75_H2_N500.RData")

BFtable_H2(res_H0_rr75_H2_N500)
postprobcat_H2(res_H0_rr75_H2_N500)

analyze_DA_postMeans(DA_H0_rr75_N500_H2, 
                     trueIntercept=2, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0.4,0.8))
mtext("H0, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.75, N = 600 #######################

set.seed(1234)
DAT_H0_rr75_N600_H2 <- simANCOVA(iter = 1000, 
                                 N = 600, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effSingle = 0, 
                                 effCombi = 0, 
                                 retestReliability = 0.75)
DA_H0_rr75_N600_H2 <- DA_ANCOVA(simData = DAT_H0_rr75_N600_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H0_rr75_N600_H2, file = "./generated_data/DA_H0_rr75_N600_H2.RData")

res_H0_rr75_H2_N600 <- analyze_DA_Effects_H2(DA_H0_rr75_N600_H2)
save(res_H0_rr75_H2_N600, file = "./generated_data/res_H0_rr75_H2_N600.RData")

BFtable_H2(res_H0_rr75_H2_N600)
postprobcat_H2(res_H0_rr75_H2_N600)

analyze_DA_postMeans(DA_H0_rr75_N600_H2, 
                     trueIntercept=2, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0.4,0.8))
mtext("H0, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.60, N = 500 #######################

set.seed(1234)
DAT_H0_rr60_N500_H2 <- simANCOVA(iter = 1000, 
                                 N = 500, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effSingle = 0, 
                                 effCombi = 0, 
                                 retestReliability = 0.60)
DA_H0_rr60_N500_H2 <- DA_ANCOVA(simData = DAT_H0_rr60_N500_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H0_rr60_N500_H2, file = "./generated_data/DA_H0_rr60_N500_H2.RData")

res_H0_rr60_H2_N500 <- analyze_DA_Effects_H2(DA_H0_rr60_N500_H2)
save(res_H0_rr60_H2_N500, file = "./generated_data/res_H0_rr60_H2_N500.RData")

BFtable_H2(res_H0_rr60_H2_N500)
postprobcat_H2(res_H0_rr60_H2_N500)

analyze_DA_postMeans(DA_H0_rr60_N500_H2, 
                     trueIntercept=2, 
                     trueRR = 0.6,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.6,0.9))
mtext("H0, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.60, N = 600 #######################

set.seed(1234)
DAT_H0_rr60_N600_H2 <- simANCOVA(iter = 1000, 
                                 N = 600, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effSingle = 0, 
                                 effCombi = 0, 
                                 retestReliability = 0.60)
DA_H0_rr60_N600_H2 <- DA_ANCOVA(simData = DAT_H0_rr60_N600_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H0_rr60_N600_H2, file = "./generated_data/DA_H0_rr60_N600_H2.RData")

res_H0_rr60_H2_N600 <- analyze_DA_Effects_H2(DA_H0_rr60_N600_H2)
save(res_H0_rr60_H2_N600, file = "./generated_data/res_H0_rr60_H2_N600.RData")

BFtable_H2(res_H0_rr60_H2_N600)
postprobcat_H2(res_H0_rr60_H2_N600)

analyze_DA_postMeans(DA_H0_rr60_N600_H2, 
                     trueIntercept=2, 
                     trueRR = 0.6,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.6,0.9))
mtext("H0, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.75, N = 500 #######################

set.seed(1234)
DAT_H1_rr75_N500_H2 <- simANCOVA_H2(iter = 1000,
                                    N = 500, 
                                    meanControl = 2, 
                                    sdWithinGroup = 0.9, 
                                    effimpInt = 0.3, 
                                    effmentCont = 0,
                                    effCombi = 0.3, 
                                    retestReliability = 0.75)
                                 
DA_H1_rr75_N500_H2 <- DA_ANCOVA(simData = DAT_H1_rr75_N500_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H1_rr75_N500_H2, file = "./generated_data/DA_H1_rr75_N500_H2.RData")

res_H1_rr75_H2_N500 <- analyze_DA_Effects_H2(DA_H1_rr75_N500_H2)
save(res_H1_rr75_H2_N500, file = "./generated_data/res_H1_rr75_H2_N500.RData")

BFtable_H2(res_H1_rr75_H2_N500)
postprobcat_H2(res_H1_rr75_H2_N500)

analyze_DA_postMeans(DA_H1_rr75_N500_H2, 
                     trueIntercept=2, 
                     trueRR = 0.75,
                     trueImpIntEffect=0.3,
                     trueMentContEffect=0,
                     trueCombiEffect=0.3, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.5,0.7))
mtext("H1, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.75, N = 600 #######################

set.seed(1234)
DAT_H1_rr75_N600_H2 <- simANCOVA_H2(iter = 1000,
                                    N = 600, 
                                    meanControl = 2, 
                                    sdWithinGroup = 0.9, 
                                    effimpInt = 0.3, 
                                    effmentCont = 0,
                                    effCombi = 0.3, 
                                    retestReliability = 0.75)
                                 
DA_H1_rr75_N600_H2 <- DA_ANCOVA(simData = DAT_H1_rr75_N600_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H1_rr75_N600_H2, file = "./generated_data/DA_H1_rr75_N600_H2.RData")

res_H1_rr75_H2_N600 <- analyze_DA_Effects_H2(DA_H1_rr75_N600_H2)
save(res_H1_rr75_H2_N600, file = "./generated_data/res_H1_rr75_H2_N600.RData")

BFtable_H2(res_H1_rr75_H2_N600)
postprobcat_H2(res_H1_rr75_H2_N600)

analyze_DA_postMeans(DA_H1_rr75_N600_H2, 
                     trueIntercept=2, 
                     trueRR = 0.75,
                     trueImpIntEffect=0.3,
                     trueMentContEffect=0,
                     trueCombiEffect=0.3, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.5,0.7))
mtext("H1, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.60, N = 500 #######################

set.seed(1234)
DAT_H1_rr60_N500_H2 <- simANCOVA_H2(iter = 1000, 
                                 N = 500, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effimpInt = 0.3, 
                                 effmentCont = 0,
                                 effCombi = 0.3, 
                                 retestReliability = 0.60)
DA_H1_rr60_N500_H2 <- DA_ANCOVA(simData = DAT_H1_rr60_N500_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H1_rr60_N500_H2, file = "./generated_data/DA_H1_rr60_N500_H2.RData")

res_H1_rr60_H2_N500 <- analyze_DA_Effects_H2(DA_H1_rr60_N500_H2)
save(res_H1_rr60_H2_N500, file = "./generated_data/res_H1_rr60_H2_N500.RData")

BFtable_H2(res_H1_rr60_H2_N500)
postprobcat_H2(res_H1_rr60_H2_N500)

analyze_DA_postMeans(DA_H1_rr60_N500_H2, 
                     trueIntercept=2, 
                     trueRR = 0.60,
                     trueImpIntEffect=0.3,
                     trueMentContEffect=0,
                     trueCombiEffect=0.3, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.6,0.8))
mtext("H1, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.60, N = 600 #######################

set.seed(1234)
DAT_H1_rr60_N600_H2 <- simANCOVA_H2(iter = 1000, 
                                 N = 600, 
                                 meanControl = 2, 
                                 sdWithinGroup = 0.9, 
                                 effimpInt = 0.3, 
                                 effmentCont = 0,
                                 effCombi = 0.3,  
                                 retestReliability = 0.60)
DA_H1_rr60_N600_H2 <- DA_ANCOVA(simData = DAT_H1_rr60_N600_H2, 
                                priorCoef = "normal(0, 1.5)", 
                                priorIntercept = "normal(2.3, 1)", 
                                priorSigma = "student_t(3, 0, 0.7)")

save(DA_H1_rr60_N600_H2, file = "./generated_data/DA_H1_rr60_N600_H2.RData")

res_H1_rr60_H2_N600 <- analyze_DA_Effects_H2(DA_H1_rr60_N600_H2)
save(res_H1_rr60_H2_N600, file = "./generated_data/res_H1_rr60_H2_N600.RData")

BFtable_H2(res_H1_rr60_H2_N600)
postprobcat_H2(res_H1_rr60_H2_N600)

analyze_DA_postMeans(DA_H1_rr60_N600_H2, 
                     trueIntercept=2, 
                     trueRR = 0.60,
                     trueImpIntEffect=0.3,
                     trueMentContEffect=0,
                     trueCombiEffect=0.3, 
                     xlimIntercept=c(1,3), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.8), 
                     xlimSigma=c(0.6,0.8))
mtext("H1, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)
