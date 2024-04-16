# ==============================================================================
# DESIGN ANALYSIS FOR ANCOVA MODEL (HYPOTHESIS 3_1)
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
source("analyzeDA_H3.R")

################## H0, retest reliability: 0.75, N = 500 #######################

set.seed(1234)
DAT_H0_rr75_N500_H31 <- simANCOVA(iter = 1000,
                                   N = 500, 
                                   meanControl = 4, 
                                   sdWithinGroup = 1.5, 
                                   effSingle = 0, 
                                   effCombi = 0, 
                                   retestReliability = 0.75)

DA_H0_rr75_N500_H31 <- DA_ANCOVA(simData = DAT_H0_rr75_N500_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_rr75_N500_H31, file = "./generated_data/DA_H0_rr75_N500_H31.RData")

res_H0_rr75_H31_N500 <- analyze_DA_Effects_H3(DA_H0_rr75_N500_H31)
save(res_H0_rr75_H31_N500, file = "./generated_data/res_H0_rr75_H31_N500.RData")

BFtable_H3(res_H0_rr75_H31_N500)
postprobcat_H3(res_H0_rr75_H31_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_rr75_N500_H31, 
                     trueIntercept=4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0,2))
mtext("H0, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.75, N = 600 #######################

set.seed(1234)
DAT_H0_rr75_N600_H31 <- simANCOVA(iter = 1000,
                                  N = 600, 
                                  meanControl = 4, 
                                  sdWithinGroup = 1.5, 
                                  effSingle = 0, 
                                  effCombi = 0, 
                                  retestReliability = 0.75)

DA_H0_rr75_N600_H31 <- DA_ANCOVA(simData = DAT_H0_rr75_N600_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_rr75_N600_H31, file = "./generated_data/DA_H0_rr75_N600_H31.RData")

res_H0_rr75_H31_N600 <- analyze_DA_Effects_H3(DA_H0_rr75_N600_H31)
save(res_H0_rr75_H31_N600, file = "./generated_data/res_H0_rr75_H31_N600.RData")

BFtable_H3(res_H0_rr75_H31_N600)
postprobcat_H3(res_H0_rr75_H31_N600)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_rr75_N600_H31, 
                     trueIntercept=4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0,2))
mtext("H0, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.60, N = 500 #######################

set.seed(1234)
DAT_H0_rr60_N500_H31 <- simANCOVA(iter = 1000,
                                  N = 500, 
                                  meanControl = 4, 
                                  sdWithinGroup = 1.5, 
                                  effSingle = 0, 
                                  effCombi = 0, 
                                  retestReliability = 0.60)

DA_H0_rr60_N500_H31 <- DA_ANCOVA(simData = DAT_H0_rr60_N500_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_rr60_N500_H31, file = "./generated_data/DA_H0_rr60_N500_H31.RData")

res_H0_rr60_H31_N500 <- analyze_DA_Effects_H3(DA_H0_rr60_N500_H31)
save(res_H0_rr60_H31_N500, file = "./generated_data/res_H0_rr60_H31_N500.RData")

BFtable_H3(res_H0_rr60_H31_N500)
postprobcat_H3(res_H0_rr60_H31_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_rr60_N500_H31, 
                     trueIntercept=4, 
                     trueRR = 0.60,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.7), 
                     xlimSigma=c(0,2))
mtext("H0, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H0, retest reliability: 0.60, N = 600 #######################

set.seed(1234)
DAT_H0_rr60_N600_H31 <- simANCOVA(iter = 1000,
                                  N = 600, 
                                  meanControl = 4, 
                                  sdWithinGroup = 1.5, 
                                  effSingle = 0, 
                                  effCombi = 0, 
                                  retestReliability = 0.60)

DA_H0_rr60_N600_H31 <- DA_ANCOVA(simData = DAT_H0_rr60_N600_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_rr60_N600_H31, file = "./generated_data/DA_H0_rr60_N600_H31.RData")

res_H0_rr60_H31_N600 <- analyze_DA_Effects_H3(DA_H0_rr60_N600_H31)
save(res_H0_rr60_H31_N600, file = "./generated_data/res_H0_rr60_H31_N600.RData")

BFtable_H3(res_H0_rr60_H31_N600)
postprobcat_H3(res_H0_rr60_H31_N600)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_rr60_N600_H31, 
                     trueIntercept=4, 
                     trueRR = 0.60,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.5,0.7), 
                     xlimSigma=c(0,2))
mtext("H0, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.75, N = 500 #######################

set.seed(1234)
DAT_H1_rr75_N500_H31 <- simANCOVA_H2(iter = 1000,
                                     N = 500, 
                                     meanControl = 4, 
                                     sdWithinGroup = 1.5, 
                                     effimpInt = 0,
                                     effmentCont = 0.5,
                                     effCombi = 0.5,
                                     retestReliability = 0.75)

DA_H1_rr75_N500_H31 <- DA_ANCOVA(simData = DAT_H1_rr75_N500_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_rr75_N500_H31, file = "./generated_data/DA_H1_rr75_N500_H31.RData")

res_H1_rr75_H31_N500 <- analyze_DA_Effects_H3(DA_H1_rr75_N500_H31)
save(res_H1_rr75_H31_N500, file = "./generated_data/res_H1_rr75_H31_N500.RData")

BFtable_H3(res_H1_rr75_H31_N500)
postprobcat_H3(res_H1_rr75_H31_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_rr75_N500_H31, 
                     trueIntercept=4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0,2))
mtext("H1, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.75, N = 600 #######################

set.seed(1234)
DAT_H1_rr75_N600_H31 <- simANCOVA_H2(iter = 1000,
                                     N = 600, 
                                     meanControl = 4, 
                                     sdWithinGroup = 1.5, 
                                     effimpInt = 0,
                                     effmentCont = 0.5,
                                     effCombi = 0.5,
                                     retestReliability = 0.75)

DA_H1_rr75_N600_H31 <- DA_ANCOVA(simData = DAT_H1_rr75_N600_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_rr75_N600_H31, file = "./generated_data/DA_H1_rr75_N600_H31.RData")

res_H1_rr75_H31_N600 <- analyze_DA_Effects_H3(DA_H1_rr75_N600_H31)
save(res_H1_rr75_H31_N600, file = "./generated_data/res_H1_rr75_H31_N600.RData")

BFtable_H3(res_H1_rr75_H31_N600)
postprobcat_H3(res_H1_rr75_H31_N600)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_rr75_N600_H31, 
                     trueIntercept=4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(0,2))
mtext("H1, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.60, N = 500 #######################

set.seed(1234)
DAT_H1_rr60_N500_H31 <- simANCOVA_H2(iter = 1000,
                                     N = 500, 
                                     meanControl = 4, 
                                     sdWithinGroup = 1.5, 
                                     effimpInt = 0,
                                     effmentCont = 0.5,
                                     effCombi = 0.5,
                                     retestReliability = 0.60)

DA_H1_rr60_N500_H31 <- DA_ANCOVA(simData = DAT_H1_rr60_N500_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_rr60_N500_H31, file = "./generated_data/DA_H1_rr60_N500_H31.RData")

res_H1_rr60_H31_N500 <- analyze_DA_Effects_H3(DA_H1_rr60_N500_H31)
save(res_H1_rr60_H31_N500, file = "./generated_data/res_H1_rr60_H31_N500.RData")

BFtable_H3(res_H1_rr60_H31_N500)
postprobcat_H3(res_H1_rr60_H31_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_rr60_N500_H31, 
                     trueIntercept=4, 
                     trueRR = 0.60,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(0,2))
mtext("H1, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

################## H1, retest reliability: 0.60, N = 600 #######################

set.seed(1234)
DAT_H1_rr60_N600_H31 <- simANCOVA_H2(iter = 1000,
                                     N = 600, 
                                     meanControl = 4, 
                                     sdWithinGroup = 1.5, 
                                     effimpInt = 0,
                                     effmentCont = 0.5,
                                     effCombi = 0.5,
                                     retestReliability = 0.60)

DA_H1_rr60_N600_H31 <- DA_ANCOVA(simData = DAT_H1_rr60_N600_H31,
                                 priorCoef = "normal(0, 2)", 
                                 priorIntercept = "normal(4, 1.5)", 
                                 priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_rr60_N600_H31, file = "./generated_data/DA_H1_rr60_N600_H31.RData")

res_H1_rr60_H31_N600 <- analyze_DA_Effects_H3(DA_H1_rr60_N600_H31)
save(res_H1_rr60_H31_N600, file = "./generated_data/res_H1_rr60_H31_N600.RData")

BFtable_H3(res_H1_rr60_H31_N600)
postprobcat_H3(res_H1_rr60_H31_N600)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_rr60_N600_H31, 
                     trueIntercept=4, 
                     trueRR = 0.60,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5, 
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(0,2))
mtext("H1, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)
