# ========================== LOAD FUNCTIONS ====================================

library(brms)
source("sampleSizePlanning_functions.R")

# =========================== 250 PARTICIPANTS =================================

sampSize <- 250

#### H0 ####
simD250_H0 <- simANCOVA(iter = 1000, 
                     N = sampSize, 
                     meanControl = 13.4, 
                     sd_measurement = sqrt(20), 
                     sd_truescore = sqrt(32), 
                     effSingle = 0, 
                     effCombi = 0, 
                     betaPhysActPre = 1)
DA250_resultH0 <- DA_ANCOVA(simD250_H0)

save(DA250_resultH0, file = "DA250_resultH0.RData")

pdf("simD250_H0_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA250_resultH0)
analyze_DA_Effects(DA250_resultH0)
analyze_DA_noDiffSingle(DA250_resultH0)
analyze_DA_combi(DA250_resultH0)
dev.off()

#### H1 ####

simD250_H1 <- simANCOVA(iter = 1000, 
                        N = sampSize, 
                        meanControl = 13.4, 
                        sd_measurement = sqrt(20), 
                        sd_truescore = sqrt(32), 
                        effSingle = 2.5, 
                        effCombi = 5, 
                        betaPhysActPre = 1)
DA250_resultH1 <- DA_ANCOVA(simD250_H1)

save(DA250_resultH1, file = "DA250_resultH1.RData")

pdf("simD250_H1_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA250_resultH1)
analyze_DA_Effects(DA250_resultH1)
analyze_DA_noDiffSingle(DA250_resultH1)
analyze_DA_combi(DA250_resultH1)
dev.off()

# =========================== 200 PARTICIPANTS =================================

sampSize <- 200

#### H0 ####
simD200_H0 <- simANCOVA(iter = 1000, 
                        N = sampSize, 
                        meanControl = 13.4, 
                        sd_measurement = sqrt(20), 
                        sd_truescore = sqrt(32), 
                        effSingle = 0, 
                        effCombi = 0, 
                        betaPhysActPre = 1)
DA200_resultH0 <- DA_ANCOVA(simD200_H0)

save(DA200_resultH0, file = "DA200_resultH0.RData")

pdf("simD200_H0_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA200_resultH0)
analyze_DA_Effects(DA200_resultH0)
analyze_DA_noDiffSingle(DA200_resultH0)
analyze_DA_combi(DA200_resultH0)
dev.off()

#### H1 ####

simD200_H1 <- simANCOVA(iter = 1000, 
                        N = sampSize, 
                        meanControl = 13.4, 
                        sd_measurement = sqrt(20), 
                        sd_truescore = sqrt(32), 
                        effSingle = 2.5, 
                        effCombi = 5, 
                        betaPhysActPre = 1)
DA200_resultH1 <- DA_ANCOVA(simD200_H1)

save(DA200_resultH1, file = "DA200_resultH1.RData")

pdf("simD200_H1_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA200_resultH1)
analyze_DA_Effects(DA200_resultH1)
analyze_DA_noDiffSingle(DA200_resultH1)
analyze_DA_combi(DA200_resultH1)
dev.off()

# =========================== 220 PARTICIPANTS =================================

sampSize <- 220

#### H0 ####
simD220_H0 <- simANCOVA(iter = 1000, 
                        N = sampSize, 
                        meanControl = 13.4, 
                        sd_measurement = sqrt(20), 
                        sd_truescore = sqrt(32), 
                        effSingle = 0, 
                        effCombi = 0, 
                        betaPhysActPre = 1)
DA220_resultH0 <- DA_ANCOVA(simD220_H0)

save(DA220_resultH0, file = "DA220_resultH0.RData")

pdf("simD220_H0_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA220_resultH0)
analyze_DA_Effects(DA220_resultH0)
analyze_DA_noDiffSingle(DA220_resultH0)
analyze_DA_combi(DA220_resultH0)
dev.off()

#### H1 ####

simD220_H1 <- simANCOVA(iter = 1000, 
                        N = sampSize, 
                        meanControl = 13.4, 
                        sd_measurement = sqrt(20), 
                        sd_truescore = sqrt(32), 
                        effSingle = 2.5, 
                        effCombi = 5, 
                        betaPhysActPre = 1)
DA220_resultH1 <- DA_ANCOVA(simD220_H1)

save(DA220_resultH1, file = "DA220_resultH1.RData")

pdf("simD220_H1_results.pdf", width = 10.39, height = 6.41)
analyze_DA_postMeans(DA220_resultH1)
analyze_DA_Effects(DA220_resultH1)
analyze_DA_noDiffSingle(DA220_resultH1)
analyze_DA_combi(DA220_resultH1)
dev.off()
